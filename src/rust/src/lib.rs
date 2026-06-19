//! faSTM — extendr binding to topica's structural topic model fitter.
//!
//! The entire binding surface is ONE function: `fit_stm`. It wraps
//! `topica_core::ctm::fit_ctm` (STM = CTM + prevalence + content) and returns the
//! fitted `CtmModel` arrays as a flat R list. All stm-object assembly, the
//! per-document posterior draws, and `estimateEffect` are pure R (see `R/`).
//!
//! Why no posterior/effects binding: the per-document Laplace posterior is fully
//! captured by `lambda` (variational mean η) + `nu` (variational covariance) in
//! the returned list, so drawing θ and the method-of-composition regression are
//! done in R with `MASS::mvrnorm` — nothing to bind.

use extendr_api::prelude::*;
use rand::SeedableRng;
use rand_chacha::ChaCha8Rng;
use topica_core::ctm::{fit_ctm, infer_theta, GammaPrior};

/// Run `f` on a scoped rayon pool of `n` workers (mirrors topica's
/// `run_with_threads`). `n < 1` uses the global pool (all cores). The parallel
/// E-step (`variational::laplace`) is deterministic regardless of worker count.
fn run_with_threads<T: Send, F: FnOnce() -> T + Send>(n: i32, f: F) -> T {
    if n >= 1 {
        match rayon::ThreadPoolBuilder::new().num_threads(n as usize).build() {
            Ok(pool) => pool.install(f),
            Err(_) => f(),
        }
    } else {
        f()
    }
}

/// Fit a structural topic model and return its raw arrays.
///
/// Inputs are pre-converted in `R/stm.R`:
/// - `docs_flat` / `doc_lens`: documents as one concatenated 0-based token-id
///   stream plus per-document lengths (counts already expanded). Reassembled
///   here into `Vec<Vec<u32>>`, the shape `fit_ctm` wants.
/// - `prevalence`: row-major D×P design matrix flattened (NULL if none).
/// - `content_groups`: per-doc 0-based group id (NULL if none); `num_groups`.
///
/// `inference`: "batch" -> `fit_ctm` (parity-validated). "svi" -> `fit_ctm_svi`
/// once topica #231 PR B (STM-SVI) is in the pinned revision; the R layer gates
/// the prevalence/content + svi combination until then.
#[extendr]
#[allow(clippy::too_many_arguments, unused_variables)]
fn fit_stm(
    docs_flat: Vec<i32>,
    doc_lens: Vec<i32>,
    num_types: i32,
    num_topics: i32,
    em_iters: i32,
    em_tol: f64,
    sigma_shrink: f64,
    prevalence: Nullable<Vec<f64>>,
    num_features: i32,
    content_groups: Nullable<Vec<i32>>,
    num_groups: i32,
    init_spectral: bool,
    init_beta: Nullable<Vec<f64>>,
    gamma_l1_alpha: Nullable<f64>,
    diagonal: bool,
    seed: i32,
    inference: String,
    batch_size: i32,
    tau: f64,
    kappa: f64,
    num_threads: i32,
) -> List {
    // --- reassemble documents: flat token stream -> Vec<Vec<u32>> ---------
    let mut docs: Vec<Vec<u32>> = Vec::with_capacity(doc_lens.len());
    let mut cur = 0usize;
    for &len in &doc_lens {
        let len = len as usize;
        docs.push(docs_flat[cur..cur + len].iter().map(|&t| t as u32).collect());
        cur += len;
    }

    // --- prevalence: flat D*P -> Vec<Vec<f64>> (D rows of P) ----------------
    let prevalence_owned: Option<Vec<Vec<f64>>> = match prevalence {
        Nullable::NotNull(flat) => {
            let p = num_features as usize;
            Some(flat.chunks(p).map(|r| r.to_vec()).collect())
        }
        Nullable::Null => None,
    };
    let prevalence_ref: Option<&[Vec<f64>]> = prevalence_owned.as_deref();

    // --- content: per-doc group id ----------------------------------------
    let groups_owned: Option<Vec<usize>> = match content_groups {
        Nullable::NotNull(g) => Some(g.iter().map(|&x| x as usize).collect()),
        Nullable::Null => None,
    };
    let content_ref: Option<(&[usize], usize)> =
        groups_owned.as_deref().map(|g| (g, num_groups as usize));

    let gamma_prior = match gamma_l1_alpha {
        Nullable::NotNull(a) => GammaPrior::L1 { alpha: a },
        Nullable::Null => GammaPrior::Pooled,
    };

    // --- optional externally-supplied base beta (K*V row-major) for a
    //     "replicate the original" fit started from a given init (topica #234/#235)
    let init_beta_owned: Option<Vec<Vec<f64>>> = match init_beta {
        Nullable::NotNull(flat) => {
            let v = num_types as usize;
            Some(flat.chunks(v).map(|r| r.to_vec()).collect())
        }
        Nullable::Null => None,
    };
    let init_beta_ref: Option<&[Vec<f64>]> = init_beta_owned.as_deref();

    // Mirror topica's STM rng (parity TODO: confirm vs src/python.rs).
    let mut rng = ChaCha8Rng::seed_from_u64(seed as u64);

    let model = match inference.as_str() {
        "batch" => run_with_threads(num_threads, || {
            fit_ctm(
                &docs,
                num_topics as usize,
                num_types as usize,
                em_iters as usize,
                em_tol,
                sigma_shrink,
                prevalence_ref,
                content_ref,
                init_spectral,
                init_beta_ref,
                gamma_prior,
                /* keep_nu = */ true, // need ν for the method-of-composition posterior
                diagonal,
                &mut rng,
            )
        }),
        "svi" => {
            // TODO(topica#231 PR B): route to ctm::fit_ctm_svi once it accepts
            // prevalence + content. The R layer already gates svi+covariates and
            // passes _batch_size/_tau/_kappa, so wiring is a one-call swap here.
            panic!(
                "inference=\"svi\" is not wired yet in this build; pin a topica \
                 revision containing STM-SVI (#231 PR B) and route to fit_ctm_svi"
            )
        }
        other => panic!("unknown inference mode: {other:?} (expected \"batch\" or \"svi\")"),
    };

    // --- pack CtmModel -> R list (assembled into an stm object in R) -------
    let k = model.num_topics;
    let v = model.num_types;

    let beta_flat: Vec<f64> = model.beta.iter().flatten().copied().collect();
    let lambda_flat: Vec<f64> = model.lambda.iter().flatten().copied().collect();
    let nu_flat: Vec<f64> = model.nu.iter().flatten().copied().collect();

    let gamma_flat: Nullable<Vec<f64>> = match &model.gamma {
        Some(g) => Nullable::NotNull(g.iter().flatten().copied().collect()),
        None => Nullable::Null,
    };
    let content_beta_flat: Nullable<Vec<f64>> = match &model.content_beta {
        // G×K×V flattened group-major; R reshapes with num_groups.
        Some(cb) => Nullable::NotNull(cb.iter().flatten().flatten().copied().collect()),
        None => Nullable::Null,
    };

    // SAGE κ decomposition (topica #237 / v0.24.1): background m (V), topic κ
    // (K×V), covariate κ (G×V), interaction κ (K·G×V, topic*G+group). Flattened
    // row-major; the R layer reshapes and builds stm's beta$kappa structure.
    let (ck_m, ck_topic, ck_cov, ck_inter): (
        Nullable<Vec<f64>>, Nullable<Vec<f64>>, Nullable<Vec<f64>>, Nullable<Vec<f64>>,
    ) = match &model.content_kappa {
        Some(ck) => (
            Nullable::NotNull(ck.m.clone()),
            Nullable::NotNull(ck.kappa_topic.iter().flatten().copied().collect()),
            Nullable::NotNull(ck.kappa_cov.iter().flatten().copied().collect()),
            Nullable::NotNull(ck.kappa_interaction.iter().flatten().copied().collect()),
        ),
        None => (Nullable::Null, Nullable::Null, Nullable::Null, Nullable::Null),
    };

    list!(
        num_topics = k as i32,
        num_types = v as i32,
        num_docs = model.lambda.len() as i32,
        num_groups = model.num_groups as i32,
        beta = beta_flat,          // K*V row-major (prob; R takes log)
        lambda = lambda_flat,      // D*(K-1) row-major  -> stm `eta`
        nu = nu_flat,              // D*(K-1)^2 row-major -> per-doc posterior cov
        mu = model.mu,             // (K-1)
        sigma = model.sigma,       // (K-1)^2 row-major
        gamma = gamma_flat,        // num_features*(K-1) row-major, or NULL
        content_beta = content_beta_flat, // G*K*V or NULL
        kappa_m = ck_m,                    // V                (or NULL)
        kappa_topic = ck_topic,            // K*V row-major    (or NULL)
        kappa_cov = ck_cov,                // G*V row-major    (or NULL)
        kappa_interaction = ck_inter,      // (K*G)*V row-major (or NULL)
        bound = model.bound,
        bound_history = model.bound_history,
        converged = model.converged
    )
}

/// Out-of-sample topic inference: for each new document, run the variational
/// E-step against fixed globals (β, μ, Σ⁻¹) and return θ. Documents are passed
/// sparse — `words` are 0-based ids into the *fitted model's* vocabulary
/// (out-of-vocabulary terms dropped by the R caller) with their `counts`,
/// concatenated, plus per-document term counts `doc_nterms`.
#[extendr]
fn infer_theta_new(
    beta_flat: Vec<f64>,   // K*V row-major (probabilities)
    num_topics: i32,
    num_types: i32,
    mu: Vec<f64>,          // K-1
    siginv: Vec<f64>,      // (K-1)^2 row-major
    words: Vec<i32>,       // concatenated 0-based vocab ids
    counts: Vec<f64>,
    doc_nterms: Vec<i32>,
) -> Vec<f64> {
    let k = num_topics as usize;
    let v = num_types as usize;
    let beta: Vec<Vec<f64>> = (0..k).map(|t| beta_flat[t * v..(t + 1) * v].to_vec()).collect();

    let mut out = Vec::with_capacity(doc_nterms.len() * k);
    let mut cur = 0usize;
    for &nt in &doc_nterms {
        let nt = nt as usize;
        let w: Vec<usize> = words[cur..cur + nt].iter().map(|&x| x as usize).collect();
        let c: Vec<f64> = counts[cur..cur + nt].to_vec();
        out.extend(infer_theta(&beta, &mu, &siginv, &w, &c));
        cur += nt;
    }
    out
}

/// LDA topic-word matrix via topica's CVB0 (deterministic collapsed variational
/// Bayes), to seed a "replicate stm's LDA init" STM fit. Mirrors stm's
/// collapsed-Gibbs LDA initialization; the result is fed back as `init_beta`.
/// Returns K*V row-major topic-word probabilities.
#[extendr]
fn lda_init_beta(
    docs_flat: Vec<i32>,
    doc_lens: Vec<i32>,
    num_types: i32,
    num_topics: i32,
    iters: i32,
    alpha: f64,
    beta: f64,
    seed: i32,
) -> Vec<f64> {
    let mut docs: Vec<Vec<u32>> = Vec::with_capacity(doc_lens.len());
    let mut cur = 0usize;
    for &len in &doc_lens {
        let len = len as usize;
        docs.push(docs_flat[cur..cur + len].iter().map(|&t| t as u32).collect());
        cur += len;
    }
    let v = num_types as usize;
    let k = num_topics as usize;
    let corpus = topica_core::corpus::Corpus {
        id_to_word: (0..v).map(|i| i.to_string()).collect(),
        doc_names: vec![String::new(); docs.len()],
        doc_labels: Vec::new(),
        doc_freqs: vec![0u32; v],
        total_freqs: vec![0u32; v],
        docs,
    };
    let mut rng = ChaCha8Rng::seed_from_u64(seed as u64);
    let alpha_vec = vec![alpha; k];
    let mut lda = topica_core::cvb0::Cvb0::new(&corpus, k, &alpha_vec, beta, &mut rng);
    for _ in 0..(iters as usize) {
        lda.sweep();
    }
    lda.topic_word().into_iter().flatten().collect() // K*V row-major
}

extendr_module! {
    mod faSTM;
    fn fit_stm;
    fn infer_theta_new;
    fn lda_init_beta;
}
