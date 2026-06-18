//! faSTM — extendr binding to topica's structural topic model fitter.
//!
//! The entire binding surface is ONE function: `fit_stm`. It wraps
//! `topica::ctm::fit_ctm` (STM = CTM + prevalence + content) and returns the
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
use topica::ctm::{fit_ctm, GammaPrior};

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
#[allow(clippy::too_many_arguments)]
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
    gamma_l1_alpha: Nullable<f64>,
    diagonal: bool,
    seed: i32,
    inference: String,
    _batch_size: i32,
    _tau: f64,
    _kappa: f64,
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

    // Mirror topica's STM rng (parity TODO: confirm vs src/python.rs).
    let mut rng = ChaCha8Rng::seed_from_u64(seed as u64);

    let model = match inference.as_str() {
        "batch" => fit_ctm(
            &docs,
            num_topics as usize,
            num_types as usize,
            em_iters as usize,
            em_tol,
            sigma_shrink,
            prevalence_ref,
            content_ref,
            init_spectral,
            gamma_prior,
            /* keep_nu = */ true, // need ν for the method-of-composition posterior
            diagonal,
            &mut rng,
        ),
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
        bound = model.bound,
        bound_history = model.bound_history,
        converged = model.converged
    )
}

extendr_module! {
    mod faSTM;
    fn fit_stm;
}
