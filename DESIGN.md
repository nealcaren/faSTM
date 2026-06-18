# faSTM — design

A fast fitting backend for structural topic models that is **drop-in compatible
with the R `stm` package**. faSTM does *not* fork stm and does *not* reimplement
its ecosystem. It swaps in a Rust fitter (from
[`topica`](https://github.com/nealcaren/topica)) and returns an object that
stm's own post-fit functions read unmodified.

## The one architectural fact everything rests on

stm's post-fit functions — `labelTopics()`, `plot.STM()`, `findThoughts()`,
`sageLabels()`, `toLDAvis()`, and `estimateEffect()` — **do not re-run the
estimator**. They are pure readers of the fitted `STM` S3 object. So if faSTM's
`stm()` returns an object structurally identical to stm's, the entire downstream
ecosystem works for free, with zero forking and zero reimplementation.

Therefore faSTM = **(1) a Rust fit + (2) an stm-shaped object constructor + (3)
a pure-R `estimateEffect`**. Text prep is `Imports: stm` (use `textProcessor()`
/ `prepDocuments()` as-is). Plotting/labeling/LDAvis dispatch into stm's own
methods.

## Why estimation-only, not a fork

The user's question was: minimal package (use stm for everything, faSTM only for
estimation) vs. full fork of stm with the backend swapped. **Estimation-only
wins**, because:

- stm's readers are pure functions of the object → reproduce the object shape and
  you inherit the whole ecosystem, including *future* stm improvements.
- stm's estimator is not one clean function: it's an R-driven EM loop with a C++
  `estep` and SAGE updates interleaved in R. Transplanting the Rust core into the
  middle of that loop is brittle. Returning a finished, compatible object is not.
- A fork means owning stm's entire codebase and tracking upstream forever.

The only thing faSTM reimplements is `estimateEffect`, because that is where
topica is *better* than stm (honest per-document posterior propagation), so we
want our own anyway.

## Binding surface: one Rust function

topica's STM is `topica::ctm::fit_ctm` (STM = CTM + prevalence + content), pure
Rust, public in the `rlib`:

```rust
pub fn fit_ctm<R: Rng>(
    docs: &[Vec<u32>], num_topics, num_types, em_iters, em_tol, sigma_shrink,
    prevalence: Option<&[Vec<f64>]>,
    content:    Option<(&[usize], usize)>,   // (per-doc group id, num_groups)
    init_spectral: bool, gamma_prior: GammaPrior, keep_nu: bool, diagonal: bool,
    rng: &mut R,
) -> CtmModel
```

`CtmModel` already carries everything the stm object needs:

| `CtmModel` field      | stm `STM` slot              | note                                  |
|-----------------------|-----------------------------|---------------------------------------|
| `beta` (K×V, prob)    | `beta$logbeta` (list)       | take `log()`; list, one per content group |
| `content_beta` (G×K×V)| `beta$logbeta` (G entries)  | SAGE content model                    |
| `lambda` (D×(K-1), η) | `eta` (D×(K-1))             | per-doc variational mean              |
| `nu` (D×(K-1)²)       | (used by thetaPosterior)    | per-doc Laplace covariance            |
| softmax(`lambda`,0)   | `theta` (D×K)               | append reference 0, softmax           |
| `mu`                  | `mu$mu`                     | (K-1)                                 |
| `sigma`               | `sigma` ((K-1)²)            |                                       |
| `gamma`               | `mu$gamma`                  | prevalence coefs, `Some` if prevalence|
| `bound`,`bound_history`,`converged` | `convergence`  |                                       |
| `vocab` (passed in)   | `vocab`                     |                                       |
| `settings`            | `settings`                  | reconstruct; see `as_stm_object()`    |

**`posterior_theta_samples` does NOT need a Rust binding.** The per-doc Laplace
posterior is fully described by `lambda` + `nu` in the returned object, so the
draw (η ~ N(λ_d, ν_d) → softmax → θ) is pure R (`MASS::mvrnorm`). That is exactly
what `R/posterior.R` does, and what `estimateEffect()` consumes.

So the extendr crate exposes a single `fit_stm(...)` that returns the `CtmModel`
arrays as an R list; all object assembly and effects are R.

## Input conversion

stm `documents` is a named list of `2 × n_d` integer matrices (row 1 = 1-based
word id, row 2 = count). topica wants `docs: Vec<Vec<u32>>`, each doc a sequence
of 0-based token-type ids with counts expanded. Conversion (in `R/stm.R` before
the `.Call`): repeat `(id-1)` `count` times. `vocab` length = `num_types`.

## SVI / large corpora (topica #231)

topica is gaining an opt-in `inference="svi"` stochastic-VI path across the
logistic-normal family. Base CTM SVI exists today (`fit_ctm_svi`); **STM-SVI
(prevalence + content) lands in #231 PR B**, generalizing `fit_ctm_svi`.

faSTM plumbs the SVI kwargs through **from day one** so no API change is needed
when PR B lands:

```r
stm(documents, vocab, K, prevalence = ~x, data = meta,
    inference = c("batch", "svi"),   # default "batch"
    batch_size = 256, tau = 64, kappa = 0.7)
```

- `inference = "batch"` routes to `fit_ctm` (today; parity-validated).
- `inference = "svi"` routes to `fit_ctm_svi`. Until PR B is in the pinned
  topica, `svi` + (prevalence|content) errors with a clear "requires topica >=
  <ver>" message; base SVI works now. `iters` is interpreted as epochs in SVI
  mode (topica's convention).

The released faSTM pins the topica revision that contains STM-SVI, so the scaling
path the user needs for large corpora is in the box.

## Determinism

Batch fit is bit-reproducible from `(seed, vocab, documents)` and is the path
that carries topica's parity validation against R stm. SVI is *seed-reproducible*
(deterministic minibatch shuffle from the model rng), not bit-exact across
thread counts — documented, not hidden. faSTM seeds `ChaCha8Rng::seed_from_u64`
to mirror topica exactly (parity TODO: confirm against `src/python.rs` STM rng
construction).

## Dependency wiring

faSTM's `src/rust/Cargo.toml` depends on `topica` as a **git dependency pinned to
a tag**, `default-features = false` (the NumPy-free core; no `python`, no
embeddings). Local dev uses a `[patch]`/path override to
`~/Documents/GitHub/topica`. For an eventual CRAN release, vendor with
`cargo vendor` into `src/rust/vendor` (rextendr supports this).

The rand stack must match topica's lockfile (`rand 0.8`, `rand_chacha 0.3`) so
the `Rng` trait bound and seed stream are identical — pulling topica via git
brings its `Cargo.lock`, so pin to the same versions in faSTM.

## Build checklist before claiming "parity with stm"

1. Map exactly which slots each stm reader touches; fill those (half-day reading
   stm source). The table above is the starting point — verify `plot.STM`,
   `toLDAvis`, `sageLabels`, `findThoughts` against a real fit.
2. Match init defaults: stm defaults to **spectral** (`init_spectral = TRUE`);
   confirm faSTM's default and seed reproducibility.
3. Confirm the `content = ~` / SAGE path reaches `content_beta` and that
   `sageLabels()` reads it correctly.
4. Recovery test: faSTM fit vs `stm::stm()` on `gadarian` — topic correlation,
   `estimateEffect` coefficients, and held-out behavior within tolerance.
5. SVI recovery: `inference="svi"` recovers `inference="batch"` topics within
   tolerance (mirrors topica's #231 recovery gate).

## Layout

```
DESCRIPTION            Imports: stm, MASS, stats
NAMESPACE              export(stm, estimateEffect, faSTM_fit, posterior_theta_samples)
R/stm.R                stm() wrapper: convert input -> .Call(fit) -> as_stm_object()
R/as-stm-object.R      CtmModel arrays -> stm-compatible STM S3 object
R/posterior.R          posterior_theta_samples(): eta~N(lambda,nu) -> softmax
R/estimate-effect.R    estimateEffect(): method of composition + Rubin pooling
R/extendr-wrappers.R   generated by rextendr::document() (.Call shims)
src/rust/Cargo.toml    extendr-api + topica (git, default-features=false)
src/rust/src/lib.rs    single fit_stm() extendr entry point
tests/testthat/        recovery vs stm; SVI vs batch
```

`R/extendr-wrappers.R`, `src/Makevars*`, `src/entrypoint.c`, and `NAMESPACE`
useDynLib are produced by `rextendr::document()` — run it after editing
`src/rust/src/lib.rs`. (rextendr is a dev-time dependency; not yet installed in
this environment — `install.packages("rextendr")`.)
