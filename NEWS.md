# faSTM 0.0.0.9000 (development)

Initial scaffold.

* `stm()` — fast STM fit via topica's Rust `fit_ctm`, returning an
  `c("faSTM", "STM")` object compatible with the `stm` package's post-fit
  functions. Prevalence + content (SAGE) covariates supported.
* `estimateEffect()` — honest covariate effects via the method of composition
  (per-document posterior draws pooled by Rubin's rules).
* `posterior_theta_samples()` — per-document topic-proportion posterior draws
  (pure R from the fit's `eta` + `nu`).
* SVI kwargs (`inference`, `batch_size`, `tau`, `kappa`) plumbed through ahead of
  topica's STM-SVI landing (topica #231); gated for prevalence/content until a
  topica build with STM-SVI is pinned.
