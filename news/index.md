# Changelog

## faSTM 0.0.0.9000 (development)

A fast, modern Structural Topic Model for R: an `stm`-compatible fitting
backend on a multithreaded Rust core (`topica`), plus a richer effects
and tidy-workflow toolkit on top.

### Fitting

- [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md) — fast
  STM fit via `topica`’s Rust `fit_ctm`, returning a `c("faSTM", "STM")`
  object compatible with the `stm` package’s post-fit functions
  (`labelTopics`, `plot`, `findThoughts`, `sageLabels`, `toLDAvis`).
  Prevalence and content (SAGE) covariates, spectral / LDA / random /
  custom initialization, and a `num_threads` knob. ~6x faster than `stm`
  on the poliblog example (and ~13x on `search_k`), wall-clock to
  convergence.
- **Multiple content covariates** — `content = ~ a + b` crosses
  covariates into a saturated SAGE content model (one topic-word
  distribution per level combination), beyond `stm`’s single content
  variable.
- SVI kwargs (`inference`, `batch_size`, `tau`, `kappa`) plumbed through
  ahead of `topica`’s STM-SVI landing (topica
  [\#231](https://github.com/nealcaren/faSTM/issues/231)); gated for
  prevalence/content for now.

### Covariate effects (`estimateEffect()`)

- Honest effects via the method of composition (per-document posterior
  draws pooled by Rubin’s rules), drawing from each document’s own
  Laplace covariance.
- **Survey weights** (weighted least squares) and **cluster-robust
  standard errors** (sandwich vcov with `stm`’s finite-sample
  correction).
- **Random effects** in prevalence — `lme4`-style `(term | group)` terms
  fit per draw and pooled.
- [`ame()`](https://nealcaren.github.io/faSTM/reference/ame.md) —
  average marginal effects;
  [`effect_estimates()`](https://nealcaren.github.io/faSTM/reference/effect_estimates.md)
  — tidy effect tables; `combine` for aggregate-topic effects.

### Inspection & topic quality

- [`coherence()`](https://nealcaren.github.io/faSTM/reference/coherence.md)
  with **Mimno, NPMI, and C_V** measures;
  [`semantic_coherence()`](https://nealcaren.github.io/faSTM/reference/semantic_coherence.md),
  [`exclusivity()`](https://nealcaren.github.io/faSTM/reference/exclusivity.md),
  [`frex_scores()`](https://nealcaren.github.io/faSTM/reference/frex_scores.md),
  [`topic_correlation()`](https://nealcaren.github.io/faSTM/reference/topic_correlation.md),
  [`topic_corr_graph()`](https://nealcaren.github.io/faSTM/reference/topic_corr_graph.md).
- [`label_topics()`](https://nealcaren.github.io/faSTM/reference/label_topics.md),
  [`sage_labels()`](https://nealcaren.github.io/faSTM/reference/sage_labels.md),
  [`find_thoughts()`](https://nealcaren.github.io/faSTM/reference/find_thoughts.md),
  [`topic_terms()`](https://nealcaren.github.io/faSTM/reference/topic_terms.md),
  [`topic_proportions()`](https://nealcaren.github.io/faSTM/reference/topic_proportions.md),
  [`content_topics()`](https://nealcaren.github.io/faSTM/reference/content_topics.md).

### Model selection

- [`search_k()`](https://nealcaren.github.io/faSTM/reference/search_k.md)
  — parallelizes across K (bstewart/stm#262) and selects by held-out
  likelihood or any coherence measure;
  [`select_model()`](https://nealcaren.github.io/faSTM/reference/select_model.md),
  [`many_topics()`](https://nealcaren.github.io/faSTM/reference/many_topics.md),
  [`make_heldout()`](https://nealcaren.github.io/faSTM/reference/make_heldout.md)
  /
  [`eval_heldout()`](https://nealcaren.github.io/faSTM/reference/eval_heldout.md).

### Tidyverse & out-of-sample

- `broom` generics:
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) (beta /
  frex / gamma),
  [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html);
  [`predict()`](https://rdrr.io/r/stats/predict.html) for new documents.
- [`as_corpus()`](https://nealcaren.github.io/faSTM/reference/as_corpus.md)
  reads prepared text from `quanteda` / `tidytext`;
  [`from_tidy()`](https://nealcaren.github.io/faSTM/reference/from_tidy.md).

### Data

- `poliblog` — the CMU 2008 political-blog corpus (the `stm` vignette
  example).
- `congress` — 1,679 balanced U.S. House/Senate floor speeches,
  Congresses 100–111 (party × chamber × time), for the
  multiple-content-covariate and over-time demos.

### Vignettes

- *faSTM: the stm vignette, run on faSTM* — the `stm` workflow, fit
  live.
- *Beyond stm: faSTM’s extensions* — the tools above, on the `congress`
  corpus.
