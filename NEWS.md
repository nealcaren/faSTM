# faSTM 0.0.0.9000 (development)

A fast, modern Structural Topic Model for R: an `stm`-compatible fitting backend
on a multithreaded Rust core (`topica`), plus a richer effects and tidy-workflow
toolkit on top.

## Fitting

* `stm()` — fast STM fit via `topica`'s Rust `fit_ctm`, returning a
  `c("faSTM", "STM")` object compatible with the `stm` package's post-fit
  functions (`labelTopics`, `plot`, `findThoughts`, `sageLabels`, `toLDAvis`).
  Prevalence and content (SAGE) covariates, spectral / LDA / random / custom
  initialization, and a `num_threads` knob. ~6x faster than `stm` on the poliblog
  example (and ~13x on `search_k`), wall-clock to convergence.
* **Multiple content covariates** — `content = ~ a + b` crosses covariates into a
  saturated SAGE content model (one topic-word distribution per level combination),
  beyond `stm`'s single content variable.
* SVI kwargs (`inference`, `batch_size`, `tau`, `kappa`) plumbed through ahead of
  `topica`'s STM-SVI landing (topica #231); gated for prevalence/content for now.

## Covariate effects (`estimateEffect()`)

* Honest effects via the method of composition (per-document posterior draws
  pooled by Rubin's rules), drawing from each document's own Laplace covariance.
* **Survey weights** (weighted least squares) and **cluster-robust standard
  errors** (sandwich vcov with `stm`'s finite-sample correction).
* **Random effects** in prevalence — `lme4`-style `(term | group)` terms fit per
  draw and pooled.
* `ame()` — average marginal effects; `effect_estimates()` — tidy effect tables;
  `combine` for aggregate-topic effects.

## Inspection & topic quality

* `coherence()` with **Mimno, NPMI, and C_V** measures; `semantic_coherence()`,
  `exclusivity()`, `frex_scores()`, `topic_correlation()`, `topic_corr_graph()`.
* `label_topics()`, `sage_labels()`, `find_thoughts()`, `topic_terms()`,
  `topic_proportions()`, `content_topics()`.

## Model selection

* `search_k()` — parallelizes across K (bstewart/stm#262) and selects by held-out
  likelihood or any coherence measure; `select_model()`, `many_topics()`,
  `make_heldout()` / `eval_heldout()`.

## Tidyverse & out-of-sample

* `broom` generics: `tidy()` (beta / frex / gamma), `glance()`, `augment()`;
  `predict()` for new documents.
* `as_corpus()` reads prepared text from `quanteda` / `tidytext`; `from_tidy()`.

## Data

* `poliblog` — the CMU 2008 political-blog corpus (the `stm` vignette example).
* `congress` — 1,679 balanced U.S. House/Senate floor speeches, Congresses 100–111
  (party × chamber × time), for the multiple-content-covariate and over-time demos.

## Vignettes

* *faSTM: the stm vignette, run on faSTM* — the `stm` workflow, fit live.
* *Beyond stm: faSTM's extensions* — the tools above, on the `congress` corpus.
