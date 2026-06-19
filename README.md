# faSTM

<!-- badges: start -->
[![R-CMD-check](https://github.com/nealcaren/faSTM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nealcaren/faSTM/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/nealcaren/faSTM/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/nealcaren/faSTM/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

📖 **Documentation:** <https://nealcaren.github.io/faSTM/>

A fast, **`stm`-compatible reimplementation** of the **Structural Topic Model**
for R. faSTM reimplements [`stm`](https://github.com/bstewart/stm)'s full
framework (prevalence and content covariates, FREX/lift/score labels, semantic
coherence, representative documents, covariate-effect estimation, model
selection, and out-of-sample inference) on a self-contained, multithreaded Rust
core, and adds an estimation and tidy-workflow toolkit on top.

faSTM keeps a familiar, `stm`-compatible API, so most `stm` analysis code runs
with minimal edits and the fitted object is structurally compatible. What it adds
on top of `stm`'s framework: an `estimateEffect` with survey weights,
cluster-robust SEs, random effects, and average marginal effects; multiple
content covariates; NPMI/c_v coherence; broom tidiers and a `predict()` method.

**Replicating a specific fit.** STM's objective is non-convex and faSTM uses its
own optimizer, so an independent fit settles into its own valid, deterministic
optimum: different topic numbering, not a relabeling of a given `stm` run.
faSTM's spectral initialization reproduces `stm`'s Arora anchor-recovery step
exactly, so when you want a guaranteed match you can seed from `stm`'s own
spectral β via `stm(..., init.beta = )`. All four `init.type`s are supported:
`"Spectral"`, `"Random"`, `"LDA"` (seeded from a CVB0 LDA), and `"Custom"`.

```r
library(quanteda)   # tokenization (faSTM reads quanteda/tidytext objects directly)
library(faSTM)

dfmat <- data_corpus_inaugural |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("en")) |>
  tokens_wordstem() |>
  dfm() |>
  dfm_trim(min_termfreq = 5)

corpus <- as_corpus(dfmat)                     # docvars become metadata
fit    <- stm(corpus, K = 10, prevalence = ~ Party)

label_topics(fit)                              # prob / FREX / lift / score
semantic_coherence(fit); exclusivity(fit)
eff <- estimateEffect(1:10 ~ Party, fit, metadata = corpus$meta)
summary(eff)
```

## Highlights

- **Engine.** A multithreaded Rust variational EM (`num_threads` knob). On the
  poliblog example (5000 docs, K = 20, prevalence `~ rating + s(day)`) faSTM is
  **~6× faster** than `stm` (4s vs 23s), and **~13× on `search_k`** (it
  parallelizes across `K`). Both numbers are wall-clock to convergence. faSTM
  reaches the same fit in more iterations, but each one is far cheaper.
- **Scale.** An opt-in `inference = "svi"` (stochastic variational) path for
  corpora too large for batch EM. *(Covariate-model SVI lands with topica
  [#231](https://github.com/nealcaren/topica/issues/231).)*
- **Uncertainty-aware effects.** `estimateEffect()` uses the method of
  composition, propagating per-document posterior uncertainty, and supports
  survey `weights`, cluster-robust SEs, random effects (`(1 | group)` via
  `lme4`), average marginal effects (`ame()`), and `combine`d topics.
- **Inspection.** `label_topics()` (prob/FREX/lift/score), `topic_terms()`
  with numeric scores, `coherence()` (Mimno / NPMI / c_v), `exclusivity()`,
  `topic_proportions()`, topic correlations and SAGE content labels.
- **Multiple content covariates.** `content = ~ g + h` fits the fully crossed
  content model, and `content_topics()` recovers each covariate's marginal
  vocabulary.
- **Tidyverse-friendly.** `tidy()` / `glance()` / `augment()` (broom) and a
  `predict()` method, alongside a **ggplot2** plotting layer.
- **Self-contained.** Tokenize with `quanteda` / `tidytext` (which the field
  already uses) and faSTM reads their objects directly; no runtime dependency on
  Rust, `topica`, or Python once installed.

## Faithful where it counts

On a **shared fit** (same β/θ), faSTM's inspection numbers match `stm`'s:
FREX/prob/lift/score labels are **identical** to `stm::labelTopics`, and
`exclusivity()` / `semantic_coherence()` match to floating point. The fitted
object is `stm`-shaped, so `stm`'s own readers (`labelTopics()`, `plot.STM()`,
`sageLabels()`, `findThoughts()`, `estimateEffect()`) run on a faSTM fit, which
makes migrating existing analyses straightforward. This is parity *given the same
fit*, not a claim that the two produce the same fit (see above).

The [**Validation** article](https://nealcaren.github.io/faSTM/articles/validation.html)
checks both live against `stm`: it computes every inspection metric both ways on a
shared model (asserting equality with `stopifnot`), and shows that faSTM's own
fit, though a different topic decomposition, reaches held-out likelihood within a
fraction of a percent of `stm`'s.

## Status

Feature-complete and stabilizing toward a CRAN release. Working today:

- Corpus ingestion from quanteda / tidytext / document-term matrices.
- Fast fit with prevalence and (one or several, crossed) content covariates,
  threaded; all four `init.type`s including a real `"LDA"` initializer.
- Full inspection: labels, FREX/lift/score, coherence (Mimno/NPMI/c_v),
  exclusivity, topic correlations, SAGE content labels and marginals.
- `estimateEffect` with Global/Local/None uncertainty, survey weights,
  cluster-robust SEs, random effects, average marginal effects, combined topics,
  multiple-testing correction, and R²/F diagnostics.
- Out-of-sample inference (`fit_new_documents` / `predict` / `fitNewDocuments`
  with prior modes and posterior return).
- Model selection (`search_k`, `select_model`, `many_topics`) and a ggplot2
  plotting layer (topic summary, covariate effects with CIs, search-K
  diagnostics, topic-correlation network + igraph export).
- broom tidiers, `toLDAvis`, `topicQuality`, and stm-compatible exports.

On the roadmap: stochastic variational inference for covariate models
(topica [#231](https://github.com/nealcaren/topica/issues/231)).

## Install (beta)

faSTM has a Rust core, so installing from source needs a **Rust toolchain**
(`cargo` + `rustc`). This is a one-time setup:

```sh
# macOS / Linux: install Rust if you don't have it
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Then install in R with whichever tool you already have (they're equivalent;
`devtools::install_github` just calls `remotes`):

```r
# the familiar way
devtools::install_github("nealcaren/faSTM")

# or the lighter-weight package (no full dev toolkit needed just to install)
# install.packages("remotes")
remotes::install_github("nealcaren/faSTM")

# or the modern, fast installer
# install.packages("pak")
pak::pak("nealcaren/faSTM")
```

The first install compiles the Rust core (it fetches and builds the pinned,
dependency-light `topica-core` crate from GitHub, so it needs an internet
connection and takes a couple of minutes). After that, the package is **fully
self-contained**: it has no runtime dependency on Rust, `topica`, or Python.

Optional features pull in extra R packages only when used: `ggplot2` / `ggrepel`
(plots), `quanteda` / `tidytext` (text prep), `lme4` (random effects), `glmnet`
(`topic_lasso`), `igraph` (`topic_corr_graph`), `LDAvis` (`toLDAvis`).

> **Beta status.** The API is stabilizing toward a CRAN release. Please file
> issues at <https://github.com/nealcaren/faSTM/issues>. Bug reports, rough
> edges, and feature requests are all welcome.

## License

Apache-2.0.
