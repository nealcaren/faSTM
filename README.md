# faSTM

A fast, modern **Structural Topic Model** for R. faSTM covers the core `stm`
workflow — prevalence and content covariates, FREX/lift/score labels, semantic
coherence, representative documents, covariate-effect estimation, model
selection, out-of-sample inference — but fits in **seconds instead of minutes**
via a Rust core, scales to large corpora, and is self-contained (no dependency
on [`stm`](https://github.com/bstewart/stm)).

`stm` is no longer actively developed (last feature release 2023; 100+ open
issues). faSTM is a clean-room successor with a faster engine, an honest
`estimateEffect`, and a familiar API. Most `stm` analysis code runs with minimal
edits — the fitted object is structurally compatible.

**What faSTM is *not*:** it does not reproduce a *specific* `stm` fit's topics.
STM's objective is non-convex and faSTM uses a different optimizer, so an
independent fit lands in its own (valid, deterministic) optimum — different
topics and numbering, not a relabeling of `stm`'s. For a guaranteed
"replicate the original" run, pass `stm`'s own spectral β via
`stm(..., init.beta = )`. A few `stm` features are also not yet ported (e.g.
`toLDAvis`, `topicQuality`, `init.type = "LDA"/"Custom"`,
`estimateEffect(uncertainty = "Local")`), and `stm`'s SAGE readers
(`sageLabels`/`labelTopics`) do not work on faSTM **content** models — use
`faSTM::sage_labels()` instead.

```r
library(quanteda)   # tokenization (faSTM reads quanteda/tidytext, doesn't reinvent it)
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
semantic_coherence(fit); exclusivity(fit)      # bit-identical to stm
eff <- estimateEffect(1:10 ~ Party, fit, metadata = corpus$meta)
summary(eff)
```

## What's different from stm

- **Speed.** Rust variational EM, multithreaded E-step. ~35× faster than `stm`
  across corpus sizes (10k docs: ~1s vs ~40s), with a `num_threads` knob.
- **Scale.** Opt-in `inference = "svi"` (stochastic variational) for corpora that
  don't fit batch EM — something `stm` cannot do. *(prevalence/content SVI lands
  with topica [#231](https://github.com/nealcaren/topica/issues/231).)*
- **Honest effects.** `estimateEffect()` uses the method of composition,
  propagating per-document posterior uncertainty.
- **Fixes open stm requests.** `frex_scores()` returns the numeric FREX matrix,
  not just words (stm#265); inspection carries the corpus so nothing needs
  re-supplying; deterministic spectral init whose Arora recovery reproduces
  `stm`'s `recoverL2()` step exactly (topica#234, fixed in topica v0.24.0).
- **Self-contained.** Tokenize with `quanteda`/`tidytext` (which the field
  already uses); faSTM reads their objects. No `textProcessor` to inherit bugs
  from.

## Faithful where it counts

On a **shared fit** (same β/θ), faSTM's inspection numbers match `stm`'s:
FREX/prob/score labels are **identical** to `stm::labelTopics`, and
`exclusivity()` / `semantic_coherence()` match to floating point. The fitted
object is `stm`-shaped, so `stm::labelTopics()` and `stm::plot.STM()` run on a
faSTM (non-content) fit during migration. This is parity *given the same fit*,
not a claim that faSTM and `stm` produce the same fit (see above).

## Status

Working: corpus ingestion (quanteda/tidytext/matrix); fast fit (prevalence +
content, threads); the full inspection layer (labels/FREX/coherence/exclusivity/
topic correlations, SAGE labels); honest `estimateEffect`; out-of-sample
inference (`fit_new_documents`); model selection (`search_k`, `select_model`,
`many_topics`); and a modern **ggplot2** plotting layer (topic summary, covariate
effects with CIs, search-K diagnostics, topic-correlation network). Not yet
ported: `toLDAvis`, `topicQuality`, `init.type = "LDA"/"Custom"`,
`estimateEffect(uncertainty = "Local")`, stm-compatible SAGE readers on content
fits. In progress: SVI for covariate models (topica#231).

## Install (beta)

faSTM has a Rust core, so installing from source needs a **Rust toolchain**
(`cargo` + `rustc`) — one-time setup:

```sh
# macOS / Linux: install Rust if you don't have it
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Then, in R — use whichever you already have (they're equivalent;
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

The first install compiles the Rust core (it fetches and builds the pinned
`topica` crate from GitHub — needs an internet connection — and takes a couple of
minutes). After that, the package is **fully self-contained**: it has no runtime
dependency on Rust, `topica`, or Python.

Optional features pull in extra R packages only when used:
`ggplot2`/`ggrepel` (plots), `quanteda`/`tidytext` (text prep), `glmnet`
(`topic_lasso`), `clue` (faster topic alignment).

> **Beta status.** The API is stabilizing toward a CRAN release. Please file
> issues at <https://github.com/nealcaren/faSTM/issues> — bug reports, rough
> edges, and missing `stm` features all welcome.

## License

Apache-2.0.
