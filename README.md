# faSTM

A fast, modern **Structural Topic Model** for R. faSTM does everything the
[`stm`](https://github.com/bstewart/stm) package does — prevalence and content
covariates, FREX/lift/score labels, semantic coherence, representative
documents, covariate-effect estimation — but fits in **seconds instead of
minutes** via a Rust core, scales to large corpora, and is self-contained (no
dependency on `stm`).

`stm` is no longer actively developed (last feature release 2023; 100+ open
issues). faSTM is a clean-room successor: the same model and outputs, a faster
engine, and fixes for the long-standing pain points. Existing `stm` analyses
migrate with minimal changes — the fitted object is structurally compatible.

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
  re-supplying; reproducible spectral init (tracking topica#234).
- **Self-contained.** Tokenize with `quanteda`/`tidytext` (which the field
  already uses); faSTM reads their objects. No `textProcessor` to inherit bugs
  from.

## Faithful where it counts

On a shared fit, faSTM's FREX/prob labels are **identical** to `stm::labelTopics`,
and `exclusivity()` / `semantic_coherence()` match `stm` to floating point — so
the numbers reviewers expect don't change. And because the fitted object is
`stm`-shaped, `stm`'s own `labelTopics`/`plot`/`toLDAvis` still work on it during
migration.

## Status

Working: corpus ingestion (quanteda/tidytext/matrix), fast fit (prevalence +
content, threads), the full inspection layer (labels/FREX/coherence/exclusivity/
topic correlations), honest `estimateEffect`, posterior draws. In progress:
native plotting (until then, the `stm`-compatible object covers it), `searchK`
with multicore, SVI for covariate models.

## Install

```r
# requires a Rust toolchain (cargo, rustc)
remotes::install_github("nealcaren/faSTM")
```

## License

Apache-2.0.
