# faSTM

Fast structural topic models for R, with a **drop-in `stm`-compatible** object.

faSTM swaps in a Rust fitting backend (from
[`topica`](https://github.com/nealcaren/topica)) for the slow part of
[`stm`](https://github.com/bstewart/stm), and hands you back an object that
stm's own functions read unmodified. You keep your stm workflow; the fit gets
faster and scales to large corpora.

```r
library(stm)        # prep + visualization, unchanged
library(faSTM)      # fast fit + honest effects

# prep with stm, exactly as before
processed <- textProcessor(gadarian$open.ended.response, metadata = gadarian)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# fit with the Rust backend; returns an stm-compatible object
fit <- faSTM::stm(out$documents, out$vocab, K = 5,
                  prevalence = ~ treatment, data = out$meta)

# stm's own post-fit tools work unmodified
labelTopics(fit)
plot(fit)
# toLDAvis(fit, out$documents)

# faSTM's honest covariate effects (method of composition)
eff <- faSTM::estimateEffect(1:5 ~ treatment, fit, metadata = out$meta)
summary(eff)
```

## Why this exists

stm's post-fit functions — `labelTopics`, `plot.STM`, `findThoughts`,
`sageLabels`, `toLDAvis`, `estimateEffect` — are **pure readers of the fitted
object**; they never re-run the estimator. So faSTM only has to (1) fit fast in
Rust and (2) return an object shaped like stm's. Everything else is stm's, reused
as-is — no fork, and you inherit stm's future improvements.

The one thing faSTM reimplements is `estimateEffect`, using topica's honest
method-of-composition that propagates per-document posterior uncertainty.

## Large corpora

```r
fit <- faSTM::stm(out$documents, out$vocab, K = 50,
                  inference = "svi",            # stochastic VI
                  batch_size = 256, tau = 64, kappa = 0.7)
```

The `inference = "svi"` path scales fitting beyond what batch EM can hold in
memory. It requires a topica build that includes STM-SVI (topica
[#231](https://github.com/nealcaren/topica/issues/231)); the prevalence/content
+ svi combination is gated with a clear error until that revision is pinned.

## Status

**Scaffold.** Architecture and the full R layer (object constructor, posterior
draws, honest `estimateEffect`) are in place. The Rust binding (`src/rust/`)
wires `topica::ctm::fit_ctm` through a single `fit_stm()` entry point but has not
yet been compiled against a pinned topica — see `DESIGN.md` for the build
checklist and the remaining wiring (rextendr `document()`, the topica git pin,
and the parity/recovery tests).

## Install (once building)

```r
# requires Rust toolchain (cargo, rustc) and rextendr at dev time
# install.packages("rextendr")
remotes::install_github("nealcaren/faSTM")
```

## License

Apache-2.0 (matches topica).
