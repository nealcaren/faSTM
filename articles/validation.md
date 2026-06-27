# Validation: parity with stm, and fit quality

faSTM is a reimplementation of the Structural Topic Model, not a wrapper
around `stm`. It has its own optimizer (a Rust variational EM), so two
questions decide whether you can trust it:

1.  **Given the same fitted model, do faSTM’s post-fit numbers match
    `stm`’s?**
2.  **Do faSTM’s own fits reach the same quality as `stm`’s**, even
    though the topic decomposition differs?

This article answers both by running `stm` live and comparing. Every
check is guarded with
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html), so this page
fails to build if parity ever breaks.

``` r

library(faSTM)
library(stm)
data(poliblog)
out <- list(documents = poliblog$documents, vocab = poliblog$vocab, meta = poliblog$meta)
```

## Same model, same numbers

The fitted object is `stm`-shaped, so `stm`’s own readers run on a faSTM
fit. We fit once with faSTM, then compute every inspection metric **both
ways** (with faSTM’s functions and with `stm`’s) on that single shared
model. Identical inputs should give identical outputs.

``` r

fit <- faSTM::stm(out$documents, out$vocab, K = 20,
                  prevalence = ~ rating + s(day), data = out$meta,
                  init.type = "Spectral", seed = 2138, verbose = FALSE)
```

### Topic labels (probability, FREX, lift, score)

``` r

fa <- faSTM::label_topics(fit, n = 7)
sl <- stm::labelTopics(fit, n = 7)
same <- vapply(c("prob", "frex", "lift", "score"),
               function(m) identical(unname(fa[[m]]), unname(sl[[m]])), logical(1))
same
#>  prob  frex  lift score 
#>  TRUE  TRUE  TRUE  TRUE
stopifnot(all(same))
```

All four rankings return the identical words, topic by topic.

### Semantic coherence and exclusivity

``` r

coh_diff  <- max(abs(faSTM::semantic_coherence(fit, M = 10) -
                     stm::semanticCoherence(fit, documents = out$documents, M = 10)))
excl_diff <- max(abs(faSTM::exclusivity(fit, M = 10) -
                     stm::exclusivity(fit, M = 10)))
c(max_coherence_diff = coh_diff, max_exclusivity_diff = excl_diff)
#>   max_coherence_diff max_exclusivity_diff 
#>         2.842171e-14         0.000000e+00
stopifnot(coh_diff < 1e-8, excl_diff < 1e-8)
```

Both agree to floating-point precision: faSTM’s `inspect.R` ports
`stm`’s `semCoh1beta` / `js.estimate` / FREX formulas directly.

## Different fit, comparable quality

faSTM’s optimizer is not `stm`’s, and the STM objective is non-convex,
so an independent run settles into its own optimum with its own topic
numbering. The question is whether that optimum is as *good*. The fair,
engine-neutral test is held-out predictive likelihood by document
completion: hold out half the tokens in each document, fit on the rest,
and score the held-out tokens. We run the same held-out set through both
packages.

``` r

ho <- stm::make.heldout(out$documents, out$vocab, seed = 2138)

ff <- faSTM::stm(ho$documents, out$vocab, K = 20, prevalence = ~ rating + s(day),
                 data = out$meta, init.type = "Spectral", seed = 2138, verbose = FALSE)
sf <- stm::stm(ho$documents, out$vocab, K = 20, prevalence = ~ rating + s(day),
               data = out$meta, init.type = "Spectral", seed = 2138, verbose = FALSE)

ll_faSTM <- mean(stm::eval.heldout(ff, ho$missing)$expected.heldout)
ll_stm   <- mean(stm::eval.heldout(sf, ho$missing)$expected.heldout)

data.frame(
  engine     = c("faSTM", "stm"),
  heldout_LL = round(c(ll_faSTM, ll_stm), 4),
  iterations = c(ff$convergence$its, length(sf$convergence$bound)))
#>   engine heldout_LL iterations
#> 1  faSTM    -6.9043         34
#> 2    stm    -6.8976         23
```

``` r

rel_gap <- abs(ll_faSTM - ll_stm) / abs(ll_stm)
round(100 * rel_gap, 3)               # percent difference in held-out likelihood
#> [1] 0.098
stopifnot(rel_gap < 0.02)             # within 2%
```

The two fits land within a fraction of a percent on held-out likelihood,
so the optima are of comparable quality. faSTM reaches it in more
iterations, but each iteration is cheaper, so it still converges faster
in wall-clock time (see the [Get
started](https://nealcaren.github.io/faSTM/articles/faSTM.md) article).

## What this means for your analysis

- **Post-fit numbers are safe to compare.** Labels, coherence, and
  exclusivity computed by faSTM equal `stm`’s for any given model.
- **Fits are not identical, by design.** faSTM and `stm` find different
  (equally good) topic decompositions. For a result that must survive
  replication, fit your final, reported model in whichever package your
  readers will rerun, and report the package and version. This page is
  the evidence that either choice is sound.
