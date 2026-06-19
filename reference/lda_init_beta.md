# LDA topic-word matrix via topica's CVB0 (deterministic collapsed variational Bayes), to seed a "replicate stm's LDA init" STM fit. Mirrors stm's collapsed-Gibbs LDA initialization; the result is fed back as `init_beta`. Returns K\*V row-major topic-word probabilities.

LDA topic-word matrix via topica's CVB0 (deterministic collapsed
variational Bayes), to seed a "replicate stm's LDA init" STM fit.
Mirrors stm's collapsed-Gibbs LDA initialization; the result is fed back
as `init_beta`. Returns K\*V row-major topic-word probabilities.

## Usage

``` r
lda_init_beta(
  docs_flat,
  doc_lens,
  num_types,
  num_topics,
  iters,
  alpha,
  beta,
  seed
)
```
