# U.S. Congressional Speeches (Party x Chamber, 1987-2011)

A balanced sample of 1,679 floor speeches from the U.S. House and
Senate, Congresses 100-111 (1987-2011). Speeches are sampled evenly
across party x chamber x congress so covariate effects are estimable,
then lowercased and pruned of stop words and rare terms. Metadata:
`party` (Democrat/Republican), `chamber` (House/Senate), `congress`
(100-111). Built to showcase multiple (crossed) content covariates and
over-time prevalence.

## Usage

``` r
congress
```

## Format

A `faSTM_corpus` with 1,679 documents and a 4,110-term vocabulary.

## Source

Congressional Record, Hein-bound edition (Gentzkow, Shapiro & Taddy),
congresses 100-111. The underlying floor speeches are U.S. government
works (public domain).

## Examples

``` r
data(congress)
fit <- stm(congress, K = 12, prevalence = ~ party + s(congress),
           content = ~ party + chamber)
#> faSTM: crossing 2 content covariates (party, chamber) into a saturated content model with 4 groups.
#> faSTM: fitting K=12 on 1679 docs (batch)...
```
