#' CMU 2008 Political Blog Corpus (poliblog5k)
#'
#' 5,000 political blog posts from the 2008 U.S. election, the worked example
#' from the `stm` package vignette, packaged as a ready-to-use [faSTM_corpus].
#' Metadata: `rating` (Conservative/Liberal), `day` (1-365), `blog`, `text`.
#'
#' @format A `faSTM_corpus` with 5,000 documents and a 2,632-term vocabulary.
#' @source Eisenstein & Xing (2010), via the \pkg{stm} package.
#' @examples
#' data(poliblog)
#' fit <- stm(poliblog, K = 20, prevalence = ~ rating + s(day))
"poliblog"

#' U.S. Congressional Speeches (Party x Chamber, 1987-2011)
#'
#' A balanced sample of 1,679 floor speeches from the U.S. House and Senate,
#' Congresses 100-111 (1987-2011). Speeches are sampled evenly across
#' party x chamber x congress so covariate effects are estimable, then lowercased
#' and pruned of stop words and rare terms. Metadata: `party`
#' (Democrat/Republican), `chamber` (House/Senate), `congress` (100-111). Built
#' to showcase multiple (crossed) content covariates and over-time prevalence.
#'
#' @format A `faSTM_corpus` with 1,679 documents and a 4,110-term vocabulary.
#' @source Congressional Record, Hein-bound edition (Gentzkow, Shapiro & Taddy),
#'   congresses 100-111. The underlying floor speeches are U.S. government works
#'   (public domain).
#' @examples
#' data(congress)
#' fit <- stm(congress, K = 12, prevalence = ~ party + s(congress),
#'            content = ~ party + chamber)
"congress"
