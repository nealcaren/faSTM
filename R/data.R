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
