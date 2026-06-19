#' faSTM: fast structural topic models with an stm-compatible object
#'
#' faSTM fits the logistic-normal structural topic model (prevalence + content
#' covariates) using the Rust core from `topica`, and returns an object
#' compatible with the `stm` package. Text preparation
#' ([stm::textProcessor()], [stm::prepDocuments()]) and post-fit tools
#' ([stm::labelTopics()], [stm::plot.STM()], [stm::findThoughts()],
#' [stm::sageLabels()], [stm::toLDAvis()]) are reused from `stm` unchanged;
#' faSTM contributes the fast fit ([stm()]) and an uncertainty-propagating
#' [estimateEffect()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
