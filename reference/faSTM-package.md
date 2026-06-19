# faSTM: fast structural topic models with an stm-compatible object

faSTM fits the logistic-normal structural topic model (prevalence +
content covariates) using the Rust core from `topica`, and returns an
object compatible with the `stm` package. Text preparation
([`stm::textProcessor()`](https://rdrr.io/pkg/stm/man/textProcessor.html),
[`stm::prepDocuments()`](https://rdrr.io/pkg/stm/man/prepDocuments.html))
and post-fit tools
([`stm::labelTopics()`](https://rdrr.io/pkg/stm/man/labelTopics.html),
[`stm::plot.STM()`](https://rdrr.io/pkg/stm/man/plot.STM.html),
[`stm::findThoughts()`](https://rdrr.io/pkg/stm/man/findThoughts.html),
[`stm::sageLabels()`](https://rdrr.io/pkg/stm/man/sageLabels.html),
[`stm::toLDAvis()`](https://rdrr.io/pkg/stm/man/toLDAvis.html)) are
reused from `stm` unchanged; faSTM contributes the fast fit
([`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md)) and an
uncertainty-propagating
[`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md).

## See also

Useful links:

- <https://nealcaren.github.io/faSTM/>

- <https://github.com/nealcaren/faSTM>

- Report bugs at <https://github.com/nealcaren/faSTM/issues>

## Author

**Maintainer**: Neal Caren <neal.caren@unc.edu>

Authors:

- Neal Caren <neal.caren@unc.edu>

Other contributors:

- Margaret Roberts (author of stm; inspection formulas in inspect.R
  adapted from stm (MIT)) \[copyright holder\]

- Brandon Stewart (author of stm (MIT)) \[copyright holder\]

- Dustin Tingley (author of stm (MIT)) \[copyright holder\]
