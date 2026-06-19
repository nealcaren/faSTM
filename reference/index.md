# Package index

## Fitting a model

Fit a structural topic model and build the prevalence design.

- [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md) : Fit a
  structural topic model (fast Rust backend, stm-compatible object)
- [`s()`](https://nealcaren.github.io/faSTM/reference/s.md) : Spline
  term for prevalence formulas
- [`makeDesignMatrix()`](https://nealcaren.github.io/faSTM/reference/makeDesignMatrix.md)
  : Build a (sparse) design matrix for new data (stm-compatible)

## Covariate effects

Honest effect estimation (method of composition) with weights,
cluster-robust SEs, and random effects; marginal effects and effect
plots.

- [`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)
  : Estimate covariate effects on topic prevalence (method of
  composition)
- [`ame()`](https://nealcaren.github.io/faSTM/reference/ame.md) :
  Average marginal effects from an estimateEffect fit
- [`effect_estimates()`](https://nealcaren.github.io/faSTM/reference/effect_estimates.md)
  : Extract estimateEffect estimates as a tidy data.frame (no plotting)
- [`posterior_theta_samples()`](https://nealcaren.github.io/faSTM/reference/posterior_theta_samples.md)
  : Draw from the per-document topic-proportion posterior
- [`plot(`*`<faSTM_effect>`*`)`](https://nealcaren.github.io/faSTM/reference/plot.faSTM_effect.md)
  : Plot estimated covariate effects on topic prevalence

## Inspecting topics

Labels, representative documents, FREX, and topic correlations.

- [`label_topics()`](https://nealcaren.github.io/faSTM/reference/label_topics.md)
  : Label topics by top words (prob, FREX, lift, score)
- [`sage_labels()`](https://nealcaren.github.io/faSTM/reference/sage_labels.md)
  : Labels for a content (SAGE) model
- [`find_thoughts()`](https://nealcaren.github.io/faSTM/reference/find_thoughts.md)
  : Representative documents for each topic
- [`find_topic()`](https://nealcaren.github.io/faSTM/reference/find_topic.md)
  : Find topics whose top words include given words
- [`topic_terms()`](https://nealcaren.github.io/faSTM/reference/topic_terms.md)
  : Top terms per topic, with their numeric scores (tidy)
- [`topic_proportions()`](https://nealcaren.github.io/faSTM/reference/topic_proportions.md)
  : Expected topic proportions (the numbers behind the summary plot)
- [`content_topics()`](https://nealcaren.github.io/faSTM/reference/content_topics.md)
  : Marginal content words by one content covariate
- [`frex_scores()`](https://nealcaren.github.io/faSTM/reference/frex_scores.md)
  : FREX scores for every word and topic
- [`topic_correlation()`](https://nealcaren.github.io/faSTM/reference/topic_correlation.md)
  : Topic correlation graph (positive correlations of topic proportions)
- [`topic_corr_graph()`](https://nealcaren.github.io/faSTM/reference/topic_corr_graph.md)
  : Topic-correlation network as an igraph graph
- [`plot(`*`<faSTM>`*`)`](https://nealcaren.github.io/faSTM/reference/plot.faSTM.md)
  : Plot a fitted model
- [`plot_topic_network()`](https://nealcaren.github.io/faSTM/reference/plot_topic_network.md)
  : Topic correlation network

## Topic quality

Semantic coherence (Mimno / NPMI / C_V), exclusivity, diagnostics.

- [`coherence()`](https://nealcaren.github.io/faSTM/reference/coherence.md)
  : Topic coherence (Mimno / NPMI / c_v)
- [`semantic_coherence()`](https://nealcaren.github.io/faSTM/reference/semantic_coherence.md)
  : Semantic coherence (Mimno et al. 2011)
- [`exclusivity()`](https://nealcaren.github.io/faSTM/reference/exclusivity.md)
  : Topic exclusivity (FREX-summary, frexw default 0.7)
- [`check_residuals()`](https://nealcaren.github.io/faSTM/reference/check_residuals.md)
  : Residual dispersion check (is K large enough?)

## Choosing the number of topics

Held-out evaluation and model selection across K.

- [`search_k()`](https://nealcaren.github.io/faSTM/reference/search_k.md)
  : Search over the number of topics K

- [`select_model()`](https://nealcaren.github.io/faSTM/reference/select_model.md)
  : Fit several models and keep the ones on the quality frontier

- [`select_best()`](https://nealcaren.github.io/faSTM/reference/select_best.md)
  :

  Pick one model from a `select_model` run

- [`many_topics()`](https://nealcaren.github.io/faSTM/reference/many_topics.md)
  : Select models across a range of K

- [`multi_stm()`](https://nealcaren.github.io/faSTM/reference/multi_stm.md)
  : Cross-run topic stability

- [`make_heldout()`](https://nealcaren.github.io/faSTM/reference/make_heldout.md)
  : Create a held-out version of a corpus for document-completion
  validation

- [`eval_heldout()`](https://nealcaren.github.io/faSTM/reference/eval_heldout.md)
  : Evaluate held-out log-likelihood of a fit on a held-out set

- [`permutation_test()`](https://nealcaren.github.io/faSTM/reference/permutation_test.md)
  : Permutation test for a binary covariate's effect on topics

- [`topic_lasso()`](https://nealcaren.github.io/faSTM/reference/topic_lasso.md)
  : Predict a document-level outcome from topic proportions (lasso)

- [`plot(`*`<faSTM_searchk>`*`)`](https://nealcaren.github.io/faSTM/reference/plot.faSTM_searchk.md)
  : Plot search_k diagnostics

- [`as.data.frame(`*`<faSTM_searchk>`*`)`](https://nealcaren.github.io/faSTM/reference/as.data.frame.faSTM_searchk.md)
  : Convert search_k diagnostics to long form for plotting

## Out-of-sample inference

Infer topic proportions for new documents.

- [`fit_new_documents()`](https://nealcaren.github.io/faSTM/reference/fit_new_documents.md)
  : Infer topic proportions for new documents
- [`predict(`*`<faSTM>`*`)`](https://nealcaren.github.io/faSTM/reference/predict.faSTM.md)
  : Predict topic proportions for new documents

## Tidy (broom) interface

- [`tidy(`*`<faSTM>`*`)`](https://nealcaren.github.io/faSTM/reference/tidy.faSTM.md)
  : Tidy a faSTM fit (topic-term or document-topic distributions)
- [`tidy(`*`<faSTM_effect>`*`)`](https://nealcaren.github.io/faSTM/reference/tidy.faSTM_effect.md)
  : Tidy an estimateEffect fit (one row per term per topic)
- [`glance(`*`<faSTM>`*`)`](https://nealcaren.github.io/faSTM/reference/glance.faSTM.md)
  : One-row model summary for a faSTM fit
- [`augment(`*`<faSTM>`*`)`](https://nealcaren.github.io/faSTM/reference/augment.faSTM.md)
  : Augment: most-likely topic for each document-term token
- [`reexports`](https://nealcaren.github.io/faSTM/reference/reexports.md)
  [`tidy`](https://nealcaren.github.io/faSTM/reference/reexports.md)
  [`glance`](https://nealcaren.github.io/faSTM/reference/reexports.md)
  [`augment`](https://nealcaren.github.io/faSTM/reference/reexports.md)
  : Objects exported from other packages

## Corpus & text preparation

Read prepared text from quanteda / tidytext and convert corpora.

- [`as_corpus()`](https://nealcaren.github.io/faSTM/reference/as_corpus.md)
  : Build a faSTM corpus from prepared text
- [`align_corpus()`](https://nealcaren.github.io/faSTM/reference/align_corpus.md)
  : Align a new corpus to a fitted model's vocabulary
- [`from_tidy()`](https://nealcaren.github.io/faSTM/reference/from_tidy.md)
  : Build a faSTM corpus from a tidy (long) term-count table
- [`make_dt()`](https://nealcaren.github.io/faSTM/reference/make_dt.md)
  : Document-topic proportions as a data frame
- [`read_ldac()`](https://nealcaren.github.io/faSTM/reference/read_ldac.md)
  [`write_ldac()`](https://nealcaren.github.io/faSTM/reference/read_ldac.md)
  : Read/write a corpus in LDA-C (Blei) sparse format

## Datasets

- [`poliblog`](https://nealcaren.github.io/faSTM/reference/poliblog.md)
  : CMU 2008 Political Blog Corpus (poliblog5k)
- [`congress`](https://nealcaren.github.io/faSTM/reference/congress.md)
  : U.S. Congressional Speeches (Party x Chamber, 1987-2011)

## stm-compatibility shims

Aliases that keep `stm`-style call sites working unmodified.

- [`alignCorpus()`](https://nealcaren.github.io/faSTM/reference/alignCorpus.md)
  : Align a new corpus to a reference vocabulary (stm-compatible)
- [`asSTMCorpus()`](https://nealcaren.github.io/faSTM/reference/asSTMCorpus.md)
  : Coerce inputs into an stm-style corpus (stm-compatible)
- [`convertCorpus()`](https://nealcaren.github.io/faSTM/reference/convertCorpus.md)
  : Convert documents/vocab between corpus formats (stm-compatible)
- [`fitNewDocuments()`](https://nealcaren.github.io/faSTM/reference/fitNewDocuments.md)
  : Infer topics for new documents (stm-compatible signature)
- [`checkBeta()`](https://nealcaren.github.io/faSTM/reference/checkBeta.md)
  : Flag words that load almost entirely on one topic
