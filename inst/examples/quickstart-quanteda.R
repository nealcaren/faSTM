#!/usr/bin/env Rscript
# faSTM end to end, with NO dependency on the stm package.
# Tokenize with quanteda -> faSTM corpus -> fit -> inspect -> effects.

library(quanteda)
library(faSTM)

## ---- prepare text with quanteda (faSTM does not tokenize) ------------------
dfmat <- data_corpus_inaugural |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  tokens_remove(stopwords("en")) |>
  tokens_wordstem() |>
  dfm() |>
  dfm_trim(min_termfreq = 5, min_docfreq = 3)

corpus <- as_corpus(dfmat)          # pulls quanteda docvars (Year, Party, ...) as meta
print(corpus)

## ---- choose K (held-out likelihood + coherence/exclusivity) ----------------
cat("\n== search over K (parallel across K) ==\n")
print(search_k(corpus, K = c(5, 10, 15, 20), prevalence = ~ Party, cores = 4, seed = 1))

## ---- fit (Rust core; seconds) ----------------------------------------------
fit <- stm(corpus, K = 10, prevalence = ~ Party, seed = 1, verbose = TRUE)

## ---- inspect ---------------------------------------------------------------
cat("\n== topic labels (prob / FREX / lift / score) ==\n")
print(label_topics(fit, n = 6))

cat("\n== semantic coherence / exclusivity per topic ==\n")
print(round(rbind(coherence = semantic_coherence(fit),
                  exclusivity = exclusivity(fit)), 2))

cat("\n== representative inaugural addresses for topic 1 ==\n")
ft <- find_thoughts(fit, texts = as.character(data_corpus_inaugural),
                    topics = 1, n = 2)
cat(substr(ft$docs[[1]], 1, 120), sep = "\n...\n")

## ---- covariate effect on topic prevalence (honest method of composition) ---
cat("\n== effect of Party on topic prevalence ==\n")
eff <- estimateEffect(1:10 ~ Party, fit, metadata = corpus$meta, nsims = 50, seed = 1)
print(summary(eff)$tables[[1]])
