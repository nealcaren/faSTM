# Build data/congress.rda — run from analysis/congress of the ECTM-paper repo
# (or adjust paths). Underlying speeches: Congressional Record (public domain).
suppressMessages({library(data.table); library(quanteda); library(faSTM)})
set.seed(2138)

## 1) chamber lookup from SpeakerMaps (speakerid -> H/S)
maps <- rbindlist(lapply(list.files("data", "_SpeakerMap.txt$", full.names=TRUE),
  function(f) fread(f, sep="|", colClasses=list(character="speakerid"),
                    select=c("speakerid","chamber"), showProgress=FALSE)), fill=TRUE)
maps <- unique(maps, by="speakerid")
cham <- setNames(maps$chamber, maps$speakerid)

## 2) load prepped speeches, join chamber
d <- fread("congress_for_R.csv", colClasses=list(character="speaker"), showProgress=FALSE)
d[, chamber := cham[speaker]]
d <- d[chamber %in% c("H","S") & party %in% c("D","R")]
d[, ntok := lengths(gregexpr(" ", text)) + 1L]
d <- d[ntok >= 60 & ntok <= 600]                 # readable, comparable lengths

## 3) balanced sample: party x chamber x congress, N per cell
N <- 35L
d[, cell := paste(party, chamber, congress)]
samp <- d[, .SD[sample(.N, min(.N, N))], by=cell]
cat(sprintf("sampled %d speeches across %d cells\n", nrow(samp), uniqueN(samp$cell)))

## 4) dfm from the (already-cleaned, space-tokenized) text; trim vocab
dfmat <- tokens(samp$text) |> dfm() |>
  dfm_trim(min_docfreq = 10, docfreq_type = "count") |>
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")
docvars(dfmat) <- data.frame(
  party    = factor(samp$party,   levels=c("D","R"),    labels=c("Democrat","Republican")),
  chamber  = factor(samp$chamber, levels=c("H","S"),    labels=c("House","Senate")),
  congress = as.integer(samp$congress))
dfmat <- dfm_subset(dfmat, ntoken(dfmat) >= 30)   # drop docs gutted by trimming

## 5) faSTM_corpus (documents/vocab/meta/word_counts), exactly like poliblog
congress <- as_corpus(dfmat)
cat(sprintf("FINAL: %d docs, %d vocab\n", length(congress$documents), length(congress$vocab)))
print(table(congress$meta$party, congress$meta$chamber))
cat("congresses:", paste(range(congress$meta$congress), collapse="-"), "\n")

save(congress, file = "data/congress.rda", compress = "xz")
cat("rda size:", round(file.size("/private/tmp/congress.rda")/1024), "KB\n")
