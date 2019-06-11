library(twoxtwo)

titanic <-
  as.data.frame(Titanic, stringsAsFactors = FALSE) %>%
  expand_counts(Freq)

save(titanic, file="data/titanic.rda")
