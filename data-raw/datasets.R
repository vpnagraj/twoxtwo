## prep titanic dataset
## expand counts to observation-level
titanic <-
  Titanic %>%
  ## convert to tibble
  dplyr::as_tibble() %>%
  ## expand counts
  tidyr::uncount(n)

usethis::use_data(titanic, overwrite = TRUE)
