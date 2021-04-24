## prep titanic dataset
## expand counts to observation-level
titanic <-
  Titanic %>%
  ## convert to tibble
  dplyr::as_tibble() %>%
  ## expand counts
  tidyr::uncount(n) %>%
  ## create crew variable
  dplyr::mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE), .after = "Class")

usethis::use_data(titanic, overwrite = TRUE)
