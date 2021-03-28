test_that("fisher test p-value comes through as expected", {

  tmp_fisher_res <-
    titanic %>%
    dplyr::mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
    fisher(., Crew, Survived)

  expect_lt(tmp_fisher_res$pvalue, 0.05)
})
