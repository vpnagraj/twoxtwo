test_that("fisher test p-value comes through as expected", {

  tmp_fisher_res <-
    titanic %>%
    fisher(., Crew, Survived)

  ## should be p < 0.05
  expect_lt(tmp_fisher_res$pvalue, 0.05)
})

test_that("chisq test p-value comes through as expected", {

  tmp_chisq_res <-
    titanic %>%
    chisq(., Crew, Survived)

  ## should be p < 0.05
  expect_lt(tmp_chisq_res$pvalue, 0.05)
})

