context("test-twoxtwo")

test_that("summary computes measures", {

  tmp_twoxtwo <-
    titanic %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, retain = TRUE)

  expect_equal(summary(tmp_twoxtwo)$odds_ratio, odds_ratio(titanic, exposure = Crew, outcome = Survived))
  expect_equal(summary(tmp_twoxtwo)$risk_ratio, risk_ratio(titanic, exposure = Crew, outcome = Survived))
  expect_equal(summary(tmp_twoxtwo)$risk_difference, risk_diff(titanic, exposure = Crew, outcome = Survived))

})

test_that("summary does not try to compute measures with retain FALSE", {

  tmp_twoxtwo <-
    titanic %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, retain = FALSE)

  expect_null(summary(tmp_twoxtwo)$odds_ratio)
  expect_null(summary(tmp_twoxtwo)$risk_ratio)
  expect_null(summary(tmp_twoxtwo)$risk_difference)

})

test_that("levels argument can flip orientation", {

  ## flip the exposure
  tmp_twoxtwo <-
    titanic %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, levels = list(exposure = c(FALSE,TRUE), outcome = c("Yes","No")))

  expect_equal(tmp_twoxtwo$cells$A, 499)
  expect_equal(tmp_twoxtwo$cells$B, 817)
  expect_equal(tmp_twoxtwo$cells$C, 212)
  expect_equal(tmp_twoxtwo$cells$D, 673)

  ## try to flip the outcome too
  tmp_twoxtwo <-
    titanic %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, levels = list(exposure = c(FALSE,TRUE), outcome = c("No","Yes")))

  expect_equal(tmp_twoxtwo$cells$A, 817)
  expect_equal(tmp_twoxtwo$cells$B, 499)
  expect_equal(tmp_twoxtwo$cells$C, 673)
  expect_equal(tmp_twoxtwo$cells$D, 212)

  ## make sure numeric exposure/outcomes that are numeric can flip too
  tmp_twoxtwo <-
    titanic %>%
    dplyr::mutate(Survived = ifelse(Survived == "Yes", 1, 0)) %>%
    twoxtwo(., exposure = Crew, outcome = Survived, levels = list(exposure = c(FALSE,TRUE), outcome = c(1,0)))

  expect_equal(tmp_twoxtwo$cells$A, 499)
  expect_equal(tmp_twoxtwo$cells$B, 817)
  expect_equal(tmp_twoxtwo$cells$C, 212)
  expect_equal(tmp_twoxtwo$cells$D, 673)

})

test_that("levels argument errors with level that does not exist", {

  expect_error({
    tmp_twoxtwo <-
    titanic %>%
    twoxtwo(., exposure = Crew, outcome = Survived, levels = list(exposure = c(FALSE,TRUE), outcome = c("Survived","Died")))
  })

})


test_that("cell count with 0 triggers warning", {

  tmp <-
    dplyr::tribble(~exposed, ~diseased,~n,
                 TRUE, TRUE, 7,
                 TRUE, FALSE,15,
                 FALSE, TRUE, 0,
                 FALSE, FALSE, 19) %>%
    tidyr::uncount(n)

  wrng_msg <- "\nAt least one of the cells in the two-by-two table is 0.\nEstimates may be uninformative."

  expect_warning(odds_ratio(tmp, exposed, diseased), wrng_msg)
  expect_warning(risk_ratio(tmp, exposed, diseased), wrng_msg)
  expect_warning(risk_diff(tmp, exposed, diseased), wrng_msg)
  expect_warning(twoxtwo(tmp, exposed, diseased), wrng_msg)

})

