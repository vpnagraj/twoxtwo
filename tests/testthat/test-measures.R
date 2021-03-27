context("test-measures")

test_that("risk ratio is correct", {

  ## source: szklo and nieto 3rd edition, appendix A3
  tmp <-
    dplyr::tribble(
    ~ severe_hypertension, ~ myocardial_infarction, ~ n,
    1, 1, 180,
    1, 0, 9820,
    0, 1, 30,
    0, 0, 9970
  ) %>%
    tidyr::uncount(n) %>%
    risk_ratio(., severe_hypertension, myocardial_infarction)

  ## estimate
  expect_equal(round(tmp$estimate, 2), 6.00)
  ## lower
  expect_equal(round(tmp$ci_lower, 2), 4.08)
  ## upper
  expect_equal(round(tmp$ci_upper, 2), 8.82)

})

test_that("odds ratio is correct", {

  ## source: szklo and nieto 3rd edition, appendix A4
  tmp <-
    dplyr::tribble(
      ~ severe_hypertension, ~ myocardial_infarction, ~ n,
      1, 1, 180,
      1, 0, 982,
      0, 1, 30,
      0, 0, 997
    ) %>%
    tidyr::uncount(n) %>%
    odds_ratio(., severe_hypertension, myocardial_infarction)

  ## estimate
  expect_equal(round(tmp$estimate, 2), 6.09)
  ## lower
  expect_equal(round(tmp$ci_lower, 2), 4.10)
  ## upper
  expect_equal(round(tmp$ci_upper, 2), 9.06)

})


test_that("risk difference is correct", {

  ## source: keyes and galea, chapter 6
  tmp <-
    dplyr::tribble(
      ~ poor_nutrition, ~ obesity, ~ n,
      1, 1, 70,
      1, 0, 330,
      0, 1, 100,
      0, 0, 200
    ) %>%
    tidyr::uncount(n) %>%
    risk_diff(., poor_nutrition, obesity)

  ## estimate
  expect_equal(round(tmp$estimate, 3), -0.158)
  ## lower
  expect_equal(round(tmp$ci_lower, 3), -0.223)
  ## upper
  expect_equal(round(tmp$ci_upper, 3), -0.093)

})

