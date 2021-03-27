context("test-twoxtwo")

test_that("summary computes measures", {

  tmp <-
    titanic %>%
    dplyr::mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE))

  tmp_twoxtwo <-
    tmp %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, retain = TRUE)

  expect_equal(summary(tmp_twoxtwo)$odds_ratio, odds_ratio(tmp, exposure = Crew, outcome = Survived))
  expect_equal(summary(tmp_twoxtwo)$risk_ratio, risk_ratio(tmp, exposure = Crew, outcome = Survived))
  expect_equal(summary(tmp_twoxtwo)$risk_difference, risk_diff(tmp, exposure = Crew, outcome = Survived))

})

test_that("summary does not try to compute measures with retain FALSE", {

  tmp_twoxtwo <-
    titanic %>%
    dplyr::mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
    twoxtwo::twoxtwo(., exposure = Crew, outcome = Survived, retain = FALSE)

  expect_null(summary(tmp_twoxtwo)$odds_ratio)
  expect_null(summary(tmp_twoxtwo)$risk_ratio)
  expect_null(summary(tmp_twoxtwo)$risk_difference)

})
