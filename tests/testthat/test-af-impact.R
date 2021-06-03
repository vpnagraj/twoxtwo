
## source: pmid 16836748, table 2
ex1 <-
  dplyr::tribble(~exposed, ~disease,~n,
                 TRUE, TRUE,69,
                 TRUE, FALSE, 10263,
                 FALSE, TRUE, 100,
                 FALSE, FALSE, 24008) %>%
  tidyr::uncount(n) %>%
  twoxtwo(., exposed, disease)

## source: pmid 16836748, table 4
ex2 <-
  dplyr::tribble(~exposed, ~disease,~n,
                 TRUE, TRUE,420,
                 TRUE, FALSE, 10099,
                 FALSE, TRUE, 153,
                 FALSE, FALSE, 4665) %>%
  tidyr::uncount(n) %>%
  twoxtwo(., exposed, disease)

test_that("arp is correct", {

  ## example 1
  ex1_arp <-
    ex1 %>%
    arp(.)

  ## estimate
  expect_equal(round(ex1_arp$estimate, 3), 0.379)
  ## lower
  expect_equal(round(ex1_arp$ci_lower, 3), 0.189)
  ## upper
  expect_equal(round(ex1_arp$ci_upper, 3), 0.569)

  ## example 2
  ex2_arp <-
    ex2 %>%
    arp(.)

  ## estimate
  expect_equal(round(ex2_arp$estimate, 3), 0.205)
  ## lower
  expect_equal(round(ex2_arp$ci_lower, 3), 0.060)
  ## upper
  expect_equal(round(ex2_arp$ci_upper, 3), 0.349)

})

test_that("parp is correct", {

  ## example 1
  ex1_parp <-
    ex1 %>%
    parp(.)

  ## estimate
  expect_equal(round(ex1_parp$estimate, 3), 0.155)
  ## lower
  expect_equal(round(ex1_parp$ci_lower, 3), 0.049)
  ## upper
  expect_equal(round(ex1_parp$ci_upper, 3), 0.260)

  ## example 2
  ex2_parp <-
    ex2 %>%
    parp(.)

  ## estimate
  expect_equal(round(ex2_parp$estimate, 3), 0.150)
  ## lower
  expect_equal(round(ex2_parp$ci_lower, 3), 0.037)
  ## upper
  expect_equal(round(ex2_parp$ci_upper, 3), 0.263)
})

test_that("ein is correct", {

  ## example 1
  ex1_ein <-
    ex1 %>%
    ein(.)

  ## estimate
  expect_equal(round(ex1_ein$estimate, 2), 395.21)
  ## lower
  expect_equal(round(ex1_ein$ci_lower, 2), 232.67)
  ## upper
  expect_equal(round(ex1_ein$ci_upper, 2), 1311.27)

  ## example 2
  ex2_ein <-
    ex2 %>%
    ein(.)

  ## estimate
  expect_equal(round(ex2_ein$estimate, 2), 122.37)
  ## lower
  expect_equal(round(ex2_ein$ci_lower, 2), 69.55)
  ## upper
  expect_equal(round(ex2_ein$ci_upper, 2), 508.69)
})

test_that("cin is correct", {

  ## example 1
  ex1_cin <-
    ex1 %>%
    cin(.)

  ## estimate
  expect_equal(round(ex1_cin$estimate, 2), 6.46)
  ## lower
  expect_equal(round(ex1_cin$ci_lower, 2), 3.84)
  ## upper
  expect_equal(round(ex1_cin$ci_upper, 2), 20.36)

  ## example 2
  ex2_cin <-
    ex2 %>%
    cin(.)

  ## estimate
  expect_equal(round(ex2_cin$estimate, 2), 6.67)
  ## lower
  expect_equal(round(ex2_cin$ci_lower, 2), 3.80)
  ## upper
  expect_equal(round(ex2_cin$ci_upper, 2), 27.27)

})

test_that("ecin is correct", {

  ## example 1
  ex1_ecin <-
    ex1 %>%
    ecin(.)

  ## estimate
  expect_equal(round(ex1_ecin$estimate, 2), 2.64)
  ## lower
  expect_equal(round(ex1_ecin$ci_lower, 2), 1.76)
  ## upper
  expect_equal(round(ex1_ecin$ci_upper, 2), 5.29)

  ## example 2
  ex2_ecin <-
    ex2 %>%
    ecin(.)

  ## estimate
  expect_equal(round(ex2_ecin$estimate, 2), 4.89)
  ## lower
  expect_equal(round(ex2_ecin$ci_lower, 2), 2.86)
  ## upper
  expect_equal(round(ex2_ecin$ci_upper, 2), 16.67)

})

