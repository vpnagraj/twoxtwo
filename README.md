
<!-- README.md is generated from README.Rmd. Please edit that file -->

# twoxtwo

<!-- badges: start -->

[![build status
(main)](https://github.com/vpnagraj/twoxtwo/workflows/R-CMD-check-main/badge.svg)](https://github.com/vpnagraj/twoxtwo/actions)
[![build status
(dev)](https://github.com/vpnagraj/twoxtwo/workflows/R-CMD-check-dev/badge.svg)](https://github.com/vpnagraj/twoxtwo/actions)
<!-- badges: end -->

The `twoxtwo` package provides a collection of functions to display,
summarize, and analyze data in two-by-two contingency tables.
Statistical analysis functions are oriented towards epidemiological
investigation of exposure/outcome relationships.

## Installation

``` r
## install.packages("devtools")
devtools::install_github("vpnagraj/twoxtwo", build_vignettes = TRUE)
```

## Features

  - `twoxtwo()`: Construct `twoxtwo` object
  - `odds_ratio()`: Estimate odds ratio and confidence interval
  - `risk_ratio()`: Estimate risk ratio and confidence interval
  - `risk_diff()`: Estimate risk difference and confidence interval
  - `fisher()`: Perform Fisher’s exact test
  - `chisq()`: Perform Pearson’s chi-squared test
  - `summary.twoxtwo()`: Summarize `twoxtwo` object
  - `print.twoxtwo()`: Print `twoxtwo` object
  - `display()`: Render `twoxtwo` table contents as a `knitr::kable`

## Usage

### Example

First load `twoxtwo` and `dplyr` to help prep data:

``` r
library(twoxtwo)
library(dplyr)
```

Next create a object with S3 class `twoxtwo`. For this example, use the
`twoxtwo::titanic` dataset. Note that “exposure” and “outcome” variables
must each be binary variables:

``` r
crew_2x2 <-
  titanic %>%
  mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
  twoxtwo(.data = ., exposure = Crew, outcome = Survived)

crew_2x2
# |         |           |OUTCOME      |OUTCOME     |
# |:--------|:----------|:------------|:-----------|
# |         |           |Survived=Yes |Survived=No |
# |EXPOSURE |Crew=TRUE  |212          |673         |
# |EXPOSURE |Crew=FALSE |499          |817         |
```

The `twoxtwo` class has its own `summary.twoxtwo()` method that computes
effect measures (odds ratio, risk ratio, and risk difference):

``` r
summary(crew_2x2)
# 
# |         |           |OUTCOME      |OUTCOME     |
# |:--------|:----------|:------------|:-----------|
# |         |           |Survived=Yes |Survived=No |
# |EXPOSURE |Crew=TRUE  |212          |673         |
# |EXPOSURE |Crew=FALSE |499          |817         |
# 
# 
# Outcome: Survived
# Outcome + : Yes
# Outcome - : No
# 
# Exposure: Crew
# Exposure + : TRUE
# Exposure - : FALSE
# 
# Number of missing observations: 0
# 
# Odds Ratio: 0.516 (0.426,0.624)
# Risk Ratio: 0.632 (0.551,0.724)
# Risk Difference: -0.14 (-0.178,-0.101)
```

Each effect measure can also be calculated without first creating the
`twoxtwo` object:

``` r
titanic %>%
  mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
  odds_ratio(.data = ., exposure = Crew, outcome = Survived)
# # A tibble: 1 x 6
#   measure    estimate ci_lower ci_upper exposure         outcome         
#   <chr>         <dbl>    <dbl>    <dbl> <chr>            <chr>           
# 1 Odds Ratio    0.516    0.426    0.624 Crew::TRUE/FALSE Survived::Yes/No
```

``` r
titanic %>%
  mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
  risk_ratio(.data = ., exposure = Crew, outcome = Survived)
# # A tibble: 1 x 6
#   measure    estimate ci_lower ci_upper exposure         outcome         
#   <chr>         <dbl>    <dbl>    <dbl> <chr>            <chr>           
# 1 Risk Ratio    0.632    0.551    0.724 Crew::TRUE/FALSE Survived::Yes/No
```

``` r
titanic %>%
  mutate(Crew = ifelse(Class == "Crew", TRUE, FALSE)) %>%
  risk_diff(.data = ., exposure = Crew, outcome = Survived)
# # A tibble: 1 x 6
#   measure         estimate ci_lower ci_upper exposure         outcome         
#   <chr>              <dbl>    <dbl>    <dbl> <chr>            <chr>           
# 1 Risk Difference   -0.140   -0.178   -0.101 Crew::TRUE/FALSE Survived::Yes/No
```

### Vignettes

The package includes vignettes to describe usage in more detail.

For more on basic usage:

``` r
vignette("basic-usage", package = "twoxtwo")
```

## Contributing

Please use GitHub issues to report bugs or request features.
Contributions will be reviewed via pull requests.
