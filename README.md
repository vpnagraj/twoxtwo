
<!-- README.md is generated from README.Rmd. Please edit that file -->

# twoxtwo

<!-- badges: start -->

[![build status
(main)](https://github.com/vpnagraj/twoxtwo/workflows/R-CMD-check-main/badge.svg)](https://github.com/vpnagraj/twoxtwo/actions)
[![build status
(dev)](https://github.com/vpnagraj/twoxtwo/workflows/R-CMD-check-dev/badge.svg)](https://github.com/vpnagraj/twoxtwo/actions)
[![](https://cranlogs.r-pkg.org/badges/twoxtwo)](https://cran.r-project.org/package=twoxtwo)
<!-- badges: end -->

The `twoxtwo` package provides a collection of functions to display,
summarize, and analyze data in two-by-two contingency tables.
Statistical analysis functions are oriented towards epidemiological
investigation of exposure/outcome relationships.

## Installation

To install the stable release from CRAN:

``` r
install.packages("twoxtwo")
```

Or to install the development release from GitHub:

``` r
## install.packages("devtools")
devtools::install_github("vpnagraj/twoxtwo", build_vignettes = TRUE)
```

## Features

-   `twoxtwo()`: Construct `twoxtwo` object
-   `odds_ratio()`: Estimate odds ratio and confidence interval
-   `risk_ratio()`: Estimate risk ratio and confidence interval
-   `risk_diff()`: Estimate risk difference and confidence interval
-   `fisher()`: Perform Fisher’s exact test
-   `chisq()`: Perform Pearson’s chi-squared test
-   `arp()`: Estimate attributable risk proportion (ARP) and confidence
    interval
-   `parp()`: Estimate population attributable risk proportion (PARP)
    and confidence interval
-   `ein()`: Estimate exposure impact number (EIN) and confidence
    interval
-   `cin()`: Estimate case impact number (CIN) and confidence interval
-   `ecin()`: Estimate exposed cases impact number (ECIN) and confidence
    interval
-   `summary.twoxtwo()`: Summarize `twoxtwo` object
-   `print.twoxtwo()`: Print `twoxtwo` object
-   `display()`: Render `twoxtwo` table contents as a `knitr::kable`

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

Individual measures of effect, hypothesis tests, and impact numbers can
be calculated using the `twoxtwo` object. For example:

``` r
crew_2x2 %>%
  odds_ratio()
# # A tibble: 1 x 6
#   measure    estimate ci_lower ci_upper exposure         outcome         
#   <chr>         <dbl>    <dbl>    <dbl> <chr>            <chr>           
# 1 Odds Ratio    0.516    0.426    0.624 Crew::TRUE/FALSE Survived::Yes/No
```

``` r
crew_2x2 %>%
  chisq()
# # A tibble: 1 x 9
#   test      estimate ci_lower ci_upper statistic    df   pvalue exposure outcome
#   <chr>     <lgl>    <lgl>    <lgl>        <dbl> <int>    <dbl> <chr>    <chr>  
# 1 Pearson'… NA       NA       NA            46.5     1 8.97e-12 Crew::T… Surviv…
```

Note that data analysis can also be performed without first creating the
`twoxtwo` object:

``` r
titanic %>%
  odds_ratio(.data = ., exposure = Crew, outcome = Survived)
# # A tibble: 1 x 6
#   measure    estimate ci_lower ci_upper exposure         outcome         
#   <chr>         <dbl>    <dbl>    <dbl> <chr>            <chr>           
# 1 Odds Ratio    0.516    0.426    0.624 Crew::TRUE/FALSE Survived::Yes/No
```

``` r
titanic %>%
  chisq(.data = ., exposure = Crew, outcome = Survived)
# # A tibble: 1 x 9
#   test      estimate ci_lower ci_upper statistic    df   pvalue exposure outcome
#   <chr>     <lgl>    <lgl>    <lgl>        <dbl> <int>    <dbl> <chr>    <chr>  
# 1 Pearson'… NA       NA       NA            46.5     1 8.97e-12 Crew::T… Surviv…
```

### Vignettes

The package includes vignettes to describe usage in more detail.

For details on the `twoxtwo` data structure and demonstration of basic
usage:

``` r
vignette("basic-usage", package = "twoxtwo")
```

For formulas and examples of how to calculate measures of effect:

``` r
vignette("measures-of-effect", package = "twoxtwo")
```

For information on hypothesis testing functionality in the package:

``` r
vignette("hypothesis-testing", package = "twoxtwo")
```

For formulas and demonstration of attributable fraction and impact
number calculations:

``` r
vignette("af-impact", package = "twoxtwo")
```

## Contributing

Please use GitHub issues to report bugs or request features.
Contributions will be reviewed via pull requests.
