#' Fisher's exact test
#'
#' @description
#'
#' This function conducts a Fisher's exact test using specified exposure and outcome. Internally the function uses \link[stats]{fisher.test} to test independence of `twoxtwo` rows and columns. The output of the function includes the odds ratio, the lower/upper bounds for the confidence interval around the estimate, and the p-value from the test.
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param alternative Alternative hypothesis for test; must be one of "two.sided", "greater", or "less"; default is `"two.sided"`
#' @param conf_level Confidence level for the confidence interval; default is `0.95`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function
#'
#' @md
#'
#' @return
#'
#' A `tibble` with the following columns:
#'
#' - **test**: Name of the test conducted
#' - **estimate**: Point estimate from the test
#' - **ci_lower**: The lower bound of the confidence interval for the estimate
#' - **ci_upper**: The upper bound of the confidence interval for the estimate
#' - **pvalue**: P-value from the test
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
#'
#' @importFrom rlang "!!"
#' @export
#'
fisher <- function(.data, exposure, outcome, alternative = "two.sided", conf_level = 0.95, ...) {

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl

  tmp_twoxtwo %>%
    dplyr::select(-c(3,4)) %>%
    dplyr::summarise(estimate = stats::fisher.test(., alternative = alternative, conf.level = conf_level)$estimate,
                     ci_lower = stats::fisher.test(., alternative = alternative, conf.level = conf_level)$conf.int[1],
                     ci_upper = stats::fisher.test(., alternative = alternative, conf.level = conf_level)$conf.int[2],
                     pvalue = stats::fisher.test(., alternative = alternative, conf.level = conf_level)$p.value) %>%
    dplyr::mutate(test = "Fisher") %>%
    dplyr::mutate(exposure = unique(tmp_twoxtwo$exposure),
                  outcome = unique(tmp_twoxtwo$outcome)) %>%
    dplyr::select(test, estimate, ci_lower, ci_upper, pvalue, exposure, outcome)

}


#' Pearson's chi-squared test
#'
#' #' @description
#'
#' This function conducts a Pearson's chi-squared test for a `twoxtwo` constructed using the specified exposure and outcome. Internally the function uses \link[stats]{chisq.test}. The output of the function includes the chi-squared test statistic, degrees of freedom, and the p-value from the test.
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param correct Logical as to whether or not to apply continuity correction; default is `TRUE`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function

#' A `tibble` with the following columns:
#'
#' - **test**: Name of the test conducted
#' - **estimate**: Point estimate from the test (`NA` for `chisq()`)
#' - **ci_lower**: The lower bound of the confidence interval for the estimate (`NA` for `chisq()`)
#' - **ci_upper**: The upper bound of the confidence interval for the estimate (`NA` for `chisq()`)
#' - **statistic**: Test statistic from the test
#' - **df**: Degrees of freedom parameter for the test statistic
#' - **pvalue**: P-value from the test
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
#'
#' @importFrom rlang "!!"
#' @export
#'
#' @md
#'
#'
chisq <- function(.data, exposure, outcome, correct = TRUE, ...) {

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl

  tmp2_twoxtwo <-
    tmp_twoxtwo %>%
    dplyr::select(-c(3,4))

  fit <-
    tmp2_twoxtwo %>%
    as.matrix(.) %>%
    stats::chisq.test(., correct = correct)

  dplyr::tibble(test = fit$method,
                estimate = NA,
                ci_lower = NA,
                ci_upper = NA,
                statistic = fit$statistic,
                df = fit$parameter,
                pvalue = fit$p.value,
                exposure = unique(tmp_twoxtwo$exposure),
                outcome = unique(tmp_twoxtwo$outcome))


}
