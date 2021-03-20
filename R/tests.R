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
#' - **exposure**: Name of the exposure variable followed by +/- lfevels (e.g. smoking::yes/no)
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
