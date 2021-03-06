#' Fisher's exact test
#'
#' @description
#'
#' This function conducts a Fisher's exact test using specified exposure and outcome. Internally the function uses \link[stats]{fisher.test} to test independence of `twoxtwo` rows and columns. The output of the function includes the odds ratio, the lower/upper bounds for the confidence interval around the estimate, and the p-value from the test.
#'
#' @param .data Either a data frame with observation-level exposure and outcome data or a \link[twoxtwo]{twoxtwo} object
#' @param exposure Name of exposure variable; ignored if input to `.data` is a `twoxtwo` object
#' @param outcome Name of outcome variable; ignored if input to `.data` is a `twoxtwo` object
#' @param alternative Alternative hypothesis for test; must be one of "two.sided", "greater", or "less"; default is `"two.sided"`
#' @param conf_level Confidence level for the confidence interval; default is `0.95`
#' @param or Hypothesized odds ratio; default is `1`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function; ignored if input to `.data` is a `twoxtwo` object
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
#' - **statistic**: Test statistic from the test (`NA` for `fisher()`)
#' - **df**: Degrees of freedom parameter for the test statistic (`NA` for `fisher()`)
#' - **pvalue**: P-value from the test
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
#'
#' @importFrom rlang "!!"
#' @export
#'
fisher <- function(.data, exposure, outcome, alternative = "two.sided", conf_level = 0.95, or = 1, ...) {

  if(any(class(.data) == "twoxtwo")) {
    tmp_twoxtwo <- .data
  } else {
    ## handle exposure/outcome variable name quotation
    quo_exposure <- dplyr::enquo(exposure)
    quo_outcome <- dplyr::enquo(outcome)

    ## run twoxtwo
    tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)
  }

  tmp_twoxtwo <- tmp_twoxtwo$tbl

  tmp_twoxtwo2 <-
    tmp_twoxtwo %>%
    dplyr::select(-c(3,4))

  fit <-
    tmp_twoxtwo2 %>%
    stats::fisher.test(., alternative = alternative, conf.level = conf_level, or = or)

  dplyr::tibble(test = fit$method,
                estimate = fit$estimate,
                ci_lower = fit$conf.int[1],
                ci_upper = fit$conf.int[2],
                statistic = NA,
                df = NA,
                pvalue = fit$p.value,
                exposure = unique(tmp_twoxtwo$exposure),
                outcome = unique(tmp_twoxtwo$outcome))

}


#' Pearson's chi-squared test
#'
#' @description
#'
#' This function conducts a Pearson's chi-squared test for a `twoxtwo` constructed using the specified exposure and outcome. Internally the function uses \link[stats]{chisq.test}. The output of the function includes the chi-squared test statistic, degrees of freedom, and the p-value from the test.
#'
#' @param .data Either a data frame with observation-level exposure and outcome data or a \link[twoxtwo]{twoxtwo} object
#' @param exposure Name of exposure variable; ignored if input to `.data` is a `twoxtwo` object
#' @param outcome Name of outcome variable; ignored if input to `.data` is a `twoxtwo` object
#' @param correct Logical as to whether or not to apply continuity correction; default is `TRUE`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function; ignored if input to `.data` is a `twoxtwo` object
#'
#' @return
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

  if(any(class(.data) == "twoxtwo")) {
    tmp_twoxtwo <- .data
  } else {
    ## handle exposure/outcome variable name quotation
    quo_exposure <- dplyr::enquo(exposure)
    quo_outcome <- dplyr::enquo(outcome)

    ## run twoxtwo
    tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)
  }

  tmp_twoxtwo <- tmp_twoxtwo$tbl

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
