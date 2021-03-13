#' Fisher test
#'
#' @description
#'
#' description
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function
#'
#' @md
#'
#' @return tibble
#' @importFrom rlang "!!"
#' @export
#'
fisher <- function(.data, exposure, outcome, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl %>%
    dplyr::select(-c(3,4)) %>%
    dplyr::summarise(odds_ratio = stats::fisher.test(.)$estimate,
                     ci_lower = stats::fisher.test(.)$conf.int[1],
                     ci_upper = stats::fisher.test(.)$conf.int[2],
                     pvalue = stats::fisher.test(.)$p.value)

}
