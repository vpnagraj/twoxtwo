#' Fisher test
#'
#' @param .data dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param ... additional arguments passed to twoxtwo function
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
    dplyr::summarise(odds_ratio = fisher.test(.)$estimate,
                     ci_lower = fisher.test(.)$conf.int[1],
                     ci_upper = fisher.test(.)$conf.int[2],
                     pvalue = fisher.test(.)$p.value)

}
