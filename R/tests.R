#' Fisher test
#'
#' @param df dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param ... additional arguments passed to twoxtwo function
#'
#' @return tibble
#' @importFrom rlang "!!"
#' @export
#'
fisher <- function(df, exposure, outcome, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(df, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::select(-c(3,4)) %>%
    dplyr::summarise(odds_ratio = fisher.test(.)$estimate,
                     ci_lower = fisher.test(.)$conf.int[1],
                     ci_upper = fisher.test(.)$conf.int[2],
                     pvalue = fisher.test(.)$p.value)

}
