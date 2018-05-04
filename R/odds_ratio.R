#' Calculate odds ratio
#'
#' @param df dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param alpha significance level
#' @param ... additional arguments passed to twoxtwo function
#'
#' @return tibble
#' @importFrom rlang "!!"
#' @export
#'
odds_ratio <- function(df, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(df, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::summarise(or = prod(.[1,1], .[2,2]) / prod(.[2,1], .[1,2]),
                     se = sqrt((1/.[1,1]) + (1/.[1,2]) + (1/.[2,1]) + (1/.[2,2]))) %>%
    dplyr::mutate(
      lower = exp(log(.$or) - (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1],
      upper = exp(log(.$or) + (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1]
    ) %>%
    dplyr::select(-2)

}
