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

#' Calculate risk ratio
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
risk_ratio <- function(df, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(df, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::mutate(risk = .[[1]] / rowSums(.)) %>%
    dplyr::summarise(rr = .[1,3] / .[2,3],
                     se = sqrt(
                       ((1-.[1,3])/((.[1,1]+.[1,2])*.[1,3])) + ((1-.[2,3])/((.[2,1]+.[2,2])*.[2,3])))) %>%
    dplyr::mutate(
      lower = exp(log(.$rr) - (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1],
      upper = exp(log(.$rr) + (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1]
    ) %>%
    dplyr::select(-2)

}

#' Calculate risk difference
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
risk_diff <- function(df, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(df, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::mutate(
      risk = .[[1]] / rowSums(.)) %>%
    dplyr::summarise(rd = .[1,3] - .[2,3],
                     se = sqrt(
                       ((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] )) * (1-((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] ))) * ((1/(.[1,1] + .[1,2])) + (1/(.[2,1] + .[2,2])))
                     )) %>%
    dplyr::mutate(
      lower = .$rd[[1]] - (stats::qnorm(1-(0.05/2)) * (.$se))[1,1],
      upper = .$rd[[1]] + (stats::qnorm(1-(0.05/2)) * (.$se))[1,1]
    ) %>%
    dplyr::select(-2)

}

