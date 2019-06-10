#' Calculate odds ratio
#'
#' @param .data dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param alpha significance level
#' @param ... additional arguments passed to [twoxtwo()]
#'
#' @return `tibble`
#' @importFrom rlang "!!"
#' @export
#' @family Effect measures
odds_ratio <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::summarise(odds_ratio = prod(.[1,1], .[2,2]) / prod(.[2,1], .[1,2]),
                     se = sqrt((1/.[1,1]) + (1/.[1,2]) + (1/.[2,1]) + (1/.[2,2])),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    dplyr::mutate(
      ci_lower = exp(log(.$odds_ratio) - (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1],
      ci_upper = exp(log(.$odds_ratio) + (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1]
    ) %>%
    dplyr::select(-2) %>%
    # order exposure and outcome last
    dplyr::select(1,4,5,2,3) %>%
    # make sure all columns are simplified
    dplyr::mutate_all("unlist")

}

#' Calculate risk ratio
#'
#' @param .data dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param alpha significance level
#' @param ... additional arguments passed to [twoxtwo()]
#'
#' @return `tibble`
#' @importFrom rlang "!!"
#' @export
#' @family Effect measures
risk_ratio <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    # risks are in last column
    dplyr::summarise(risk_ratio = .[1,ncol(.)] / .[2,ncol(.)][1,1],
                     se = sqrt(
                       ((1-.[1,ncol(.)])/((.[1,1]+.[1,2])*.[1,ncol(.)])) + ((1-.[2,ncol(.)])/((.[2,1]+.[2,2])*.[2,ncol(.)]))),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    dplyr::mutate(
      ci_lower = exp(log(.$risk_ratio) - (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1],
      ci_upper = exp(log(.$risk_ratio) + (stats::qnorm(1-(alpha/2)) * (.$se)))[1,1]
    ) %>%
    dplyr::select(-2) %>%
    # order exposure and outcome last
    dplyr::select(1,4,5,2,3) %>%
    # make sure all columns are simplified
    dplyr::mutate_all("unlist")


}

#' Calculate risk difference
#'
#' @param .data dataframe
#' @param exposure exposure
#' @param outcome outcome
#' @param alpha significance level
#' @param ... additional arguments passed to [twoxtwo()]
#'
#' @return `tibble`
#' @importFrom rlang "!!"
#' @export
#' @family Effect measures
risk_diff <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...) %>%
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    dplyr::summarise(risk_diff = .[1,ncol(.)] - .[2,ncol(.)],
                     se = sqrt(
                       ((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] )) * (1-((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] ))) * ((1/(.[1,1] + .[1,2])) + (1/(.[2,1] + .[2,2])))
                     ),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    dplyr::mutate(
      ci_lower = .$risk_diff[[1]] - (stats::qnorm(1-(alpha/2)) * (.$se))[1,1],
      ci_upper = .$risk_diff[[1]] + (stats::qnorm(1-(alpha/2)) * (.$se))[1,1]
    ) %>%
    dplyr::select(-2) %>%
    # order exposure and outcome last
    dplyr::select(1,4,5,2,3) %>%
    # make sure all columns are simplified
    dplyr::mutate_all("unlist")

}

