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

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl %>%
    ## A = value at first row, first column [1,1]
    ## B = value at second row, first column [2,1]
    ## C = value at first row, second column [1,2]
    ## D = value at second row, second column [2,2]
    ## each of the above should be single numeric vectors
    ## use the above to calculate odds ratio and standard error
    ## OR = A*D / B*C
    ## seOR = sqrt(1/A + 1/B + 1/C + 1/D)
    dplyr::summarise(odds_ratio = prod(.[1,1], .[2,2]) / prod(.[2,1], .[1,2]),
                     se = sqrt((1/.[1,1]) + (1/.[1,2]) + (1/.[2,1]) + (1/.[2,2])),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    ## get confidence interval around odds ratio ...
    ## exponentiate the log of odds ratio +/- critical value times standard error
    dplyr::mutate(
      ci_lower = exp(log(.$odds_ratio) - (critical_value * (.$se))),
      ci_upper = exp(log(.$odds_ratio) + (critical_value * (.$se)))
    ) %>%
    ## select columns of interest
    dplyr::select(odds_ratio, ci_lower, ci_upper, exposure, outcome) %>%
    ## make sure all columns are simplified
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

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl %>%
    ## A = value at first row, first column [1,1]
    ## B = value at second row, first column [2,1]
    ## C = value at first row, second column [1,2]
    ## D = value at second row, second column [2,2]
    ## risks = (A / A + B) and (C / C + D)
    ## get A + B, C + D respectively with rowSums()
    ## the .[[1]] will give you the first column (A,C)
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    ## the risk ratio (RR) is the risk in first row (exposured) divided by the risk in second row (unexposed)
    ## risk is in the last column (hence ncol(.))
    ## also calculate seRR using formula with A,B,C,D values
    dplyr::summarise(risk_ratio = .[1,ncol(.)] / .[2,ncol(.)],
                     se = sqrt(
                       ((1-.[1,ncol(.)])/((.[1,1]+.[1,2])*.[1,ncol(.)])) + ((1-.[2,ncol(.)])/((.[2,1]+.[2,2])*.[2,ncol(.)]))),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    ## extract value for RR and use that with critical value and seRR to get the lower and upper bounds of CI
    dplyr::mutate(
      ci_lower = exp(log(.$risk_ratio) - (critical_value * (.$se))),
      ci_upper = exp(log(.$risk_ratio) + (critical_value * (.$se)))
    ) %>%
    ## select columns of interest
    dplyr::select(risk_ratio, ci_lower, ci_upper, exposure, outcome) %>%
    ## make sure all columns are simplified
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

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)$tbl %>%
    ## A = value at first row, first column [1,1]
    ## B = value at second row, first column [2,1]
    ## C = value at first row, second column [1,2]
    ## D = value at second row, second column [2,2]
    ## risks = (A / A + B) and (C / C + D)
    ## get A + B, C + D respectively with rowSums()
    ## the .[[1]] will give you the first column (A,C)
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    ## the risk difference (RD) is the risk in first row (exposed) minus risk in second row (unexposed)
    ## risk is in the last column (hence ncol(.))
    ## also calculate seRD using formula with A,B,C,D values
    dplyr::summarise(risk_diff = .[1,ncol(.)] - .[2,ncol(.)],
                     se = sqrt(
                       ((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] )) * (1-((.[1,1] + .[2,1]) / (.[1,1] + .[2,1] + .[1,2] + .[2,2] ))) * ((1/(.[1,1] + .[1,2])) + (1/(.[2,1] + .[2,2])))
                     ),
                     exposure = dplyr::first(exposure),
                     outcome = dplyr::first(outcome)) %>%
    ## extract value for RD and use that with critical value and seRD to get the lower and upper bounds of CI
    dplyr::mutate(
      ci_lower = .$risk_diff[[1]] - (critical_value * (.$se)),
      ci_upper = .$risk_diff[[1]] + (critical_value * (.$se))
    ) %>%
    ## select columns of interest
    dplyr::select(risk_diff, ci_lower, ci_upper, exposure, outcome) %>%
    ## make sure all columns are simplified
    dplyr::mutate_all("unlist")

}

