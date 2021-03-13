#' Effect measures
#'
#' @name measures
#'
#' @description
#'
#' ## odds ratio
#'
#' lorem ipsum
#'
#' ## risk ratio
#'
#' lorem ipsum
#'
#' ## risk difference
#'
#' lorem ipsum
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function
#'
#' @return `tibble`
#'
#' @importFrom rlang "!!"
#' @md
#'




#' @export
#' @rdname measures
odds_ratio <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)

  ## get the cell values
  A <- tmp_twoxtwo$cells$A
  B <- tmp_twoxtwo$cells$B
  C <- tmp_twoxtwo$cells$C
  D <- tmp_twoxtwo$cells$D

  ## odds ratio = odds among exposed / odds among unexposed
  ## simplifies to ...
  ## OR = (A*D) / (B*C)
  or <- (A*D) / (B*C)
  ## get standard error of OR
  ## sqrt(1/A + 1/B + 1/C + 1/D)
  se_or <- sqrt(1/A + 1/B + 1/C + 1/D)
  ## use standard error and critical value to establish bounds for CI
  ci_lower_bound <- exp(log(or) - critical_value * se_or)
  ci_upper_bound <- exp(log(or) + critical_value * se_or)

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Odds Ratio",
    estimate = or,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )

}

#' @export
#' @rdname measures
risk_ratio <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)

  ## get the cell values
  A <- tmp_twoxtwo$cells$A
  B <- tmp_twoxtwo$cells$B
  C <- tmp_twoxtwo$cells$C
  D <- tmp_twoxtwo$cells$D

  ## risk ratio = risk among exposed / risk among unexposed
  ## RR = (A / A + B)) / (C / C + D)
  r_exposed <- A / (A + B)
  r_unexposed <- C / (C + D)
  rr <- r_exposed/r_unexposed
  ## get standard error of RR
  se_rr <- sqrt(((1 - r_exposed)/((A+B)*r_exposed)) + ((1-r_unexposed)/((C+D)*r_unexposed)))
  ## use standard error and critical value to establish bounds for CI
  ci_lower_bound <- exp(log(rr) - critical_value * se_rr)
  ci_upper_bound <- exp(log(rr) + critical_value * se_rr)

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Risk Ratio",
    estimate = rr,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )

}

#' @export
#' @rdname measures
risk_diff <- function(.data, exposure, outcome, alpha = 0.05, ...) {

  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  ## handle exposure/outcome variable name quotation
  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## run twoxtwo
  tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)

  ## get the cell values
  A <- tmp_twoxtwo$cells$A
  B <- tmp_twoxtwo$cells$B
  C <- tmp_twoxtwo$cells$C
  D <- tmp_twoxtwo$cells$D

  ## risk difference = risk among exposed minus risk among unexposed
  ## RD = (A / A + B)) / (C / C + D)
  r_exposed <- A / (A + B)
  r_unexposed <- C / (C + D)
  rd <- r_exposed - r_unexposed
  ## get standard error of RD
  # se_rd <- sqrt(((1 - r_exposed)/((A+B)*r_exposed)) + ((1-r_unexposed)/((C+D)*r_unexposed)))
  se_rd <- sqrt(((A + C) / (A + B + C + D)) * (1-((A + C) / (A + B + C + D))) * ((1/(A+B)) + (1/(C+D))))

  ## use standard error and critical value to establish bounds for CI
  ci_lower_bound <- rd - (critical_value * se_rd)
  ci_upper_bound <- rd + (critical_value * se_rd)

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Risk Difference",
    estimate = rd,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )

}

