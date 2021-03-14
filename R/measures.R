#' Measures of effect
#'
#' @name measures
#'
#' @description
#'
#' The \link[twoxtwo]{twoxtwo} framework allows for estimation of the magnitude of association between an exposure and outcome. Measures of effect that can be calculated include odds ratio, risk ratio, and risk difference. Each measure can be calculated as a point estimate as well as the standard error (SE) around that value. It is critical to note that the interpretation of measures of effect depends on the study design and research question being investigated.
#'
#' The `odds_ratio()`, `risk_ratio()`, and `risk_diff()` functions provide a standard interface for calculating measures of effect. Each function takes an input dataset and arguments for outcome and exposure as bare, unquoted variable names. The functions all return a tidy `tibble` with the name of the measure, the point estimate, and lower/upper bounds of a confidence interval (CI) based on the SE.
#'
#' Formulas used in point estimate and SE calculations are available in 'Details'.
#'
#' @details
#'
#' The formulas below denote cell values as A,B,C,D. For more on `twoxtwo` notation see the \link[twoxtwo]{twoxtwo} documentation.
#'
#' ## Odds Ratio
#'
#' \deqn{OR = (A*D)/(B*C)}
#'
#' \deqn{seOR = sqrt(1/A + 1/B + 1/C + 1/D)}
#'
#' ## Risk Ratio
#'
#' \deqn{RR = (A/(A+B)) / (C/(C+D))}
#'
#' \deqn{seRR = sqrt(((1 - (A/(A+B)))/((A+B)*(A/(A+B)))) + ((1-(C/(C+D)))/((C+D)*(C/(C+D)))))}
#'
#' ## Risk Difference
#'
#' \deqn{RD = (A/(A+B)) - (C/(C+D))}
#'
#' \deqn{seRD = sqrt(((A*B)/((A+B)^3)) + ((C*D)/((C+D)^3)))}
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function
#'
#' @return
#'
#' A `tibble` with the following columns:
#'
#' - **measure**: Name of the measure calculated
#' - **estimate**: Point estimate for the effect measure
#' - **ci_lower**: The lower bound of the confidence interval for the estimate
#' - **ci_upper**: The upper bound of the confidence interval for the estimate
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
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
  ## se_RD = sqrt((A*B) / ((A+B)^3) + (C*D)/((C+D)^3))
  r_exposed <- A / (A + B)
  r_unexposed <- C / (C + D)
  rd <- r_exposed - r_unexposed
  ## get standard error of RD
  se_rd <- sqrt(((A*B)/((A+B)^3)) + ((C*D)/((C+D)^3)))



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

