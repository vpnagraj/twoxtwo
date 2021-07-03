#' Impact numbers
#'
#' @name impact
#'
#' @description
#'
#' Impact numbers are designed to communicate how impactful interventions and/or exposures can be on a population. The \link[twoxtwo]{twoxtwo} framework allows for calculation of impact numbers: exposure impact number (EIN), case impact number (CIN), and the exposed cases impact number (ECIN).
#'
#' The `ein()`, `cin()`, and `ecin()` functions provide interfaces for calculating impact number estimates. Each function takes an input dataset and arguments for outcome and exposure as bare, unquoted variable names. If the input has the  \link[twoxtwo]{twoxtwo} class then the measures will be calculated using exposure and outcome information from that object. The functions all return a tidy `tibble` with the name of the measure, the point estimate, and lower/upper bounds of a confidence interval (CI) based on the SE.
#'
#' Formulas used in point estimate and SE calculations are available in 'Details'.
#'
#' @param .data Either a data frame with observation-level exposure and outcome data or a \link[twoxtwo]{twoxtwo} object
#' @param exposure Name of exposure variable; ignored if input to `.data` is a `twoxtwo` object
#' @param outcome Name of outcome variable; ignored if input to `.data` is a `twoxtwo` object
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param prevalence Prevalence of exposure in the population; must be numeric between `0` and `1`; only used in `cin()`; default is `NULL` and will be ignored
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function; ignored if input to `.data` is a `twoxtwo` object
#'
#' @details
#'
#' The formulas below denote cell values as A,B,C,D. For more on `twoxtwo` notation see the \link[twoxtwo]{twoxtwo} documentation.
#'
#' Note that formulas for standard errors are not provided below but are based on forumlas described in Hildebrandt et al (2006).
#'
#' ## Exposure Impact Number (EIN)
#'
#' \deqn{EIN = 1/((A/(A+B)) - (C/(C+D)))}
#'
#' ## Case Impact Number (CIN)
#'
#' \deqn{CIN = 1/(((A+C)/(A+B+C+D))-(C/(C+D)))) / ((A+C)/(A+B+C+D))}
#'
#'  If "prevalence" argument is not `NULL` then the formula uses the value specified for prevalence of exposure (p):
#'
#'  \deqn{CIN = 1/ ((p * (((A/(A+B)) / (C/(C+D))) - 1)) / (p * (((A/(A+B)) / (C/(C+D))) - 1) + 1))}
#'
#' ## Exposed Cases Impact Number (ECIN)
#'
#' \deqn{ECIN = 1/(1 - (1/((A/(A+B)) / (C/(C+D)))))}
#'
#' @return
#'
#' A `tibble` with the following columns:
#'
#' - **measure**: Name of the measure calculated
#' - **estimate**: Point estimate for the impact number
#' - **ci_lower**: The lower bound of the confidence interval for the estimate
#' - **ci_upper**: The upper bound of the confidence interval for the estimate
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
#'
#' @references Hildebrandt, M., Bender, R., Gehrmann, U., & Blettner, M. (2006). Calculating confidence intervals for impact numbers. BMC medical research methodology, 6, 32. https://doi.org/10.1186/1471-2288-6-32
#' @references Heller, R. F., Dobson, A. J., Attia, J., & Page, J. (2002). Impact numbers: measures of risk factor impact on the whole population from case-control and cohort studies. Journal of epidemiology and community health, 56(8), 606–610. https://doi.org/10.1136/jech.56.8.606
#' @export
#' @md
#'
#'
#'

#' @export
#' @rdname impact
ein <- function(.data, exposure, outcome, alpha = 0.05, ...) {
  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  if(any(class(.data) == "twoxtwo")) {
    tmp_twoxtwo <- .data
  } else {
    ## handle exposure/outcome variable name quotation
    quo_exposure <- dplyr::enquo(exposure)
    quo_outcome <- dplyr::enquo(outcome)

    ## run twoxtwo
    tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)
  }

  tmp_rd <- risk_diff(tmp_twoxtwo)

  tmp_ein <- 1/tmp_rd$estimate
  ci_lower_bound <- 1/tmp_rd$ci_upper
  ci_upper_bound <- 1/tmp_rd$ci_lower

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Exposure Impact Number",
    estimate = tmp_ein,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )
}

#' @export
#' @rdname impact
cin <- function(.data, exposure, outcome, alpha = 0.05, prevalence = NULL, ...) {
  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  if(any(class(.data) == "twoxtwo")) {
    tmp_twoxtwo <- .data
  } else {
    ## handle exposure/outcome variable name quotation
    quo_exposure <- dplyr::enquo(exposure)
    quo_outcome <- dplyr::enquo(outcome)

    ## run twoxtwo
    tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)
  }

  tmp_parp <- parp(tmp_twoxtwo, percent = FALSE, prevalence = prevalence)

  tmp_cin <- 1/tmp_parp$estimate
  ci_lower_bound <- 1/tmp_parp$ci_upper
  ci_upper_bound <- 1/tmp_parp$ci_lower

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Case Impact Number",
    estimate = tmp_cin,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )
}

#' @export
#' @rdname impact
ecin <- function(.data, exposure, outcome, alpha = 0.05, ...) {
  ## get critical value from normal distribution based on value to alpha
  critical_value <- stats::qnorm(1-(alpha/2))

  if(any(class(.data) == "twoxtwo")) {
    tmp_twoxtwo <- .data
  } else {
    ## handle exposure/outcome variable name quotation
    quo_exposure <- dplyr::enquo(exposure)
    quo_outcome <- dplyr::enquo(outcome)

    ## run twoxtwo
    tmp_twoxtwo <- twoxtwo(.data, !! quo_exposure, !! quo_outcome, ...)
  }

  tmp_arp <- arp(tmp_twoxtwo, percent = FALSE)

  tmp_ecin <- 1/tmp_arp$estimate
  ci_lower_bound <- 1/tmp_arp$ci_upper
  ci_upper_bound <- 1/tmp_arp$ci_lower

  ## return everything as a tibble
  dplyr::tibble(
    measure = "Exposed Cases Impact Number",
    estimate = tmp_ecin,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )
}

