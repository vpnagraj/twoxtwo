#' Attributable fractions
#'
#' @name af
#'
#' @description
#'
#' In addition to \link[twoxtwo]{measures} of effect such as odds ratio, risk ratio, and risk difference, the \link[twoxtwo]{twoxtwo} framework allows for calculation of attributable fractions: attributable risk in the exposed (ARP) and the population attributable risk (PARP).
#'
#' Estimates of the attributable fractions can be calculated with the `arp()` and `parp()` functions respectively.  Each function takes an input dataset and arguments for outcome and exposure as bare, unquoted variable names. If the input has the  \link[twoxtwo]{twoxtwo} class then the effect measures will be calculated using exposure and outcome information from that object. The functions all return a tidy `tibble` with the name of the measure, the point estimate, and lower/upper bounds of a confidence interval (CI) based on the SE.
#'
#' Formulas used in point estimate and SE calculations are available in 'Details'.
#'
#' @details
#'
#' The formulas below denote cell values as A,B,C,D. For more on `twoxtwo` notation see the \link[twoxtwo]{twoxtwo} documentation.
#'
#'
#' @param .data Either a data frame with observation-level exposure and outcome data or a \link[twoxtwo]{twoxtwo} object
#' @param exposure Name of exposure variable; ignored if input to `.data` is a `twoxtwo` object
#' @param outcome Name of outcome variable; ignored if input to `.data` is a `twoxtwo` object
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param percent Logical as to whether or not the measure should be returned as a percentage; default is `FALSE`
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function; ignored if input to `.data` is a `twoxtwo` object
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
#' @references Hildebrandt, M., Bender, R., Gehrmann, U., & Blettner, M. (2006). Calculating confidence intervals for impact numbers. BMC medical research methodology, 6, 32. https://doi.org/10.1186/1471-2288-6-32
#' @references Szklo, M., & Nieto, F. J. (2007). Epidemiology: Beyond the basics. Sudbury, Massachussets: Jones and Bartlett.
#'
#' @importFrom rlang "!!"
#' @md
#'


#' @export
#' @rdname af
arp <- function(.data, exposure, outcome, alpha = 0.05, percent = FALSE, ...) {

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

  ## get the cell values
  A <- tmp_twoxtwo$cells$A
  B <- tmp_twoxtwo$cells$B
  C <- tmp_twoxtwo$cells$C
  D <- tmp_twoxtwo$cells$D

  r_unexposed <- C / (C + D)
  r_exposed <- A / (A + B)

  tmp_rr <- r_exposed / r_unexposed

  tmp_arp <- 1 - (1/tmp_rr)

  ## calculate SE
  se_tmp_arp <-
    sqrt(
      ((1/tmp_rr)^2) *
        (((1-r_unexposed) / ((C+D)*r_unexposed)) + ((1-r_exposed) / ((A+B)*r_exposed)))
    )

  ci_lower_bound <- tmp_arp - (critical_value*se_tmp_arp)
  ci_upper_bound <- tmp_arp + (critical_value*se_tmp_arp)

  #tmp_par <- ((tmp_rr - 1) / tmp_rr)
  if(percent) {
    tmp_arp <- tmp_arp * 100
    ci_lower_bound <- ci_lower_bound * 100
    ci_upper_bound <- ci_upper_bound * 100
  }

  ## return everything as a tibble
  dplyr::tibble(
    measure = ifelse(percent, "Attributable Risk Percentage", "Attributable Risk Proportion"),
    estimate = tmp_arp,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )

}

#' @export
#' @rdname af
parp <- function(.data, exposure, outcome, alpha = 0.05, percent = FALSE, ...) {

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

  ## get the cell values
  A <- tmp_twoxtwo$cells$A
  B <- tmp_twoxtwo$cells$B
  C <- tmp_twoxtwo$cells$C
  D <- tmp_twoxtwo$cells$D

  r_exposed <- A / (A + B)
  r_unexposed <- C / (C + D)
  r_overall <- (A+C) / (A + B + C + D)

  tmp_rr <- r_exposed / r_unexposed

  tmp_parp <- (r_overall - r_unexposed) / r_overall
  theta <- 1-tmp_parp
  tmp_n <- A+B+C+D
  pi_01 <- C / tmp_n

  ## calculate SE
  se_tmp_parp <-
    sqrt(
      (theta^2) * (((1-pi_01) / (tmp_n*pi_01)) - ((r_exposed+r_unexposed-2*pi_01)/(tmp_n*r_unexposed*r_exposed)))
    )

  ci_lower_bound <- tmp_parp - (critical_value*se_tmp_parp)
  ci_upper_bound <- tmp_parp + (critical_value*se_tmp_parp)

  #tmp_par <- ((tmp_rr - 1) / tmp_rr)
  if(percent) {
    tmp_parp <- tmp_parp * 100
    ci_lower_bound <- ci_lower_bound * 100
    ci_upper_bound <- ci_upper_bound * 100
  }

  ## return everything as a tibble
  dplyr::tibble(
    measure = ifelse(percent, "Population Attributable Risk Percentage", "Population Attributable Risk Proportion"),
    estimate = tmp_parp,
    ci_lower = ci_lower_bound,
    ci_upper = ci_upper_bound,
    exposure = dplyr::first(tmp_twoxtwo$tbl$exposure),
    outcome = dplyr::first(tmp_twoxtwo$tbl$outcome),
  )

}
