#' Format measure
#'
#' @description
#'
#' This helper takes the output from a `twoxtwo` effect measure function and formats the point estimate and lower/upper bounds of the computed confidence interval (CI) as a string.
#'
#' @param .data Output from a twoxtwo effect measure function (e.g. \link[twoxtwo]{odds_ratio})
#' @param digits Number of digits; default is `3`
#'
#' @md
#'
#' @return
#'
#' A character vector of length 1 with the effect measure formatted as point estimate (lower bound of CI, upper bound of CI). The point estimate and CI are rounded to precision specified in "digits" argument.
#'
format_measure <- function(.data, digits = 3) {
  tmp_est <- .data$estimate
  tmp_lower <- .data$ci_lower
  tmp_upper <- .data$ci_upper
  paste0(round(tmp_est, digits),
         " (",
         round(tmp_lower, digits),
         ",",
         round(tmp_upper, digits),
         ")")
}


#' Display twoxtwo object
#'
#' @description
#'
#' This is a helper to render a \link[twoxtwo]{twoxtwo} object as a \link[knitr]{kable}. The function extracts `twoxtwo` cell counts and uses exposure levels as row names and outcome levels as column names.
#'
#' @param .twoxtwo \link[twoxtwo]{twoxtwo} object
#' @param ... Additional arguments passed to \link[knitr]{kable}
#'
#' @return
#'
#' A `knitr_kable` object with the `twoxtwo` cell counts, exposure levels as row names, and outcome levels as column names.
#'
#' @export
#' @md
#'
display <- function(.twoxtwo, ...) {
  ## extract exposure and outcome names / levels from twoxtwo object
  exposure_var <- .twoxtwo$exposure$variable
  exposure_levels <- .twoxtwo$exposure$levels
  exposures <- paste0(exposure_var, "=", exposure_levels)

  outcome_var <- .twoxtwo$outcome$variable
  outcome_levels <- .twoxtwo$outcome$levels
  outcomes <- paste0(outcome_var, "=", outcome_levels)

  ## stack names of outcomes with the first two columns of the twoxtwo tibble
  tmp <- rbind(outcomes, .twoxtwo$tbl[,1:2])

  tmp <-
    cbind(c("", exposures),tmp) %>%
    as.matrix(.)

  rownames(tmp) <- c("","EXPOSURE","EXPOSURE")
  names(tmp) <- NULL

  knitr::kable(tmp,
               col.names = c("","OUTCOME","OUTCOME"),
               row.names = TRUE,
               ...)
}

#' Bound a vector
#'
#' @description
#'
#' This unexported helper function bounds a numeric vector on a minimum and maximum value.
#'
#' @param x Numeric vector to be bounded
#' @param min Minimum allowed value for vector "x"; default is `0.01`
#' @param max Maximum allowed value for vector "x"; default is `0.99`
#'
#' @return Numeric vector of the same length as `x` with no values less than `minimum` nor greater than `maximum`.
#'
#' @md
#'
bound <- function(x, min = 0.01, max = 0.99) {
  x <- ifelse(x < min, min, x)
  x <- ifelse(x > max, max, x)
  return(x)
}
