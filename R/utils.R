#' Title
#'
#' @param .data Output from a twoxtwo effect measure function (e.g. \link[twoxtwo]{odds_ratio})
#' @param digits Number of digits; default is `3`
#'
#' @md
#'
#' @return formatted measure
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


#' Title
#'
#' @param .twoxtwo \link[twoxtwo]{twoxtwo} object
#' @param ... Additional arguments passed to \link[knitr]{kable}
#'
#' @return kable output ...
#' @export
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

