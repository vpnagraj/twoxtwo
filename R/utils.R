#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Expand aggregated count data to observation-level
#'
#' @param .data data frame
#' @param freq_col column containing the frequency values
#'
#' @return tibble
#' @export
expand_counts <- function(.data, freq_col) {

  quo_freq <- dplyr::enquo(freq_col)

  freqs <- dplyr::pull(.data, !!quo_freq)

  ind <- rep(seq_len(nrow(.data)), freqs)

  # Drop count column
  .data <- dplyr::select(.data, - !!quo_freq)

  # Get the rows from x
  .data[ind, ]

}


#' Title
#'
#' @param .data
#' @param digits
#'
#' @return
#'
format_measure <- function(.data, digits = 3) {
  tmp_meas <- unname(unlist(.data[1,1]))
  tmp_lower <- .data$ci_lower
  tmp_upper <- .data$ci_upper
  paste0(round(tmp_meas, digits),
         " (",
         round(tmp_lower, digits),
         ",",
         round(tmp_upper, digits),
         ")")
}


#' Title
#'
#' @param .twoxtwo
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

