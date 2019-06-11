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
