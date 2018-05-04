#' Create a two-by-two table
#'
#' @param df data frame
#' @param exposure exposure
#' @param outcome outcome
#' @param vals values to keep; passed as a named list; contingency table will be oriented in sequence of values
#' @param na.rm logical as to whether or not to remove NA values when constructing contingency table; default is TRUE
#'
#' @return tibble
#' @importFrom rlang ":="
#' @export
twoxtwo <- function(df, exposure, outcome, vals = NULL, na.rm = TRUE) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  if (na.rm) {

    df <-
      df %>%
      dplyr::filter(!is.na(!! quo_exposure) & !is.na(!! quo_outcome))

  }

  df <-
    df %>%
    dplyr::group_by(!! quo_exposure,
                    !! quo_outcome) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    tidyr::spread(!! quo_exposure, n) %>%
    dplyr::select(-1)

  if(!is.null(vals)) {

    df[vals$exposure,vals$outcome]

  } else {

    df

  }

}
