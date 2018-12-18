#' Create a two-by-two table
#'
#' @param df data frame
#' @param exposure exposure
#' @param outcome outcome
#' @param levels explicitly specify levels for the exposure and outcome as a named list; if supplied, contingency table will be oriented with respect to the sequence of levels specified
#' @param na.rm logical as to whether or not to remove NA values when constructing contingency table; default is TRUE
#'
#' @return tibble
#' @importFrom rlang ":="
#' @export
twoxtwo <- function(df, exposure, outcome, levels = NULL, na.rm = TRUE) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  if (na.rm) {

    df <-
      df %>%
      dplyr::filter(!is.na(!! quo_exposure) & !is.na(!! quo_outcome))

  }

  df <-
    df %>%
    dplyr::count(!! quo_exposure, !! quo_outcome) %>%
    # use ':=' to unquote left and right side
    dplyr::mutate(!! quo_exposure := forcats::fct_rev(as.factor(!! quo_exposure)),
                  !! quo_outcome := forcats::fct_rev(as.factor(!! quo_outcome))) %>%
    tidyr::spread(!! quo_outcome, n)

    if(!is.null(levels)) {

      # if reordering of outcome and exposure for 2x2 is desired, convert to data.frame ...
      # then take values from named list levels for columnames and rownames
      df <- as.data.frame(df)

      row.names(df) <- df[,1]

      # check inputs for exposure and outcome levels
      if(!(all(levels$outcome %in% colnames(df)) & all(levels$exposure %in% rownames(df)))) {
        stop("One or more of the levels you've specified in levels does not exist in the exposure and/or outcome.")
      }

      # this step will reorder *and get rid of outcome column (index 1)
      df <- df[levels$exposure,levels$outcome]

      df <- dplyr::as_tibble(df)

    } else {

      # otherwise get rid of outcome column (index 1)altogether
      df <- df[,-1]
  }

  # set names to include name / level of exposure variable
  df %>%
    magrittr::set_colnames(.,
                           paste0(dplyr::quo_name(quo_outcome),
                                  "_", colnames(.)
                                  )
                           )

}
