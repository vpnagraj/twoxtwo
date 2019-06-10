#' Create a two-by-two table
#'
#' @param .data data frame
#' @param exposure exposure
#' @param outcome outcome
#' @param levels explicitly specify levels for the exposure and outcome as a named list; if supplied, contingency table will be oriented with respect to the sequence of levels specified
#' @param na.rm logical as to whether or not to remove NA values when constructing contingency table; default is TRUE
#'
#' @return tibble
#' @importFrom rlang ":="
#' @export
twoxtwo <- function(.data, exposure, outcome, levels = NULL, na.rm = TRUE) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  if (na.rm) {

    .data <-
      .data %>%
      dplyr::filter(!is.na(!! quo_exposure) & !is.na(!! quo_outcome))

  }

  .data <-
    .data %>%
    dplyr::count(!! quo_exposure, !! quo_outcome) %>%
    # use ':=' to unquote left and right side
    dplyr::mutate(!! quo_exposure := forcats::fct_rev(as.factor(!! quo_exposure)),
                  !! quo_outcome := forcats::fct_rev(as.factor(!! quo_outcome))) %>%
    tidyr::spread(!! quo_outcome, n) %>%
    dplyr::mutate(!! quo_exposure := as.character(!! quo_exposure))

    if(!is.null(levels)) {

      # if reordering of outcome and exposure for 2x2 is desired, convert to data.frame ...
      # then take values from named list levels for columnames and rownames
      .data <- as.data.frame(.data)

      row.names(.data) <- .data[,1]

      # check inputs for exposure and outcome levels
      if(!(all(levels$outcome %in% colnames(.data)) & all(levels$exposure %in% rownames(.data)))) {
        stop("Make sure all levels specified exist in the exposure and/or outcome.")
      }

      # check inputs for exposure and outcome levels
      if(length(unique(levels$outcome)) == 1 | length(unique(levels$exposure)) == 1) {
        stop("Make sure all levels specified are unique to exposure or outcome.")
      }

      # this step will reorder *and get rid of outcome column (index 1)
      .data <- .data[levels$exposure,levels$outcome]

      .data$exposure <- paste0(dplyr::quo_name(quo_exposure),
                            "::",
                            paste0(rownames(.data), collapse = "/"))

      .data$outcome <- paste0(dplyr::quo_name(quo_outcome),
                           "::",
                           paste0(colnames(.data)[1:2], collapse = "/"))

      .data <- dplyr::as_tibble(.data)

    } else {

      .data$exposure <- paste0(dplyr::quo_name(quo_exposure),
                            "::",
                            paste0(dplyr::pull(.data,1), collapse = "/"))

      .data$outcome <- paste0(dplyr::quo_name(quo_outcome),
                           "::",
                           paste0(colnames(.data)[2:3], collapse = "/"))

      # otherwise get rid of outcome column (index 1) altogether
      .data <- .data[,-1]
  }

  # set names to include name / level of exposure variable
  .data %>%
    magrittr::set_colnames(.,
                           c(paste0(dplyr::quo_name(quo_outcome),
                                    "_", colnames(.)[1:2]),
                             # keep expsoure outcome name
                             colnames(.)[3:4]
                           )
    )

}
