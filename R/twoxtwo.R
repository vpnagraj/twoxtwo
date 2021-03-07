#' Create a two-by-two table
#'
#' @param .data data frame
#' @param exposure exposure
#' @param outcome outcome
#' @param levels explicitly specify levels for the exposure and outcome as a named list; if supplied, contingency table will be oriented with respect to the sequence of levels specified
#' @param na.rm logical as to whether or not to remove NA values when constructing contingency table; default is TRUE
#' @param verbose Logical
#'
#' @return tibble
#' @importFrom rlang ":="
#' @export
twoxtwo <- function(.data, exposure, outcome, levels = NULL, na.rm = TRUE, verbose = FALSE) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)

  ## how many are NA
  n_na <-
    .data %>%
    dplyr::filter(is.na(!! quo_exposure) | is.na(!! quo_outcome)) %>%
    nrow(.)

  ## optionally remove before proceeding
  if (na.rm) {
    dat <-
      .data %>%
      dplyr::filter(!is.na(!! quo_exposure) & !is.na(!! quo_outcome))
  } else {
    dat <- .data
  }

  dat <-
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
      dat <- as.data.frame(dat)

      row.names(dat) <- dat[,1]

      # check inputs for exposure and outcome levels
      if(!(all(levels$outcome %in% colnames(dat)) & all(levels$exposure %in% rownames(dat)))) {
        stop("Make sure all levels specified exist in the exposure and/or outcome.")
      }

      # check inputs for exposure and outcome levels
      if(length(unique(levels$outcome)) == 1 | length(unique(levels$exposure)) == 1) {
        stop("Make sure all levels specified are unique to exposure or outcome.")
      }

      # this step will reorder *and get rid of outcome column (index 1)
      dat <- dat[levels$exposure,levels$outcome]

      exposure_levels <- rownames(dat)
      dat$exposure <- paste0(dplyr::quo_name(quo_exposure),
                            "::",
                            paste0(exposure_levels, collapse = "/"))

      outcome_levels <- colnames(dat)[1:2]
      dat$outcome <- paste0(dplyr::quo_name(quo_outcome),
                           "::",
                           paste0(outcome_levels, collapse = "/"))

      dat <- dplyr::as_tibble(dat)

    } else {

      exposure_levels <- dplyr::pull(dat,1)
      dat$exposure <- paste0(dplyr::quo_name(quo_exposure),
                            "::",
                            paste0(exposure_levels, collapse = "/"))

      outcome_levels <- colnames(dat)[2:3]
      dat$outcome <- paste0(dplyr::quo_name(quo_outcome),
                           "::",
                           paste0(outcome_levels, collapse = "/"))

      # otherwise get rid of outcome column (index 1) altogether
      dat <- dat[,-1]
  }

  ## set names to include name / level of exposure variable
  dat <-
    dat %>%
    magrittr::set_colnames(.,
                           c(paste0(dplyr::quo_name(quo_outcome),
                                    "_", colnames(.)[1:2]),
                             # keep expsoure outcome name
                             colnames(.)[3:4]
                           )
    ) %>%
    dplyr::as_tibble(.)

  ## construct output object
  res <- list(tbl = dat,
              exposure = list(variable = dplyr::quo_name(quo_exposure),
                              levels = exposure_levels),
              outcome = list(variable = dplyr::quo_name(quo_outcome),
                             levels = outcome_levels),
              n_missing = n_na,
              data = .data)
  ## assign the twoxtwo class
  class(res) <- "twoxtwo"

  res

}
