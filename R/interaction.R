#' Relative risk due to interaction (RERI)
#'
#' @param .data
#' @param exposure
#' @param outcome
#' @param effect_modifier
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
reri <- function(.data, exposure, outcome, effect_modifier, ...) {

  quo_exposure <- dplyr::enquo(exposure)
  quo_outcome <- dplyr::enquo(outcome)
  quo_effect_modifier <- dplyr::enquo(effect_modifier)

  combined <-
    .data %>%
    dplyr::mutate(effect_modifier := forcats::fct_rev(as.factor(!!quo_effect_modifier)))

  # why doesn't enquo work for this?
  # splitter <- as.character(quo_effect_modifier)
  splitter <- as.character(substitute(effect_modifier))

  apart <- split(combined, combined[,splitter])

  strata1 <-
    apart[[1]] %>%
    twoxtwo(!!quo_exposure,!!quo_outcome)$tbl %>%
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    dplyr::mutate(effect_modifier := paste0(splitter, "::", names(apart)[1]))


  strata2 <-
    apart[[2]] %>%
    twoxtwo(!!quo_exposure,!!quo_outcome)$tbl %>%
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    dplyr::mutate(effect_modifier := paste0(splitter, "::", names(apart)[2]))

  rstrata1 <-
    strata1 %>%
    dplyr::pull(risk)

  rstrata2 <-
    strata2 %>%
    dplyr::pull(risk)

  r11 <- rstrata2[1]
  r01 <- rstrata2[2]
  r10 <- rstrata1[1]
  r00 <- rstrata1[2]

  reri <- (r11/r00) - (r10/r00) - (r01/r00) + 1

  dplyr::tibble(reri = reri)

}
