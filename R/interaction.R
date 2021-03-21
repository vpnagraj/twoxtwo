#' Relative excess risk due to interaction (RERI)
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}]
#'
#' This function computes the RERI, which is a metric for assessing additive interaction in relative risk. The input data is first stratified by the effect modifier variable. The function then internally uses \link[twoxtwo]{twoxtwo} to create the two-by-two contingency table and compute risk within strata. The RERI is then calculatated from these stratum-specific risks.
#'
#' The formulas used to calculate the RERI is available in 'Details'.
#'
#' @details
#'
#' \deqn{RERI = (r11/r00) - (r10/r00) - (r01/r00) + 1}
#'
#' @param .data Data frame with observation-level exposure and outcome data
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param effect_modifier Name of effect modifier variable
#' @param ... Additional arguments passed to \link[twoxtwo]{twoxtwo} function
#'
#' @return
#'
#' A `tibble` with the following columns
#'
#' - **estimate**: Value for the RERI based on exposure, outcome, and effect modifier variables specified
#' - **exposure**: Name of the exposure variable followed by +/- levels (e.g. smoking::yes/no)
#' - **outcome**: Name of the outcome variable followed by +/- levels (e.g. heart_disease::yes/no)
#' - **effect_modifier**: Name of the effect modifier followed by levels (e.g. diabetes::TRUE/FALSE)
#'
#' @references Richardson, D. B., & Kaufman, J. S. (2009). Estimation of the relative excess risk due to interaction and associated confidence bounds. American journal of epidemiology, 169(6), 756–760. https://doi.org/10.1093/aje/kwn411
#' @export
#'
#' @md
#'
reri <- function(.data, exposure, outcome, effect_modifier, ...) {

  ## handle exposure/outcome and effect modifer variable name quotation
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

  tmp1 <-
    apart[[1]] %>%
    twoxtwo(!!quo_exposure,!!quo_outcome, ...)

  strata1 <-
    tmp1$tbl %>%
    dplyr::mutate(risk = .[[1]] / rowSums(dplyr::select(.,-exposure,-outcome))) %>%
    dplyr::mutate(effect_modifier := paste0(splitter, "::", names(apart)[1]))

  tmp2 <-
    apart[[2]] %>%
    twoxtwo(!!quo_exposure,!!quo_outcome, ...)

  strata2 <-
    tmp2$tbl %>%
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

  ## format the effect modifer info for return tibble
  effect_modifier_levels <-
    ## paste everything together ...
    ## first get the name of the effect modifier variable (could use either strata1 or strata2 tbl here)
    ## second use "::" to separate
    ## third get the level of the effect modifier in strata1
    ## fourth use "/" to seperate levels
    ## fifth get the level of the effect modifier in strata2
    paste0(strsplit(strata1$effect_modifier, "::")[[1]][1],
           "::",
           strsplit(strata1$effect_modifier, "::")[[1]][2],
           "/",
           strsplit(strata2$effect_modifier, "::")[[1]][2])

  dplyr::tibble(estimate = reri,
                exposure = unique(strata1$exposure),
                outcome = unique(strata1$outcome),
                effect_modifier = effect_modifier_levels)
}
