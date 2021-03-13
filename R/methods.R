#' Title
#'
#' @param .twoxtwo
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
print.twoxtwo <- function(.twoxtwo, ...) {
  cat(display(.twoxtwo, ...), sep = "\n")
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
summary.twoxtwo <- function(.twoxtwo, alpha = 0.05, ...) {

  ## print relevant summary information
  cat("\n")
  cat(print(.twoxtwo, ...), sep = "\n")
  cat("\n")
  cat(paste0("Outcome: ", .twoxtwo$outcome$variable))
  cat("\n")
  cat(paste0("Outcome + : ", .twoxtwo$outcome$levels[1]))
  cat("\n")
  cat(paste0("Outcome - : ", .twoxtwo$outcome$levels[2]))
  cat("\n")
  cat("\n")
  cat(paste0("Exposure: ", .twoxtwo$exposure$variable))
  cat("\n")
  cat(paste0("Exposure + : ", .twoxtwo$exposure$levels[1]))
  cat("\n")
  cat(paste0("Exposure - : ", .twoxtwo$exposure$levels[2]))
  cat("\n")
  cat("\n")
  cat(paste0("Number of missing observations: ", .twoxtwo$n_missing))
  cat("\n")
  cat("\n")

  if(!is.null(.twoxtwo$data)) {
    tmp_or <- odds_ratio(.twoxtwo$data,
                         exposure = !! rlang::sym(.twoxtwo$exposure$variable),
                         outcome = !! rlang::sym(.twoxtwo$outcome$variable),
                         alpha = alpha)

    tmp_rr <- risk_ratio(.twoxtwo$data,
                         exposure = !! rlang::sym(.twoxtwo$exposure$variable),
                         outcome = !! rlang::sym(.twoxtwo$outcome$variable),
                         alpha = alpha)

    tmp_rd <- risk_diff(.twoxtwo$data,
                        exposure = !! rlang::sym(.twoxtwo$exposure$variable),
                        outcome = !! rlang::sym(.twoxtwo$outcome$variable),
                        alpha = alpha)

    cat(paste0("Odds Ratio: ", format_measure(tmp_or)))
    cat("\n")
    cat(paste0("Risk Ratio: ", format_measure(tmp_rr)))
    cat("\n")
    cat(paste0("Risk Difference: ", format_measure(tmp_rd)))
    cat("\n")
  }


  ## invisibly output a list with summarized measures
  invisible(
    list(odds_ratio = tmp_or, risk_ratio = tmp_rr, risk_difference = tmp_rd)
  )

}
