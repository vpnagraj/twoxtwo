#' Print twoxtwo object
#'
#' @description
#'
#' description here ...
#'
#' @param x \link[twoxtwo]{twoxtwo} object
#' @param ... Additional arguments passed to \link[knitr]{kable}
#'
#' @return print output ...
#' @export
#'
#' @md
#'
print.twoxtwo <- function(x, ...) {
  cat(display(x, ...), sep = "\n")
}

#' Summarize twoxtwo object
#'
#' @description
#'
#' description ...
#'
#' @param object \link[twoxtwo]{twoxtwo} object
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param ... Additional arguments passed to \link[twoxtwo]{print.twoxtwo}
#'
#' @return summary object
#'
#' @export
#'
#' @md
#'
summary.twoxtwo <- function(object, alpha = 0.05, ...) {

  ## print relevant summary information
  cat("\n")
  cat(print(object, ...), sep = "\n")
  cat("\n")
  cat(paste0("Outcome: ", object$outcome$variable))
  cat("\n")
  cat(paste0("Outcome + : ", object$outcome$levels[1]))
  cat("\n")
  cat(paste0("Outcome - : ", object$outcome$levels[2]))
  cat("\n")
  cat("\n")
  cat(paste0("Exposure: ", object$exposure$variable))
  cat("\n")
  cat(paste0("Exposure + : ", object$exposure$levels[1]))
  cat("\n")
  cat(paste0("Exposure - : ", object$exposure$levels[2]))
  cat("\n")
  cat("\n")
  cat(paste0("Number of missing observations: ", object$n_missing))
  cat("\n")
  cat("\n")

  if(!is.null(object$data)) {
    tmp_or <- odds_ratio(object$data,
                         exposure = !! rlang::sym(object$exposure$variable),
                         outcome = !! rlang::sym(object$outcome$variable),
                         alpha = alpha)

    tmp_rr <- risk_ratio(object$data,
                         exposure = !! rlang::sym(object$exposure$variable),
                         outcome = !! rlang::sym(object$outcome$variable),
                         alpha = alpha)

    tmp_rd <- risk_diff(object$data,
                        exposure = !! rlang::sym(object$exposure$variable),
                        outcome = !! rlang::sym(object$outcome$variable),
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
