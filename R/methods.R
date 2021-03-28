#' Print twoxtwo object
#'
#' @description
#'
#' The `print.twoxtwo()` function provides an S3 method for printing objects created with \link[twoxtwo]{twoxtwo}. The printed output formats the contents of the `twoxtwo` table as a \link[knitr]{kable}.
#'
#' @param x \link[twoxtwo]{twoxtwo} object
#' @param ... Additional arguments passed to \link[knitr]{kable}
#'
#' @return A printed `knitr_kable` object with the `twoxtwo` cell counts, exposure levels as row names, and outcome levels as column names.
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
#' The `summary.twoxtwo()` function provides an S3 method for summarizing objects created with \link[twoxtwo]{twoxtwo}. The summary function prints the `twoxtwo` via \link[twoxtwo]{print.twoxtwo} along with characteristics of the contingency table such the number of missing observations and exposure/outcome variables and levels. The summary will also compute effect measures using \link[twoxtwo]{odds_ratio}, \link[twoxtwo]{risk_ratio}, and \link[twoxtwo]{risk_diff} and print the estimates and confidence interval for each.
#'
#' @param object \link[twoxtwo]{twoxtwo} object
#' @param alpha Significance level to be used for constructing confidence interval; default is `0.05`
#' @param ... Additional arguments passed to \link[twoxtwo]{print.twoxtwo}
#'
#' @return
#'
#' Printed summary information including the outcome and exposure variables and levels, as well as the number of missing observations, the `twoxtwo` contingency table, and formatted effect measures (see "Description"). In addition to printed output, the function invisibly returns a named list with computed effect measures (i.e. the `tibble` outputs from \link[twoxtwo]{odds_ratio}, \link[twoxtwo]{risk_ratio}, and \link[twoxtwo]{risk_diff} respectively).
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
  } else {
    tmp_or <- NULL
    tmp_rr <- NULL
    tmp_rd <- NULL
    message("twxotwo object created with 'retain=FALSE' ... cannot summarize effect measures.")
  }


  ## invisibly output a list with summarized measures
  invisible(
    list(odds_ratio = tmp_or, risk_ratio = tmp_rr, risk_difference = tmp_rd)
  )

}
