#' twoxtwo
#'
#' @description
#'
#' Provides a collection of functions for data analysis with two-by-two contingency tables.
#'
#' @name twoxtwo-package
#' @aliases twoxtwo-package
#' @docType package
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "n",
                                                        "risk",
                                                        "ci_lower",
                                                        "ci_upper",
                                                        "estimate",
                                                        "test",
                                                        "pvalue"))
