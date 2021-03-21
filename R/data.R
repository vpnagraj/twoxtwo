#' Expanded Titanic dataset
#'
#' @description
#'
#' This data is based on the \link[datasets]{Titanic} dataset. Unlike the version in the `datasets` package, the data here is expanded to the observation-level rather than cross-tabulated.
#'
#' @format A data frame with 2201 rows and 4 variables:
#'
#' - **Class**: Passenger class ("1st", "2nd", "3rd") or crew status ("Crew")
#' - **Sex**: Sex of individual ("Male" or "Female")
#' - **Age**: Categorized age ("Adult" or "Child")
#' - **Survived**: Whether or not individual survived ("Yes" or "No")
#'
#' @md
#' @examples
#' head(titanic)
"titanic"
