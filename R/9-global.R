#' @importFrom grDevices terrain.colors
#' @importFrom methods new show
#' @importFrom stats dnorm rnorm runif step var
NULL

#' Package Check
#'
#' Checks whether the user has the required package installed.
#'  For back-end use only.
#'
#' @param package A character string. An R package.
packageCheck <- function(package){

  if(!requireNamespace(package, quietly = TRUE)){
    stop("Uh oh! This method depends on ", package, ".")
  }
}
