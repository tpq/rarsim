#' @importFrom grDevices terrain.colors
#' @importFrom methods new show
#' @importFrom plyr rbind.fill
#' @import stats
NULL

#' Package Check
#'
#' Checks whether the user has the required package installed.
#'  For back-end use only.
#'
#' @param package A character string. An R package.
#' @export
packageCheck <- function(package){

  if(!requireNamespace(package, quietly = TRUE)){
    stop("Uh oh! This method depends on ", package, ".")
  }
}

#' Make Progress Bar
#'
#' @param i The current iteration.
#' @param k Total iterations.
#' @param numTicks The result of \code{progress}.
#' @return The next \code{numTicks} argument.
#' @export
progress <- function(i, k, numTicks){

  if(i == 1) numTicks <- 0

  if(numTicks == 0) cat("|-")

  while(i > numTicks*(k/40)){

    cat("-")
    if(numTicks == 10) cat("(25%)")
    if(numTicks == 20) cat("(50%)")
    if(numTicks == 30) cat("(75%)")
    numTicks <- numTicks + 1
  }

  if(i == k) cat("-|\n")

  return(numTicks)
}

#' Alternative Functions
#'
#' Used for unit tests.
#'
#' @param n,mean,sd See \code{norm}.
#' @return See \code{norm}.
#' @export
alt_rnorm <- function(n, mean = 0, sd = 1){

  sd*rnorm(n, mean = 0, sd = 1) + mean
}

#' Alternative Functions
#'
#' @param q,mean,sd See \code{norm}.
#' @return See \code{norm}.
#' @export
alt_pnorm <- function(q, mean = 0, sd = 1){

  pnorm((q - mean)/sd, mean = 0, sd = 1)
}

#' Alternative Functions
#'
#' Used for unit tests.
#'
#' @param p,mean,sd See \code{norm}.
#' @return See \code{norm}.
#' @export
alt_qnorm <- function(p, mean = 0, sd = 1){

  sd*qnorm(p, mean = 0, sd = 1) + mean
}

#' Alternative Functions
#'
#' Used for unit tests.
#'
#' @param x,mean,sd See \code{norm}.
#' @return See \code{norm}.
#' @export
alt_dnorm <- function(x, mean = 0, sd = 1){

  dnorm(qnorm(pnorm(x, mean = mean, sd = sd)))
}

#' Alternative Functions
#'
#' Accesses a t-distribution based on the degrees-of-freedom,
#'  mean, and standard deviation.
#'
#' @param n,df,mean,sd See \code{t}.
#' @return See \code{t}.
#' @export
alt_rt <- function(n, df, mean = 0, sd = 1){

  sd*rt(n, df) + mean
}

#' Alternative Functions
#'
#' Accesses a t-distribution based on the degrees-of-freedom,
#'  mean, and standard deviation.
#'
#' @param q,df,mean,sd See \code{t}.
#' @return See \code{t}.
#' @export
alt_pt <- function(q, df, mean = 0, sd = 1){

  pt((q - mean)/sd, df = df)
}

#' Alternative Functions
#'
#' Accesses a t-distribution based on the degrees-of-freedom,
#'  mean, and standard deviation.
#'
#' @param p,df,mean,sd See \code{t}.
#' @return See \code{t}.
#' @export
alt_qt <- function(p, df, mean = 0, sd = 1){

  sd*qt(p, df) + mean
}

#' Alternative Functions
#'
#' Accesses a t-distribution based on the degrees-of-freedom,
#'  mean, and standard deviation.
#'
#' @param x,df,mean,sd See \code{t}.
#' @return See \code{t}.
#' @export
alt_dt <- function(x, df, mean = 0, sd = 1){

  dt(qt(alt_pt(x, df = df, mean = mean, sd = sd), df = df), df = df)
}
