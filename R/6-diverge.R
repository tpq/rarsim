#' Calculate Divergence between Distributions
#'
#' @param m1,m2 The means of the distributions.
#' @param s1,s2 The standard deviations of the distributions.
#' @return The divergence.
#' @export
diverge.kl <- function(m1, s1, m2, s2){

  log(s2/s1) + (s1^2 + (m1-m2)^2) / (2*s2^2) - (1/2)
}

#' Calculate Divergence between Distributions
#'
#' @param m1,m2 The means of the distributions.
#' @param s1,s2 The standard deviations of the distributions.
#' @return The divergence.
#' @export
diverge.kl.sym <- function(m1, s1, m2, s2){

  kl1 <- diverge.kl(m1, s1, m2, s2)
  kl2 <- diverge.kl(m2, s2, m1, s1)
  (kl1+kl2)/2
}

#' Get Divergences for a Scheduler Object
#'
#' @param scheduler A \code{scheduler} object.
#' @param how A function. The method used to calculate divergence.
#' @return A matrix of divergences.
#' @export
getDivergences <- function(scheduler, how = diverge.kl.sym){

  ms <- scheduler@post.mean
  ss <- sqrt(scheduler@post.var)
  res <- matrix(0, length(ms), length(ms))
  for(i in 1:length(ms)){
    for(j in 1:length(ms)){
      res[i,j] <- do.call(how, list(ms[i], ss[i], ms[j], ss[j]))
    }
  }

  return(res)
}
