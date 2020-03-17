#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.thompson:} Method to allocate patients by Thompson sampling.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.thompson <- function(scheduler){

  sch <- scheduler

  # Sample each posterior distribution exactly once
  sample_each_arm <- sapply(
    1:sch@K.arms,
    function(arm){
      rnorm(1, mean = sch@post.mean[arm], sd = sqrt(sch@post.var[arm]))
    })

  # Choose allocation group based on the max reward sampled
  allocation <- which.max(sample_each_arm)

  return(allocation)
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.fixed:} Method to allocate patients at a fixed ratio.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.fixed <- function(scheduler){

  # Each arm has equal probability
  sample(1:scheduler@K.arms)[1]
}

#' Calculate Probability of Posterior
#'
#' This function estimates the probability that the posterior expected reward
#'  is greater than a cutoff. It does this by computing the area under the curve
#'  for a z-score at a given cutoff.
#'
#' @param scheduler A \code{scheduler} object.
#' @param cutoff A numeric. The value against which to compare the posterior.
#' @return A vector of probabilities.
#' @export
p_greater_than_cutoff <- function(scheduler, cutoff){

  ms <- scheduler@post.mean
  ss <- sqrt(scheduler@post.var)
  z <- (ms - cutoff)/ss
  p <- pnorm(z)
  return(p)
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.auc:} Method to allocate patients proportional to the
#'  probability that the posterior expected reward is greater than 0.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.auc <- function(scheduler){

  # Weigh probability based on p(posterior > 0)
  aucs <- p_greater_than_cutoff(scheduler, cutoff = 0)
  prob <- aucs/sum(aucs)
  sample(1:scheduler@K.arms, prob = prob)[1]
}
