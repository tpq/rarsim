#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.thompson:} Method to allocate patients by Thompson sampling.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.thompson <- function(scheduler, ...){

  sch <- scheduler

  # Sample each posterior distribution exactly once
  sample_each_arm <- sapply(
    1:sch@K.arms,
    function(arm){
      alt_rt(1, df = sch@post.df[arm], mean = sch@post.mean[arm], sd = sqrt(sch@post.var[arm]))
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
sampler.fixed <- function(scheduler, ...){

  # Each arm has equal probability
  sample(1:scheduler@K.arms)[1]
}

#' Calculate Probability of Posterior
#'
#' This function estimates the probability that the posterior expected reward
#'  is greater than a cutoff. It does this by computing the area under the curve
#'  for a t-score at a given cutoff.
#'
#' @param scheduler A \code{scheduler} object.
#' @param cutoff A numeric. The value against which to compare the posterior.
#' @return A vector of probabilities.
#' @export
p_greater_than_cutoff <- function(scheduler, cutoff){

  1 - alt_pt(cutoff, df = scheduler@post.df,
             mean = scheduler@post.mean, sd = sqrt(scheduler@post.var))
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.auc.cutoff:} Method to allocate patients proportional to the
#'  probability that the posterior expected reward is greater than a cutoff.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.auc.cutoff <- function(scheduler, cutoff = 0, ...){

  # Weigh probability based on p(posterior > c)
  aucs <- p_greater_than_cutoff(scheduler, cutoff = cutoff)
  prob <- aucs/sum(aucs)
  sample(1:scheduler@K.arms, prob = prob)[1]
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.auc.reference:} Method to allocate patients proportional to the
#'  probability that the posterior expected reward is greater than a reference.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.auc.reference <- function(scheduler, reference = NULL, ...){

  if(is.null(reference)) stop("Reference is missing.")
  sampler.auc.cutoff(scheduler, cutoff = scheduler@post.mean[reference])
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.ucb1:} Method to allocate patients based on a UCB algorithm,
#'  assuming that the rewards fall within the range of 0 and 1.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.ucb1 <- function(scheduler, c = 2, batch = TRUE){

  ucb <- scheduler@online.mean +
    sqrt( c *
            (log(sum(scheduler@dynamic.count))) /
            (scheduler@dynamic.count)
    )

  if(batch){
    squash <- function(x) x - min(x)
    allocation <- sample(1:scheduler@K.arms, size = 1, prob = squash(ucb))
  }else{
    allocation <- which.max(ucb)
  }

  return(allocation)
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.ucb1.normal:} Method to allocate patients based on a UCB algorithm,
#'  assuming that the rewards are normally distributed.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.ucb1.normal <- function(scheduler, c = 16, batch = TRUE){

  SS <- sapply(scheduler@rewards, function(x) sum(x^2))

  ucb <- scheduler@online.mean +
    sqrt( c *
            (SS - scheduler@online.count * scheduler@online.mean^2) / # do not make dynamic or else get sqrt(-x)
            (scheduler@dynamic.count - 1) *
            (log(sum(scheduler@dynamic.count)-1)) /
            (scheduler@dynamic.count)
    )

  if(batch){
    squash <- function(x) x - min(x)
    allocation <- sample(1:scheduler@K.arms, size = 1, prob = squash(ucb))
  }else{
    allocation <- which.max(ucb)
  }

  return(allocation)
}

#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.epsilon.greedy:} Method to allocate patients based on
#'  an epsilon-greedy algorithm. By default, \code{epsilon = 0.1}.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.epsilon.greedy <- function(scheduler, epsilon = 0.1){

  cutoff <- runif(1, 0, 1)
  if(cutoff > epsilon){
    allocation <- which.max(scheduler@post.mean) # (1-gamma)% of the time, choose best
  }else{
    allocation <- sample(1:scheduler@K.arms)[1] # (gamma)% of the time, random
  }

  return(allocation)
}
