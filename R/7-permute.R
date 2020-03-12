#' Permute a List of Rewards
#'
#' This function does not actually permute the rewards.
#'  Used for testing purposes only.
#'
#' @param rewards A list of rewards.
#' @param reference Null argument.
#' @return A list of rewards.
#' @export
permute.null <- function(rewards, reference = NULL){

  p_rewards <- unlist(rewards)
  names(p_rewards) <- NULL

  split_into <- unlist(lapply(1:length(rewards), function(i) rep(i, length(rewards[[i]]))))
  split_into <- factor(split_into, 1:length(rewards))
  p_rewards <- split(p_rewards, split_into)
  names(p_rewards) <- NULL
  return(p_rewards)
}

#' Permute a List of Rewards
#'
#' This function samples from any group, without replacement.
#'
#' @param rewards A list of rewards.
#' @param reference Null argument.
#' @return A list of permuted rewards.
#' @export
permute.all <- function(rewards, reference = NULL){

  N_samples <- length(unlist(rewards))
  p_rewards <- sample(unlist(rewards), size = N_samples, replace = FALSE)
  names(p_rewards) <- NULL

  split_into <- unlist(lapply(1:length(rewards), function(i) rep(i, length(rewards[[i]]))))
  split_into <- factor(split_into, 1:length(rewards))
  p_rewards <- split(p_rewards, split_into)
  names(p_rewards) <- NULL
  return(p_rewards)
}

#' Permute a List of Rewards
#'
#' This function samples from the control group only, with replacement.
#'  The user must select the control group.
#'
#' @param rewards A list of rewards.
#' @param reference An integer. The list element with control rewards.
#' @return A list of permuted rewards.
#' @export
permute.control <- function(rewards, reference = NULL){

  if(is.null(reference)) stop("Reference is missing.")

  N_samples <- length(unlist(rewards))
  p_rewards <- sample(rewards[[reference]], size = N_samples, replace = TRUE)
  names(p_rewards) <- NULL

  split_into <- unlist(lapply(1:length(rewards), function(i) rep(i, length(rewards[[i]]))))
  split_into <- factor(split_into, 1:length(rewards))
  p_rewards <- split(p_rewards, split_into)
  names(p_rewards) <- NULL
  return(p_rewards)
}

#' Permute Rewards for a Scheduler Object
#'
#' @param scheduler A \code{scheduler} object.
#' @param how A function. The method used to permute the rewards.
#' @param reference An integer. Passed to \code{how}.
#' @return A \code{scheduler} object.
#' @export
getPermutations <- function(scheduler, how = permute.all, reference = NULL){

  # Create a new scheduler object
  newsch <- scheduler.start(prior.mean = scheduler@prior.mean,
                            prior.var = scheduler@prior.var,
                            N.burn.in = scheduler@N.burn.in,
                            sampler = scheduler@sampler)

  # Permute rewards and pass to new scheduler object
  p_rewards <- do.call(how, list(rewards = scheduler@rewards, reference = reference))
  newsch@ingest <- p_rewards
  newsch <- scheduler.update(newsch, p_rewards, N.allocate = 1)
  return(newsch)
}
