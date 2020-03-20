#' Structure Vector like a List
#'
#' This function structures a vector to look like a list
#'  based on a reference list.
#'
#' @param vector A vector.
#' @param list A list.
#' @return A list.
#' @export
structure_as_list <- function(vector, list){

  names(vector) <- NULL
  split_into <- unlist(lapply(1:length(list), function(i) rep(i, length(list[[i]]))))
  split_into <- factor(split_into, 1:length(list))
  vector <- split(vector, split_into)
  names(vector) <- names(list)
  return(vector)
}

#' Permute a List of Rewards
#'
#' This function does not actually permute the rewards.
#'  Used for testing purposes only.
#'
#' @param rewards A list of rewards.
#' @param reference Null argument.
#' @return A list of rewards.
#' @export
permute.from.null <- function(rewards, reference = NULL){

  p_rewards <- unlist(rewards)
  structure_as_list(p_rewards, rewards)
}

#' Permute a List of Rewards
#'
#' This function samples from any group, without replacement.
#'
#' @param rewards A list of rewards.
#' @param reference Null argument.
#' @return A list of permuted rewards.
#' @export
permute.from.all <- function(rewards, reference = NULL){

  N_samples <- length(unlist(rewards))
  p_rewards <- sample(unlist(rewards), size = N_samples, replace = FALSE)
  structure_as_list(p_rewards, rewards)
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
permute.from.control <- function(rewards, reference = NULL){

  if(is.null(reference)) stop("Reference is missing.")

  N_samples <- length(unlist(rewards))
  p_rewards <- sample(rewards[[reference]], size = N_samples, replace = TRUE)
  structure_as_list(p_rewards, rewards)
}

#' Permute a List of Rewards
#'
#' This function samples from a normal distribution with the same
#'  mean and standard deviation as the reference distribution.
#'
#' @param rewards A list of rewards.
#' @param reference An integer. The list element with control rewards.
#' @return A list of permuted rewards.
#' @export
permute.from.control.norm <- function(rewards, reference = NULL){

  if(is.null(reference)) stop("Reference is missing.")

  N_samples <- length(unlist(rewards))
  controls <- rewards[[reference]]
  p_rewards <- rnorm(N_samples, mean = mean(controls), sd = sd(controls))
  structure_as_list(p_rewards, rewards)
}

#' Permute Rewards for a Scheduler Object
#'
#' @param scheduler A \code{scheduler} object.
#' @param how A function. The method used to permute the rewards.
#' @param reference An integer. Passed to \code{how}.
#' @return A \code{scheduler} object.
#' @export
getPermutations <- function(scheduler, how = permute.from.all, reference = NULL){

  # Create a new scheduler object
  newsch <- scheduler.start(prior.mean = scheduler@prior.mean,
                            prior.var = scheduler@prior.var,
                            N.burn.in = scheduler@N.burn.in,
                            sampler = scheduler@sampler)
  newsch@sampler.args <- scheduler@sampler.args # pass sampler.args before update...

  # Permute rewards and pass to new scheduler object
  p_rewards <- do.call(how, list(rewards = scheduler@rewards, reference = reference))
  newsch@ingest <- p_rewards
  newsch <- scheduler.update(newsch, p_rewards, N.allocate = 1)
  return(newsch)
}
