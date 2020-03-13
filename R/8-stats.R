#' Represent Reward List as Data Frame
#'
#' @param scheduler A \code{scheduler} object.
#' @return A \code{data.frame}
#' @export
rewards_as_df <- function(scheduler){

  rewards <- scheduler@rewards
  df <- lapply(1:length(rewards), function(i) data.frame("group" = i, "reward" = rewards[[i]]))
  df <- do.call("rbind", df)
  return(df)
}

#' Perform a Statistical Analysis
#'
#' @param scheduler A \code{scheduler} object.
#' @return A single p-value.
#' @export
stats.anova <- function(scheduler){

  data <- rewards_as_df(scheduler)
  m <- lm(reward ~ factor(group), data)
  pvals <- summary(aov(m))[[1]][1,5]
  return(pvals)
}

#' Perform a Statistical Analysis
#'
#' @param scheduler A \code{scheduler} object.
#' @param reference An integer. The list element with control rewards.
#' @return A vector of adjusted p-values.
#' @export
stats.ttest <- function(scheduler, reference = NULL){

  if(is.null(reference)) stop("Reference is missing.")

  data <- rewards_as_df(scheduler)
  tests <- lapply(1:length(scheduler@rewards), function(i){
    t.test(x = data[data$group == reference, "reward"],
           y = data[data$group == i, "reward"])
  })
  pvals <- sapply(tests, function(t) t$p.value)[-reference]
  pvals <- p.adjust(pvals, method = "bonferroni")
  return(pvals)
}

#' Perform a Statistical Analysis
#'
#' Note that all p-values implicitly describe a two-tailed test
#'  because divergence has no sign.
#'
#' @param scheduler A \code{scheduler} object.
#' @param reference An integer. The list element with control rewards.
#' @param how.diverge A \code{diverge} method.
#' @param how.permute A \code{permute} method.
#' @param p An integer. The number of permutations.
#' @return A vector of adjusted p-values.
#' @export
stats.empiric <- function(scheduler, reference = NULL, how.diverge = diverge.kl.sym, how.permute = permute.from.all, p = 1000){

  if(is.null(reference)) stop("Reference is missing.")

  # What are the actual divergences?
  actual <- getDivergences(scheduler, how = how.diverge)[,reference]

  # What divergences do we observe when we permute the data?
  observed <- lapply(1:p, function(permutation){
    sch.p <- getPermutations(scheduler, how = how.permute, reference = reference)
    getDivergences(sch.p, how = how.diverge)[,reference]
  })
  observed <- do.call("rbind", observed)

  # Calculate empiric p-values
  pvals <- vector("numeric", ncol(observed))
  for(dist in 1:ncol(observed)){
    pvals[dist] <- 1 - sum(actual[dist] > observed[,dist]) / (nrow(observed)+1)
  }

  # Adjust p-values
  pvals <- pvals[-reference]
  pvals <- p.adjust(pvals, method = "bonferroni")
  return(pvals)
}
