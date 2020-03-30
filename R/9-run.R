#' Run a Simulated Trial
#'
#' @param scheduler A \code{scheduler} object.
#' @param simulator A \code{simulator} object.
#' @param N.trials An integer. The total number of times to allocate patients.
#' @param N.allocate An integer. The total number of patients to allocate next.
#'  By default, \code{N.allocate} is set equal to \code{N.burn.in}.
#' @return A \code{scheduler} object.
#' @export
run.trial <- function(scheduler, simulator, N.trials = 10, N.allocate = scheduler@N.burn.in){

  for(trial in 1:N.trials){
    simulator <- simulator.draw(simulator, getAllocation(scheduler))
    scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = N.allocate)
  }

  return(scheduler)
}

#' Establish the Positive Rate
#'
#' @inheritParams run.trial
#' @param alpha The Type I error to control.
#' @param repititions An integer. The number of times to repeat the trial.
#' @param fast If TRUE, measure p-value at final time step only.
#' @param how.stats A string. The statistical method used to compute a p-value.
#' @param ... Named arguments passed to \code{how.stats} method.
#' @return A \code{data.frame} of false positive rates.
#' @export
run.benchmark <- function(scheduler, simulator, N.trials = 10, N.allocate = scheduler@N.burn.in,
                          alpha = 0.05, repititions = 1000, fast = FALSE, how.stats = "stats.empiric", ...){

  if(!identical(class(how.stats), "character")){
    stop("Provide 'how.stats' argument as a character.")
  }

  # Run the N.trials trial a bunch of times
  pvals <- vector("list", repititions)
  entropies <- vector("list", repititions)
  histories <- vector("list", repititions)
  for(r in 1:repititions){

    # Make a copy of the scheduler and simulator
    numTicks <- progress(r, repititions, numTicks)
    sch.r <- scheduler
    sim.r <- simulator

    # Estimate p-value at each time step
    catch_pval_at_step <- vector("list", N.trials)
    for(trial in 1:N.trials){

      # Draw and update
      sim.r <- simulator.draw(sim.r, getAllocation(sch.r))
      sch.r <- scheduler.update(sch.r, getDraw(sim.r), N.allocate = N.allocate)

      # Save p-value
      if(!fast | trial == N.trials){
        pval <- do.call(how.stats, list(sch.r, ...))
        catch_pval_at_step[[trial]] <- data.frame("step" = trial, "arm" = t(pval))
      }else{
        catch_pval_at_step[[trial]] <- data.frame("step" = trial)
      }
    }

    # Save results as data.frame
    pvals[[r]] <- do.call(plyr::rbind.fill, catch_pval_at_step)
    entropies[[r]] <- getEntropy(sch.r)
    histories[[r]] <- sch.r@history.post
  }

  # Calculate how often (p < alpha) for at least one trial arm
  df <- do.call("rbind", pvals)
  df$min_of_all_arms <- apply(df[,-1,drop=FALSE], 1, min)
  df <- aggregate(. ~ step, df, FUN = function(x) sum(x < alpha)/length(x))

  # Calculate average entropy at each time step
  ent <- do.call("rbind", entropies)
  ent <- aggregate(entropy ~ step, ent, mean)
  colnames(ent) <- c("step", "avg_entropy")
  df <- merge(df, ent, by.x = "step", by.y = "step", all.x = TRUE, all.y = TRUE)

  # Calculate average allocation count per arm
  hist <- do.call("rbind", histories)
  hist <- aggregate(total ~ step + arm, hist, mean)
  hist <- long2wide(total ~ step + arm, hist)
  colnames(hist) <- c("step", paste0("avg_", colnames(hist)[-1]))
  df <- merge(df, hist, by.x = "step", by.y = "step", all.x = TRUE, all.y = TRUE)

  # Return a data.frame with one row
  data.frame(
    "sampler" = scheduler@sampler,
    "K.arms" = scheduler@K.arms,
    "N.burn.in" = scheduler@N.burn.in,
    "N.allocate" = N.allocate,
    "N.trials" = N.trials,
    "repititions" = repititions,
    "how.stats" = how.stats,
    df
  )
}
