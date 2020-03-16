#' S4 Scheduler Class Object
#'
#' @slot prior.mean,prior.var,prior.prec The prior statistics for each arm.
#' @slot N.burn.in An integer. The number of patients in each arm at first time step.
#' @slot K.arms An integer. The number of experimental groups (also called 'arms').
#' @slot step An integer. The number of time steps so far. Each time step is a
#'  kind of 'mini-trial'. This initializes at 0.
#' @slot rewards A list of all rewards observed to date.
#' @slot meta A list of tables for all rewards and meta-data available.
#'  This is optional, but may be preferred for complex simulations.
#' @slot online.count,online.sum,online.mean,online.var,online.prec
#'  The empiric statistics observed to date.
#' @slot post.mean,post.var,post.prec The posterior statistics for each arm.
#' @slot sampler.id A string. The sampling method used to allocate patients.
#' @slot sampler A function. The sampling method used to allocate patients.
#' @slot allocation A vector of groups to which to allocate new patients.
#' @slot ingest A list describing the structure of the data expected
#'  by the next \code{scheduler.update} call. This slot is only
#'  used to verify the incoming data.
#' @slot allocation The most recent patient allocations. This slot
#'  implies the structure of the \code{@@ingest} slot.
#' @slot history.post A table of all posterior statistics and the allocation
#'  ratios at each time step in the experiment. At \code{@@step = 0},
#'  the table records the prior statistics and burn-in conditions.
#' @slot history A list of lists of all rewards. Unlike \code{@@rewards},
#'  the rewards here are organized by time step.
#'
#' @param object,scheduler A \code{scheduler} object.
#' @param prior.mean A vector of prior means. One mean for each arm
#' @param prior.var A vector of prior variances. One variance for each arm.
#' @param N.burn.in An integer. The number patients to allocate each arm
#'  during the initial 'burn-in' phase. Ideally, around 20-30 per arm.
#' @param sampler A function. The sampling method used to allocate patients.
#' @param data.ingest A list of rewards. Must match the structure of \code{scheduler@@ingest}.
#' @param N.allocate An integer. The total number of patients to allocate next.
#'
#' @name scheduler
NULL

#' @rdname scheduler
#' @export
setClass("scheduler",
         slots =
           c(
             prior.mean = "numeric",
             prior.var = "numeric",
             prior.prec = "numeric",
             N.burn.in = "numeric",
             K.arms = "numeric",
             step = "numeric",
             rewards = "list",
             meta = "list",
             online.count = "numeric",
             online.sum = "numeric",
             online.mean = "numeric",
             online.var = "numeric",
             online.prec = "numeric",
             post.mean = "numeric",
             post.var = "numeric",
             post.prec = "numeric",
             sampler.id = "character",
             sampler = "function",
             allocation = "numeric",
             ingest = "list",
             history.post = "data.frame",
             history = "list"
           )
)

#' @rdname scheduler
#' @section Methods:
#' \code{show:} Method to show \code{scheduler} object.
#' @export
setMethod("show", "scheduler",
          function(object){

            print("## Experimental Design")
            print(paste0("Number of arms: ", object@K.arms))
            print(paste0("Burn-in size: ", object@N.burn.in))
            print(paste0("Prior means: ", paste(object@prior.mean, collapse = ", ")))
            print(paste0("Prior variances: ", paste(object@prior.var, collapse = ", ")))
            print(paste0("Current step: ", object@step))
            print(paste0("Posterior means: ", paste(object@post.mean, collapse = ", ")))
            print(paste0("Posterior variances: ", paste(object@post.var, collapse = ", ")))
            #print(paste0("Sampler: ", object@sampler))
            cat("Next Allocation:")
            print(table(object@allocation))

            print("## Available Methods")
            print("scheduler.update() -- update scheduler with new data")
            print("getAllocation() -- get allocation vector for next time step")
            print("getHistory() -- get history of all posterior statistics")
          }
)

#' @rdname scheduler
#' @section Getters:
#' \code{getAllocation:} Method to retrieve allocations from a \code{scheduler} object.
#'  This function returns a vector of groups to which to allocate new patients.
#' @export
getAllocation <- function(object){

  return(object@allocation)
}

#' @rdname scheduler
#' @section Getters:
#' \code{getHistory:} Method to retrieve history from a \code{scheduler} object.
#'  This function returns a table of all posterior statistics and the allocation
#'  ratios at each time step in the experiment.
#' @export
getHistory <- function(object){

  return(object@history.post)
}
