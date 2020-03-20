#' S4 Scheduler Class Object
#'
#' @slot prior.df The degrees-of-freedom for the t-distribution that describes the
#'  uncertainty about the mean. Set to Inf when \code{heuristic = TRUE}.
#' @slot prior.mean The prior statistics for each arm.
#' @slot prior.var The uncertainty about the mean. When the
#'  conjugate prior distribution is normal-gamma, this slot contains the
#'  variance of the marginal distribution of the mean.
#' @slot prior.nu,prior.alpha,prior.beta The prior statistics for each arm.
#'  These apply when the conjugate prior distribution is normal-gamma.
#' @slot N.burn.in An integer. The number of patients in each arm at first time step.
#' @slot K.arms An integer. The number of experimental groups (also called 'arms').
#' @slot step An integer. The number of time steps so far. Each time step is a
#'  kind of 'mini-trial'. This initializes at 0.
#' @slot rewards A list of all rewards observed to date.
#' @slot meta A list of tables for all rewards and meta-data available.
#'  This is optional, but may be preferred for complex simulations.
#' @slot online.count,online.sum,online.mean,online.var,online.prec
#'  The empiric statistics observed to date.
#' @slot heuristic A logical. Toggles whether to assume precision is known,
#'  but instead estimate it from the data. When taking this approach,
#'  the conjugate prior distribution is normal.
#' @slot post.df The degrees-of-freedom for the t-distribution that describes the
#'  uncertainty about the mean. Set to Inf when \code{heuristic = TRUE}.
#' @slot post.mean The posterior statistics for each arm.
#' @slot post.var The uncertainty about the mean. When the
#'  conjugate prior distribution is normal-gamma, this slot contains the
#'  variance of the marginal distribution of the mean.
#' @slot post.nu,post.alpha,post.beta The posterior statistics for each arm.
#'  These apply when the conjugate prior distribution is normal-gamma.
#' @slot sampler.id A string. The sampling method used to allocate patients.
#' @slot sampler.args A list. Arguments for the sampling method.
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
#' @param heuristic A logical. Toggles whether to assume precision is known,
#'  but instead estimate it from the data. When taking this approach,
#'  the conjugate prior distribution is normal.
#' @param prior.nu,prior.alpha,prior.beta The prior statistics for each arm.
#'  These apply when the conjugate prior distribution is normal-gamma
#'  (i.e., when \code{heuristic = TRUE}).
#' @param N.burn.in An integer. The number patients to allocate each arm
#'  during the initial 'burn-in' phase. Ideally, around 20-30 per arm.
#' @param sampler A function. The sampling method used to allocate patients.
#' @param data.ingest A list of rewards. Must match the structure of \code{scheduler@@ingest}.
#' @param N.allocate An integer. The total number of patients to allocate next.
#' @param cutoff The cutoff used for \code{sampler.auc.cutoff}.
#' @param reference The reference used for \code{sampler.auc.reference}.
#' @param ... Arguments passed to \code{sampler} function.
#'
#' @name scheduler
NULL

#' @rdname scheduler
#' @export
setClass("scheduler",
         slots =
           c(
             prior.df = "numeric",
             prior.mean = "numeric",
             prior.var = "numeric",
             prior.nu = "numeric",
             prior.alpha = "numeric",
             prior.beta = "numeric",

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

             heuristic = "logical",
             post.df = "numeric",
             post.mean = "numeric",
             post.var = "numeric",
             post.nu = "numeric",
             post.alpha = "numeric",
             post.beta = "numeric",

             sampler.id = "character",
             sampler.args = "list",
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
