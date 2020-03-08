#' S4 Simulator Class Object
#'
#' @slot pool A list of vectors for all rewards available.
#' @slot meta A list of tables for all rewards and meta-data available.
#'  This is optional, but may be preferred for complex simulations.
#' @slot draw The data most recently sampled from the pool.
#'
#' @param object,simulator A \code{simulator} object.
#' @param allocation A vector of groups to which to allocate new patients.
#' @param pool A list of vectors for all rewards available.
#' @param means A vector of average rewards, one for each group.
#' @param variances A vector of reward variances, one for each group.
#' @param N An integer. The size of each group within the pool.
#' @param mins A vector of minimums, one for each group.
#' @param maxes A vector of maximums, one for each group.
#'
#' @name simulator
NULL

#' @rdname simulator
#' @export
setClass("simulator",
         slots =
           c(
             pool = "list",
             meta = "list",
             draw = "list"
           )
)

#' @rdname simulator
#' @section Methods:
#' \code{show:} Method to show \code{simulator} object.
#' @export
setMethod("show", "simulator",
          function(object){

            print("Size of pool:")
            print(sapply(object@pool, length))
            print("Group means:")
            print(sapply(object@pool, mean))
            print("Group variances:")
            print(sapply(object@pool, var))
            print("Size of draw:")
            print(sapply(object@draw, length))
          }
)

#' @rdname simulator
#' @section Getters:
#' \code{getPool:} Method to retrieve the data pool from a \code{simulator} object.
#'  This function returns a list of vectors for all rewards available.
#' @export
getPool <- function(simulator){

  return(simulator@pool)
}

#' @rdname simulator
#' @section Getters:
#' \code{getDraw:} Method to retrieve the previous draw from a \code{simulator} object.
#'  This function returns the data most recently sampled from the pool.
#' @export
getDraw <- function(simulator){

  return(simulator@draw)
}

#' @rdname simulator
#' @section Setters:
#' \code{simulator.start:} Method to initialize a \code{simulator} object.
#'  This function returns a \code{simulator} object.
#' @export
simulator.start <- function(pool){


  simulator <- new("simulator")
  simulator@pool <- pool
  simulator@meta <- vector("list", length(pool))
  simulator@draw <- vector("list", length(pool))
  message("Alert: Use simulator.draw() to draw patients.")
  return(simulator)
}

#' @rdname simulator
#' @section Setters:
#' \code{simulator.start.from.norm:} Method to initialize a \code{simulator} object.
#'  This function returns a \code{simulator} object.
#' @export
simulator.start.from.norm <- function(means, variances, N = 1000){

  if(!identical(length(means), length(variances))){
    stop("Provide an equal-length vector of means and variances.")
  }

  pool <- vector("list", length(means))
  for(grp in 1:length(pool)){
    pool[[grp]] <- rnorm(N, mean = means[grp], sd = sqrt(variances[grp]))
  }

  simulator.start(pool)
}

#' @rdname simulator
#' @section Setters:
#' \code{simulator.start.from.unif:} Method to initialize a \code{simulator} object.
#'  This function returns a \code{simulator} object.
#' @export
simulator.start.from.unif <- function(mins, maxes, N = 1000){

  if(!identical(length(mins), length(maxes))){
    stop("Provide an equal-length vector of minimums and maximums.")
  }

  pool <- vector("list", length(mins))
  for(grp in 1:length(pool)){
    pool[[grp]] <- runif(N, min = mins[grp], max = maxes[grp])
  }

  simulator.start(pool)
}

#' @rdname simulator
#' @section Setters:
#' \code{simulator.draw:} Method to draw data from a \code{simulator} object.
#'  This function returns a \code{simulator} object.
#' @export
simulator.draw <- function(simulator, allocation){

  # Sample data pool for new rewards
  rewards <- vector("numeric", length(allocation))
  i <- 1
  for(group in as.numeric(allocation)){

    # Sample 1 patient from the i-th group allocation
    index.i <- sample.int(length(simulator@pool[[group]]), 1)

    # Add the patient reward, then remove the patient from pool
    rewards[i] <- simulator@pool[[group]][index.i]
    simulator@pool[[group]] <- simulator@pool[[group]][-index.i]
    i <- i + 1
  }

  # Structure rewards as a data ingest list
  simulator@draw <- split(rewards, allocation)

  return(simulator)
}
