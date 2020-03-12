#' @rdname scheduler
#' @section Setters:
#' \code{scheduler.start:} Method to initiate a \code{scheduler} object.
#'  This function returns an updated \code{scheduler} object.
#' @export
scheduler.start <- function(prior.mean, prior.var, N.burn.in, sampler = thompson){

  if(length(prior.mean) != length(prior.var)){
    stop("Provide an equal-length vector of prior means and prior variances.")
  }

  if(N.burn.in <= 0){
    stop("Provide a reasonable size for each arm of the 'burn-in' phase.")
  }

  sch <- new("scheduler")
  sch@prior.mean <- prior.mean
  sch@prior.var <- prior.var
  sch@prior.prec <- 1/prior.var
  sch@N.burn.in <- N.burn.in
  sch@K.arms <- length(prior.mean)
  sch@step <- 0
  sch@rewards <- lapply(1:length(prior.mean), function(x) NULL) # rewards are NULL to start
  sch@meta <- list() # for future use...
  sch@sampler <- sampler # the sampler is saved on initialization
  allocation <- rep(1:sch@K.arms, N.burn.in) # burn-in will have an equal allocation ratio
  sch@allocation <- sample(factor(allocation, 1:sch@K.arms)) # make sure first allocation is random
  sch@ingest <- split(rep(NA, length(sch@allocation)), sch@allocation) # dummy structure for first data ingest
  sch@history.post <- data.frame(step = sch@step, arm = 1:sch@K.arms,
                                 mean = sch@prior.mean, var = sch@prior.var, prec = sch@prior.prec,
                                 total = 0, next_ratio = 1/sch@K.arms) # initialize history table with priors
  sch@history <- list() # but history list is empty
  return(sch)
}

#' @rdname scheduler
#' @section Setters:
#' \code{scheduler.start:} Method to update a \code{scheduler} object with new data.
#'  This function returns an updated \code{scheduler} object.
#' @export
scheduler.update <- function(scheduler, data.ingest, N.allocate){

  sch <- scheduler

  # This makes sure the incoming data matches those expected by the previous time step
  if(!all(sapply(data.ingest, length) == sapply(sch@ingest, length))){
    stop("Incoming data does not match expectations.")
  }

  # Update @rewards with the incoming data
  for(arm in 1:sch@K.arms){
    sch@rewards[[arm]] <- c(sch@rewards[[arm]], data.ingest[[arm]])
  }

  # Update @step by +1
  sch@step <- sch@step + 1

  # Update @history with the incoming data
  sch@history <- c(sch@history, list(data.ingest))

  # Update current means, variances, and precisions
  sch@online.count <- sapply(sch@rewards, length)
  sch@online.sum <- sapply(sch@rewards, sum)
  sch@online.mean <- sapply(sch@rewards, mean)
  sch@online.var <- sapply(sch@rewards, var)
  sch@online.prec <- sapply(sch@rewards, function(reward) 1/var(reward))

  # Update all posteriors using empiric data
  sch@post.mean <- sapply(
    1:sch@K.arms,
    function(arm){
      # (prec0 * mu0 + prec*sum(x)) / (prec0 + n*prec)
      (sch@prior.prec[arm]*sch@prior.mean[arm] + sch@online.prec[arm]*sch@online.sum[arm])/
        (sch@prior.prec[arm] + sch@online.count[arm]*sch@online.prec[arm])
    })
  sch@post.prec <- sapply(
    1:sch@K.arms,
    function(arm){
      sch@prior.prec[arm] + sch@online.count[arm]*sch@online.prec[arm]
    })
  sch@post.var <- sapply(sch@post.prec, function(prec) 1/prec)

  # Call the sampler (e.g., sampler.thompson) one patient at a time...
  allocation <- sapply(1:N.allocate, function(patient){
    do.call(sch@sampler, list("scheduler" = sch))
  })
  sch@allocation <- factor(allocation, 1:sch@K.arms) # factor() makes sure that arms with no patients are not forgotten

  # Define new @ingest expectations
  sch@ingest <- split(rep(NA, length(sch@allocation)), sch@allocation) # dummy structure for next data ingest

  # Update posterior history
  sch@history.post <- rbind(
    sch@history.post,
    data.frame(step = sch@step, arm = 1:sch@K.arms,
               mean = sch@post.mean, var = sch@post.var, prec = sch@post.prec,
               total = sch@online.count, next_ratio = as.numeric(table(sch@allocation) / sum(table(sch@allocation)))
    )
  )

  return(sch)
}
