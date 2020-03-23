#' @rdname scheduler
#' @section Setters:
#' \code{scheduler.start:} Method to initiate a \code{scheduler} object.
#'  This function returns an updated \code{scheduler} object.
#' @export
scheduler.start <- function(prior.mean, prior.var, N.burn.in, sampler = "sampler.thompson", heuristic = TRUE,
                            prior.nu, prior.alpha, prior.beta, ...){

  if(N.burn.in <= 0){
    stop("Provide a reasonable size for each arm of the 'burn-in' phase.")
  }

  if(!identical(class(sampler), "character")){
    stop("Provide 'sampler' argument as a character.")
  }

  sch <- new("scheduler")

  if(heuristic){

    if(length(prior.mean) != length(prior.var)){
      stop("Provide an equal-length vector of prior means and prior variances.")
    }

    sch@prior.df <- rep(Inf, length(prior.mean))
    sch@prior.mean <- prior.mean
    sch@prior.var <- prior.var
    sch@prior.nu <- as.numeric(NA)
    sch@prior.alpha <- as.numeric(NA)
    sch@prior.beta <- as.numeric(NA)

    sch@heuristic <- TRUE
    sch@post.df <- rep(Inf, length(prior.mean))
    sch@post.nu <- as.numeric(NA)
    sch@post.alpha <- as.numeric(NA)
    sch@post.beta <- as.numeric(NA)

  }else{

    if(missing(prior.beta) | missing(prior.alpha) | missing(prior.nu)){

      if(!missing(prior.var)){

        if(length(prior.mean) != length(prior.var)){
          stop("Provide an equal-length vector of prior means and prior variances.")
        }

        message("Alert: Guessing priors for alpha and beta based on 'prior.var'.")
        message("Alert: Setting prior nu equal to 1.")
        message("Alert: Setting prior alpha equal to 1.")
        message("Alert: Setting prior beta equal to 'prior.var'.")

        # NOTE: tau is a gamma distribution, sigma is inverse gamma!
        prior.nu <- rep(1, length(prior.mean))
        prior.alpha <- rep(1, length(prior.mean))
        prior.beta <- prior.var

      }else{

        stop("Provide a prior for variance, or a prior for nu, alpha, and beta.")
      }
    }

    # Save all priors
    sch@heuristic <- FALSE
    sch@prior.mean <- prior.mean
    sch@prior.nu <- prior.nu
    sch@prior.alpha <- prior.alpha
    sch@prior.beta <- prior.beta

    # These parameters define the marginal t-distribution for the posterior mean
    message("Alert: The @prior.var and @post.var slots will contain the variance of the marginal\n",
            " distribution of the mean, not the expected variance of the data!")
    sch@prior.df <- 2 * sch@prior.alpha
    sch@prior.var <- sch@prior.beta / (sch@prior.nu * sch@prior.alpha)
  }

  sch@N.burn.in <- N.burn.in
  sch@K.arms <- length(prior.mean)
  sch@step <- 0
  sch@rewards <- lapply(1:length(prior.mean), function(x) NULL) # rewards are NULL to start
  sch@meta <- list() # for future use...
  sch@sampler.args <- as.list(substitute(list(...)))[-1]
  sch@sampler <- sampler # the sampler is saved on initialization
  allocation <- rep(1:sch@K.arms, N.burn.in) # burn-in will have an equal allocation ratio
  sch@allocation <- sample(factor(allocation, 1:sch@K.arms)) # make sure first allocation is random
  sch@ingest <- split(rep(NA, length(sch@allocation)), sch@allocation) # dummy structure for first data ingest
  sch@history.post <- data.frame(step = sch@step, arm = 1:sch@K.arms,
                                 mean = sch@prior.mean, var = sch@prior.var, df = sch@prior.df,
                                 nu = sch@prior.nu, alpha = sch@prior.alpha, beta = sch@prior.beta,
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

  # Name the list of rewards
  if(is.null(names(sch@rewards))){
    names(sch@rewards) <- names(data.ingest)
  }else{
    if(!identical(names(sch@rewards), names(data.ingest))){
      warning("Reward names do not match. Are you sure about your data?")
    }
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

  if(sch@heuristic){

    # Update all posteriors using empiric data
    prior.prec <- 1/sch@prior.var
    sch@post.mean <-
      # (prec0 * mu0 + prec*sum(x)) / (prec0 + n*prec)
      (prior.prec*sch@prior.mean + sch@online.prec*sch@online.sum)/
      (prior.prec + sch@online.count*sch@online.prec)
    post.prec <- prior.prec + sch@online.count*sch@online.prec
    sch@post.var <- 1/post.prec

  }else{

    # Update all posteriors using empiric data
    sch@post.mean <-
      (sch@prior.nu * sch@prior.mean + sch@online.count * sch@online.mean) /
      (sch@prior.nu + sch@online.count)
    sch@post.nu <- sch@prior.nu + sch@online.count
    sch@post.alpha <- sch@prior.alpha + sch@online.count/2
    sch@post.beta <-
      sch@prior.beta + sch@online.count*sch@online.var/2 +
      (sch@online.count*sch@prior.nu)/(sch@prior.nu + sch@online.count) *
      (sch@online.mean-sch@prior.mean)^2/2

    # These parameters define the marginal t-distribution for the posterior mean
    sch@post.df <- 2 * sch@post.alpha
    sch@post.var <- sch@post.beta / (sch@post.nu * sch@post.alpha)
  }

  # Call the sampler (e.g., sampler.thompson) one patient at a time...
  sch@dynamic.count <- sch@online.count # used by UCB algorithm to batch allocate patients
  allocation <- sapply(1:N.allocate, function(patient){
    pos <- do.call(sch@sampler, append(list("scheduler" = sch), sch@sampler.args))
    sch@dynamic.count[pos] <<- sch@dynamic.count[pos] + 1 # let UCB know that +1 patient will be sampled
    return(pos)
  })

  # factor() makes sure that arms with no patients are not forgotten
  sch@allocation <- factor(allocation, 1:sch@K.arms)

  # Define new @ingest expectations
  sch@ingest <- split(rep(NA, length(sch@allocation)), sch@allocation) # dummy structure for next data ingest

  # Update posterior history
  sch@history.post <- rbind(
    sch@history.post,
    data.frame(step = sch@step, arm = 1:sch@K.arms,
               mean = sch@post.mean, var = sch@post.var, df = sch@post.df,
               nu = sch@post.nu, alpha = sch@post.alpha, beta = sch@post.beta,
               total = sch@online.count, next_ratio = as.numeric(table(sch@allocation) / sum(table(sch@allocation)))
    )
  )

  return(sch)
}
