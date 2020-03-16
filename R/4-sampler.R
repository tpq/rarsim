#' @rdname scheduler
#' @section Samplers:
#' \code{sampler.thompson:} Method to allocate patients by Thompson sampling.
#'  This function returns an integer corresponding to the group
#'  to which the patient is randomly allocated.
#' @export
sampler.thompson <- function(scheduler){

  sch <- scheduler

  # Sample each posterior distribution exactly once
  sample_each_arm <- sapply(
    1:sch@K.arms,
    function(arm){
      rnorm(1, mean = sch@post.mean[arm], sd = sqrt(sch@post.var[arm]))
    })

  # Choose allocation group based on the max reward sampled
  allocation <- which.max(sample_each_arm)

  return(allocation)
}
