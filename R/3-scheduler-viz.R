#' @rdname scheduler
#' @section Plotters:
#' \code{plotPrior:} Method to plot prior distributions.
#' @export
plotPrior <- function(scheduler){

  packageCheck("ggplot2")
  packageCheck("patchwork")

  sch <- scheduler

  xmin <- min(-3, min(unlist(sch@rewards)))
  xmax <- max(3, max(unlist(sch@rewards)))

  plots <- lapply(1:sch@K.arms, function(arm){

    plot_arm <- ggplot2::ggplot(data = data.frame(x = c(xmin, xmax)), ggplot2::aes_string("x")) +
      ggplot2::scale_y_continuous(breaks = NULL) + ggplot2::coord_flip() + ggplot2::theme_bw() +
      ggplot2::ylab("") + ggplot2::xlab(paste0("Experimental Arm ", arm)) +
      ggplot2::stat_function(fun = alt_dt, n = 1001,
                             args = list(df = scheduler@prior.df[arm],
                                         mean = scheduler@prior.mean[arm],
                                         sd = sqrt(scheduler@prior.var[arm])))

    return(plot_arm)
  })

  patchwork::wrap_plots(plots, nrow = 1)
}

#' @rdname scheduler
#' @section Plotters:
#' \code{plotPosterior:} Method to plot the most recent posterior distributions.
#' @export
plotPosterior <- function(scheduler){

  packageCheck("ggplot2")
  packageCheck("patchwork")

  sch <- scheduler

  xmin <- min(-3, min(unlist(sch@rewards)))
  xmax <- max(3, max(unlist(sch@rewards)))

  plots <- lapply(1:sch@K.arms, function(arm){

    plot_arm <- ggplot2::ggplot(data = data.frame(x = c(xmin, xmax)), ggplot2::aes_string("x")) +
      ggplot2::scale_y_continuous(breaks = NULL) + ggplot2::coord_flip() + ggplot2::theme_bw() +
      ggplot2::ylab("") + ggplot2::xlab(paste0("Experimental Arm ", arm)) +
      ggplot2::stat_function(fun = alt_dt, n = 1001,
                             args = list(df = scheduler@post.df[arm],
                                         mean = scheduler@post.mean[arm],
                                         sd = sqrt(scheduler@post.var[arm])))

    return(plot_arm)
  })

  patchwork::wrap_plots(plots, nrow = 1)
}

#' @rdname scheduler
#' @section Plotters:
#' \code{plotHistory:} Method to plot all posterior distributions.
#' @export
plotHistory <- function(scheduler){

  packageCheck("ggplot2")
  packageCheck("patchwork")

  sch <- scheduler

  cols <- rev(terrain.colors(sch@step+1))
  steps <- 0:(length(cols)-1)

  xmin <- min(-3, min(unlist(sch@rewards)))
  xmax <- max(3, max(unlist(sch@rewards)))

  plots <- lapply(1:sch@K.arms, function(arm){

    plot_arm <- ggplot2::ggplot(data = data.frame(x = c(xmin, xmax)), ggplot2::aes_string("x")) +
      ggplot2::scale_y_continuous(breaks = NULL) + ggplot2::coord_flip() + ggplot2::theme_bw() +
      ggplot2::ylab("") + ggplot2::xlab(paste0("Experimental Arm ", arm))

    for(t in steps){

      t.post <- sch@history.post[sch@history.post$step == t,]
      plot_arm <- plot_arm +
        ggplot2::stat_function(fun = alt_dt, n = 1001,
                               args = list(df = t.post$df[arm],
                                           mean = t.post$mean[arm],
                                           sd = sqrt(t.post$var[arm])),
                               colour = cols[t+1])
    }

    return(plot_arm)
  })

  patchwork::wrap_plots(plots, nrow = 1)
}

#' @rdname scheduler
#' @section Plotters:
#' \code{plotAllocation:} Method to plot the allocation ratios for each time step.
#' @export
plotAllocation <- function(scheduler){

  packageCheck("ggplot2")

  sch <- scheduler
  plot_arm <- ggplot2::ggplot(data = sch@history.post,
                              ggplot2::aes_string(x = "next_ratio", y = "mean", time = "step", label = "step", alpha = "step")) +
    ggplot2::geom_text() + ggplot2::geom_path() + ggplot2::facet_grid(arm ~ .) + ggplot2::xlim(0, 1) +
    ggplot2::ylab("Posterior Mean") + ggplot2::xlab("Subsequent Allocation Ratio") +
    ggplot2::labs(label = "Time Step", alpha = "Time Step") +
    ggplot2::theme_bw()
  plot_arm
}
