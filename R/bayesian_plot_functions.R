#' @title Plot Density given Normal Known Parameters.
#'
#' @param data_mu Numeric, mean of observed data.
#' @param data_sigma Numeric, variance of observed data.
#' @param prior_mu Numeric, mean of prior distribution.
#' @param prior_sigma Numeric, variance of prior distibution.
#' @return
#' @export
#'
#' @examples
plot_bayes_norm <-
   function(data_mu,
            data_sigma,
            prior_mu,
            prior_sigma,
            seed = 42) {
      if (data_sigma <= 0L || prior_sigma <= 0L) {
         stop("invalid variance parameters")
      }
      if (!all(sapply(c(
         data_mu, data_sigma, prior_mu, prior_sigma
      ), is.numeric))) {
         stop("all arguments must be passed as numeric vectors")
      }
      n <- 1L
      post_mu <-
         ((prior_mu / prior_sigma ^ 2) + ((n * data_mu) / data_sigma ^ 2)) /
         ((1 / prior_sigma ^ 2) + (n / data_sigma ^ 2))

      post_sigma <-
         sqrt(1 / ((1 / prior_sigma ^ 2) + (n / data_sigma ^ 2)))

      set.seed(seed)

      y <- seq(post_mu - 8 * post_sigma,
               post_mu + 8 * post_sigma,
               length.out = 500)  # to center plot on posterior

      .dists <- list()

      .dists$y_prior <- dnorm(y, prior_mu, prior_sigma)
      .dists$y_lik  <- dnorm(y, data_mu,  data_sigma)
      .dists$y_post  <- dnorm(y, post_mu,  post_sigma)


      .tags <- c("Prior", "Likelihood", "Posterior", "Data")
      y_max <- max(unlist(.dists))

      pal <- viridisLite::viridis(length(.dists))
      plot(
         y,
         .dists[[1L]],
         type = "l",
         col = pal[1],
         lty = 2,
         xlim = c(min(y), max(y)),
         ylim = c(0, y_max),
         ylab = "density",
         xlab = "Average Reddit Karma",
         lwd = 2
      )
      for (i in seq_along(.dists[-1])) {
         lines(y,
               .dists[[i + 1]],
               type = "l",
               col = pal[i + 1],
               lwd = 2)
      }

      abline(
         v = data_mu,
         col = pal[2L],
         lty = 3,
         lwd = 2
      )

      legend(
         "topright",
         col = c(pal, pal[2]),
         lty = c(2, 1, 1, 3),
         cex = 1.5,
         lwd = 2,
         bty = "n",
         legend = .tags

      )
   }
#' @title Plot Animated Density given Normal Known Parameters.
#'
#' @param x Data.frame, to be ploted.
#' @param aes Aesthetics defined by [ggplot2::aes]
#' @return
#' @export
#'
#' @examples
plot_bayes_animate <- function(x, aes = c("y", "density", "type")) {
   #lacks abstraction
   #ci <- aggregate(cbind(ci_high, ci_low) ~ year, data = x, function(x) mean(x))
   x <- x[x[,aes[3]] != 1,]
   x[,aes[3]] <- as.factor(x[,aes[3]])
   p <- ggplot2::ggplot(x, ggplot2::aes_string(x = aes[1], y = aes[2], fill = aes[3]))
   p <- p + ggplot2::geom_area()
   p <- p + ggplot2::geom_vline(data = ci,
                                ggplot2::aes(color = year, xintercept = ci_low), size = .25) +
      ggplot2::geom_vline(data = ci, ggplot2::aes(color = year, xintercept = ci_high), size = .25) +
      ggplot2::guides(color = FALSE)
   p <- p + ggplot2::scale_fill_viridis_d(name = "Distribution",
                                          labels = c("Posterior", "Prior")) +
      ggplot2::theme_bw()
   p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           text = ggplot2::element_text(size = 20))
   p <- p + ggplot2::xlim(c(.25,.75))
   p <- p + ggplot2::ylab(aes[2])

   p <- p + gganimate::transition_time(year) + ggplot2::labs(title = "Year: {frame_time}",
                                                             subtitle = "S.Curry 3 Point Performance") +
      gganimate::view_follow(fixed_y = TRUE)
   return(p)
}
