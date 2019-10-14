#' @title Combination Function
#'
#' @param n Numeric. Size of set.
#' @param k Numeric. Size to select.
#'
#' @return
#' @export
#'
#' @examples
combination <- function(n, k) {
   .res <- factorial(n) / (factorial(k) * factorial(n - k))
   return(.res)
}
#' @title Permutation Function
#'
#' @inheritParams combination
#'
#' @return
#' @export
#'
#' @examples
permutation <- function(n, k) {
   .res <- factorial(n) / factorial(n - k)
   return(.res)
}
#' @title Get the Probability of a Full House
#'
#' @param hand_size
#'
#' @return
#' @export
#'
#' @examples
full_house <- function(hand_size) {
   #2 triples 1 random card
   .a <-
      combination(13, 2) * combination(4, 3) * combination(4, 3) * combination(11, 1) * combination(4, 1)
   #1 triple 1 pair 2 random cards
   .b <-
      combination(13, 1) * combination(12, 1) * combination(4, 2) * combination(4, 3) * combination(11, 2) * combination(4, 1)  * combination(4, 1)
   #1 triple 2 unique pair
   .c <-
      combination(13, 1) * combination(12, 2) * combination(4, 3) * combination(4, 2) * combination(4, 2)
   .res <- c(.a, .b, .c)
   .res <- sapply(.res, function(x) {
      x <- x / combination(52, hand_size)
   })
   return(sum(.res))
}
#' @title Get Posterior Probabilities Based on Bayes Rule
#'
#' @param h Numeric. vector of length 2 P(H)
#' @param d Numeric. vector of length 2 P(D|H)
#' @param H1 Logical. Assume the Null Hypothesis is False?
#'
#' @return
#' @export
#'
#' @examples
bayes_rule <- function(h, d, H1 = TRUE) {
   if (length(h) == 2L) {
      dh <- c(d, 1 - d)
   }
   if (length(revise_cell) != 2L) {
      stop("revise_cell should specify row and column number")
   }
   ct <- h * matrix(dh, ncol = 2)
   if (H1) {
      pos <- ct[, 1] / sum(ct[, 1])
   } else {
      pos <- ct[, 2] / sum(ct[, 2])
   }
   ct <- cbind(h, d, ct, pos)
   colnames(ct) <- c("Prior", "Likelihood", "BNH1", "BNH0", "POS")
   return(ct)
}
#' @title Plot Density given Normal Known Parameters.
#'
#'
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
