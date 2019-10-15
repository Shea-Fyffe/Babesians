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
#' @title Roll some Catan dice
roll_catan_dice <- function(n_dice = 2L, n_rolls = 1000L, seed = 46,
                            plot = FALSE, ...){
   stopifnot({
      is.numeric(n_dice) && is.numeric(n_rolls) && is.numeric(seed)
   })
 set.seed(seed)
   .out <- replicate(n_rolls, sample(1:6, n_dice, replace = T))
   .out <- apply(.out, 2, sum)
   if (plot) {
      h <- hist(.out, breaks = 100, plot = FALSE)
      h$counts <- h$counts / sum(h$counts)
      plot(h, ylab = "Probability", xlab = "Sum of Roll",
           main = sprintf("Odds of rolling sum for %s dice", n_dice),
           ...)
   }
   return(.out)
}

