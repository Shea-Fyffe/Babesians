#' @title Combination Function
#'
#' @param n Numeric. Size of set.
#' @param k Numeric. Size to select.
#'
#' @return
#' @export
#'
#' @examples
combination <- function(n,k) {
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
permutation <- function(n,k) {
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
   .a <- combination(13,2) * combination(4,3) * combination(4,3) * combination(11,1) * combination(4,1)
   #1 triple 1 pair 2 random cards
   .b <- combination(13,1) * combination(12,1) * combination(4,2) * combination(4,3) * combination(11,2) * combination(4,1)  * combination(4,1)
   #1 triple 2 unique pair
   .c <- combination(13,1) * combination(12,2) * combination(4,3) * combination(4,2) * combination(4,2)
   .res <- c(.a, .b, .c)
   .res <- sapply(.res, function(x){
      x <- x / combination(52, hand_size)
   })
   return(sum(.res))
}
