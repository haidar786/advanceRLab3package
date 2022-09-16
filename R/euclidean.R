#' Euclidean algorithm is used to find greatest common divisor
#'
#' @param a This argument is a numeric only value argument as a first value to find the euclidean
#' @param b This is a second argument with only numeric value to find the euclidean
#' @return  This return the euclidean final result
#' @description
#' In mathematics, the Euclidean algorithm or Euclid's algorithm, is an efficient method for computing the greatest common divisor of two integers numbers, the largest number that divides them both without a remainder. Find Wikipedia docs \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Here}.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @export
euclidean <- function(a, b) {
  is.scalar <- function(s) is.atomic(s) && length(s) == 1L
  if (!is.numeric(a) | !is.numeric(b) | !is.scalar(a) | !is.scalar(b)) {
    stop("Please enter numeric value")
  }
  one <- a;
  two <- b;

  while (two != 0) {
    rem <- one%%two
    one <- two
    two <- rem
  }
  return(one)
}
euclidean(-100, -10)
