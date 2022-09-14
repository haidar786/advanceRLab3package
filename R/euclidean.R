euclidean <- function(a, b) {
  is.scalar <- function(s) is.atomic(s) && length(s) == 1L
  if (!is.numeric(a) | !is.numeric(b) | !is.scalar(a) | !is.scalar(b)) {
    return("Please enter numeric value")
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

euclidean(123612, 13892347912)
euclidean(100, 1000)
