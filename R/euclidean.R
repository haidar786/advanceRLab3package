print("euclidean")
euclidean <- function(a, b) {
  one <- a;
  two <- b;

  while (two != 0) {
    rem <- one%%two
    one <- two
    two <- rem
  }
  return(one)
}

euclidean(20, 8)
