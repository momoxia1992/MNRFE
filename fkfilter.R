fkfilter<-function (X, Y, nsis = (dim(X)[1])/log(dim(X)[1])) 
{
  if (dim(X)[1] != length(Y)) {
    stop("X and Y should have same number of rows!")
  }
  if (missing(X) | missing(Y)) {
    stop("The data is missing!")
  }
  if (TRUE %in% (is.na(X) | is.na(Y) | is.na(nsis))) {
    stop("The input vector or matrix cannot have NA!")
  }
  if (inherits(Y, "Surv")) {
    stop("Kfilter can not implemented with object  of Surv")
  }
  N = NULL
  nslices = NULL
  slicing.scheme = NULL
  if (length(table(Y)) <= 45) {
    Y <- factor(Y)
    obj <- FKFSingle(X = X, Y = Y, nsis)
  }
  else {
    n <- nrow(X)
    if (is.null(N)) {
      N <- ceiling(log(n)) - 2
    }
    if (is.null(nslices)) {
      nslices <- 3:(N + 2)
    }
    obj <- FKFContinuous(X = X, Y = Y, nsis)
  }
  return(obj)
}