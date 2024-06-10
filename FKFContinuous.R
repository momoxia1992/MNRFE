FKFContinuous<-function (X, Y, nsis = (dim(X)[1])/log(dim(X)[1])) 
{
  n <- nrow(X)
  p <- ncol(X)
  N <- ceiling(log(n)) - 2
  nslices <- 3:(N + 2)
  ks.stat.single <- matrix(0, N, p)
  ks.stat.max <- rep(0, p)
  for (K in nslices) {
    slicing.scheme <- quantile(Y, seq(0, 1, 1/K))
    slicing.scheme[1] <- slicing.scheme[1] - 1
    slicing.scheme[K + 1] <- slicing.scheme[K + 1] + 1
    Y.dm <- cut(Y, slicing.scheme, labels = c(1:K), right = F)
    ks.stat <- matrix(0, p, K * (K - 1)/2)
    nclass <- 0
    for (j in 1:(K - 1)) {
      for (l in (j + 1):K) {
        nclass <- nclass + 1
        for (i in 1:p) {
          ks.stat[i, nclass] <- ks.test(X[Y.dm == j, 
                                          i], X[Y.dm == l, i])$statistic
        }
      }
    }
    ks.stat.max0 <- apply(ks.stat, 1, max)
    ks.stat.single[K - 2, ] <- ks.stat.max0
    ks.stat.max <- ks.stat.max + ks.stat.max0
  }
  k.rank = order(-ks.stat.max)#rank(-ks.stat.max, ties.method = "first")
  return(list(fks=ks.stat.max,rank=k.rank[1:nsis]))
}