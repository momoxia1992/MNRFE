FKFSingle<-function (X, Y, nsis = (dim(X)[1])/log(dim(X)[1])) 
{
  Y = as.factor(Y)
  Y.dm <- Y
  Y.dm <- as.numeric(Y.dm)
  K <- length(unique(Y.dm))
  p <- ncol(X)
  ks.stat <- matrix(0, p, K * (K - 1)/2)
  nclass <- 0
  for (j in 1:(K - 1)) {
    for (l in (j + 1):K) {
      nclass <- nclass + 1
      for (i in 1:p) {
        ks.stat[i, nclass] <- ks.test(X[Y.dm == j, i], 
                                      X[Y.dm == l, i])$statistic
      }
    }
  }
  ks.stat.max0 <- apply(ks.stat, 1, max)
  k.rank = order(-ks.stat.max0)# rank(-ks.stat.max0, ties.method = "first")
  return(list(fks=ks.stat.max0,rank=k.rank[1:nsis]))
}