mulNRFEdiscrete<-function(x,y,indseq,ntree=500)
{  
  n=dim(x)[1]
  fmoob=Inf###final choose mean oob
  moobseq=rep(0,dim(indseq)[1])
  selectedxyma=matrix(NA, nrow = dim(indseq)[1],ncol = 2*dim(indseq)[1])
  l=1
  while (length(indseq)>=4) {
    tay=as.data.frame(table(indseq[,2]))
    tabyf=as.numeric(as.vector(tay[,2]))####
    taby=as.numeric(as.vector(tay[,1]))
    ded=which(tabyf==1)
    if(length(ded)>0){
      for (t in 1:length(ded)) {
        indseq=indseq[-which(indseq[,2]==taby[ded[t]]),]
      }}
    selectedxyma[1:dim(indseq)[1],(2*l-1):(2*l)]=indseq
    
    tay=as.data.frame(table(indseq[,2]))
    taby=as.numeric(as.vector(tay[,1]))
    
    oob=0
    for (t in 1:length(taby)) {
      indyt=taby[t]
      indxt=which(indseq[,2]==indyt)
      xt=x[,indseq[indxt,1]]
      yt=as.factor(y[,indyt])####
      xp=xt
      yp=yt

      Rf_reg <- randomForest(xt, yt, ntree=ntree, importance=T )
      seqoob=Rf_reg$err.rate
      oobi=mean(seqoob[,1])
      oob<-oobi+oob
    }
    moob=oob/length(taby)                           
    if( moob<=fmoob )
    {
      fmoob<-moob
      selected_variables<-indseq
    }
    moobseq[l]=moob
    l=l+1
    indseq=indseq[-dim(indseq)[1],]
  }
  return(list(Mean_oob=fmoob,Selected_variables= selected_variables,moob_process=moobseq,selet_variables_process=selectedxyma))
}