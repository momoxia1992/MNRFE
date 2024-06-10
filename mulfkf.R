mulfkf<-function(x,y,xnsis=(dim(x)[1])/log(dim(x)[1]),ynsis=(dim(y)[1])/log(dim(y)[1]),datatype="NULL",ntree=500)
{
  n=dim(x)[1]
  p=dim(x)[2]
  q=dim(y)[2]
  if (dim(x)[1] != dim(y)[1]) {
    stop("X and Y should have same number of rows!")
  }
  if (missing(x) | missing(y)) {
    stop("The data is missing!")
  }
  
  ######Determining data types####
  if(datatype=="Classification"){
    
  }else if(datatype=="Continious"){
    
  }else if(length(table(y)) <= 45*ynsis){datatype="Classification"
  } else {datatype="Continious"}
  
  if (datatype=="Classification") { 
    ####classification
    fkfm=matrix(NA,nrow = p,ncol = q)
    for (k in 1:q) {
      actset0=fkfilter(x,y[,k],xnsis)
      fkfm[,k]=actset0$fks
    }####compute kjk matrix pxq
    indseq=arrayInd(sort.list(fkfm,decreasing=T)[1:(ynsis*xnsis)],dim(fkfm))###choose first ynsis*xnsis element
    colnames(indseq)=c("selected x","selected y")
    re1=mulNRFEdiscrete(x,y,indseq,ntree=ntree)
  }
  if (datatype=="Continious") {
    ###continious regression####
    fkfm=matrix(NA,nrow = p,ncol = q)
    for (k in 1:q) {
      actset0=fkfilter(x,y[,k],xnsis)
      fkfm[,k]=actset0$fks
    }####compute kjk matrix pxq
    indseq=arrayInd(sort.list(fkfm,decreasing=T)[1:(ynsis*xnsis)],dim(fkfm)) ###choose first ynsis*xnsis element
    colnames(indseq)=c("selected x","selected y")
    re1=mulNRFEcountinuous(x,y,indseq,ntree=ntree)
  }
  return(re1)
}