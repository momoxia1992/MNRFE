mulNRFEcountinuous<-function(x,y,indseq,ntree=500)
{  
  n=dim(x)[1]
mmse=Inf
mmseseq=rep(0,dim(indseq)[1])
mmae=Inf
mmaeseq=rep(0,dim(indseq)[1])
mmape=Inf
mmapeseq=rep(0,dim(indseq)[1])
selectedxyma=matrix(NA, nrow = dim(indseq)[1],ncol = 2*dim(indseq)[1])
l=1

while (length(indseq)>=4) {
  
  tay=as.data.frame(table(indseq[,2]))
  tabyf=as.numeric(as.vector(tay[,2]))
  taby=as.numeric(as.vector(tay[,1]))
  ded=which(tabyf==1)
  if(length(ded)>0){
    for (t in 1:length(ded)) {
      indseq=indseq[-which(indseq[,2]==taby[ded[t]]),]
    }}
  selectedxyma[1:dim(indseq)[1],(2*l-1):(2*l)]=indseq
  
  tay=as.data.frame(table(indseq[,2]))
  taby=as.numeric(as.vector(tay[,1]))
  
  Mean_Squared_error=0
  Mean_absolute_error=0
  Mean_absolute_percentage_error=0
  for (t in 1:length(taby)) {
    indyt=taby[t]
    indxt=which(indseq[,2]==indyt)
    
    xt=x[,indseq[indxt,1]]
    yt=y[,indyt]
    xp=xt
    yp=yt
    Rf_reg <- randomForest(xt, yt, ntree=ntree, importance=T )
   
    # Prediction
    #rf_predictions <- predict(Rf_reg, xp)
    rf_predictions<-Rf_reg$predicted
    rf_predictions <- as.data.frame(rf_predictions)
    rf_predictions <- cbind(rf_predictions,yp)
    colnames(rf_predictions)<-c("prediction","test_y")
    rf_predictions <- as.data.frame(rf_predictions)
    rf_predictions <- rf_predictions%>%mutate(Test_error = (test_y-prediction)^2) #calculating relative error
    rf_predictions <- rf_predictions%>%mutate(abs_test_error=abs(test_y-prediction))
    rf_predictions <- rf_predictions%>%mutate(absolute_percentage_error=abs((abs_test_error/test_y)))
    # Calculating mean Sqaured error
    Mean_Squared_error<-sum(rf_predictions[,3])/nrow(rf_predictions)+Mean_Squared_error
    Mean_absolute_error<-sum(rf_predictions[,4])/nrow(rf_predictions)+ Mean_absolute_error
    Mean_absolute_percentage_error<-sum(rf_predictions[,5])/nrow(rf_predictions)+ Mean_absolute_percentage_error
  }
  MMean_Squared_error=Mean_Squared_error/length(taby) 
  MMean_absolute_error= Mean_absolute_error/length(taby) 
  MMean_absolute_percentage_error=Mean_absolute_percentage_error/length(taby) 
  
  if( MMean_Squared_error<=mmse )
  {
    mmse<-MMean_Squared_error
    mselected_variables<-indseq
  }
  if( MMean_absolute_error<mmae)
  {
    mmae<-MMean_absolute_error
    mmaeselected_variables<-indseq
  }
  if( MMean_absolute_percentage_error<mmape)
  {
    mmape<-MMean_absolute_percentage_error
    mmapeselected_variables<-indseq
  }
  mmseseq[l]=MMean_Squared_error
  mmaeseq[l]=MMean_absolute_error
  mmapeseq[l]=MMean_absolute_percentage_error
  
  l=l+1
  indseq=indseq[-dim(indseq)[1],]
}
return(list(MMSE=mmse,mseSelected_variables=mselected_variables,mmse_process=mmseseq,
            MMAE=mmae,mmaeseletec_variables=mmaeselected_variables,mmae_porcess=mmaeseq,
            MMAPE=mmape,mmapeSelected_variables= mmapeselected_variables,mmape_process=mmapeseq,
            selet_variables_process=selectedxyma))
}