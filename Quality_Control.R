#Options for reconstruction: Difference Ratio Ratio.Minus Icshape Logform
#Options for method: Samples Bootstrap Formula
#Options for Normalization: Two.And.Eight Boxplot No
QC<-function(data,reconstruction="Difference",method="Formula",Normalization="Boxplot"){
  #initialize the statistic that you will use in the process
  std=0;
  std.1=0;
  std.2=0;
  Beta=0;
  SNR=0;
  CQI=0;
  Beta=0;
  Beta.1=0;
  Beta.2=0;	
  CQI.LOW=0;
  CQI.MEDIUM=0;
  CQI.HIGH=0;
  
  #Calculate the standard deviation
  std=Std.Calculation(data,reconstruction,method,0)
  std.1=Std.Calculation(data,reconstruction,method,1)
  std.2=Std.Calculation(data,reconstruction,method,2)
  #Calculate the Beta
  Beta=Beta.Calculation(data,reconstruction,method,0)
  Beta.1=Beta.Calculation(data,reconstruction,method,1)
  Beta.2=Beta.Calculation(data,reconstruction,method,2)
  #To normalize the statistic you get from the program
  #CQI=Normalization.Determination("No",CQI)
  #SNR=Normalization.Determination(Normalization,SNR)
  Beta=Normalization.Determination(Normalization,Beta)
  Beta.1=Normalization.Determination(Normalization,Beta.1)
  Beta.2=Normalization.Determination(Normalization,Beta.2)
  #To get the CQI from the data
  CQI=CQI.Calculation(data,reconstruction)
  #To get the SNR from the data
  SNR=SNR.Calculation(Beta,std)
  SNR.1=SNR.Calculation(Beta.1,std.1)
  SNR.2=SNR.Calculation(Beta.2,std.2)
  
  
  #Try to calculate the CQI for each clusters
  CQI[which(Beta<0.1)]=0#To elimate the residuals that have a small Beta
  CQI.HIGH=quantile(na.omit(CQI[which(Beta>0.7)]),0.95)
  CQI.MEDIUM=quantile(na.omit(CQI[which(Beta>=0.3&&Beta<=0.7)]),0.95)
  CQI.LOW=quantile(na.omit(CQI[which(Beta<0.3)]),0.95)
  #To let the Betas all have a positive value
  Beta[which(Beta<=0)]=0
  Beta.1[which(Beta.1<=0)]=0
  Beta.2[which(Beta.2<=0)]=0
  SNR[which(SNR<=0)]=0
  SNR.1[which(SNR.1<=0)]=0
  SNR.2[which(SNR.2<=0)]=0
  
  print(CQI)  
  #CQI.PLOT=CQI[which(CQI<quantile(na.omit(CQI),0.6))];
  #print(CQI.PLOT)
  #To plot relevant pictures
  #plot(SNR,main="SNR",type="h",xlab="Resicuals",ylab="SNR")
  plot(Beta,main="Beta",type="h",xlab="Residuals",ylab="Beta")
  abline(h=0.3,lwd=1,col="green",lty=2)
  abline(h=0.7,lwd=1,col="red",lty=2)
  plot(na.omit(cbind(Beta.1,Beta.2)),main="Pearson Correlation")
   if(reconstruction!="Logform"){
     plot(CQI,type="h",main="CQI",xlab="Residuals",ylab="CQI")
     hist(CQI,20,freq=TRUE,main="CQI Histogram")
   }else{
     ;
   }
  boxplot(na.omit(SNR))
  
  d=Cal.data(data)
  return (list(SNR.1=SNR.1,SNR.2=SNR.2,Beta.1=Beta.1,Beta.2=Beta.2,X.PLUS.MEAN=mean(d$X.PLUS),X.MINUS.MEAN=mean(d$X.MINUS),X.PLUS.MEAN.1=mean(d$X.PLUS.1),X.MINUS.MEAN.1=mean(d$X.MINUS.1),X.PLUS.MEAN.2=mean(d$X.PLUS,2),X.MINUS.MEAN.2=mean(d$X.MINUS.2),C.PLUS.MEAN.2=mean(d$C.PLUS.2),C.MINUS.MEAN.2=mean(d$C.MINUS.2),C.PLUS.MEAN.1=mean(d$C.PLUS.1),C.MINUS.MEAN.1=mean(d$C.MINUS.1),C.PLUS.MEAN=mean(d$C.PLUS),C.MINUS.MEAN=mean(d$C.MINUS),SNR.MEAN=mean(na.omit(SNR)),SNR.MEAN.1=mean(na.omit(SNR.1)),SNR.MEAN.2=mean(na.omit(SNR.2)),SNR=SNR,Beta=Beta,Standard.Deviation=std,CQI=CQI,data.frame(CQI.LOW,CQI.MEDIUM,CQI.HIGH),Pearson.Correlation=cor(na.omit(cbind(Beta.1,Beta.2)))[1,2]))
}
