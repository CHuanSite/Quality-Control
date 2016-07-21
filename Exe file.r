#Options for reconstruction: Difference Ratio Difference_minus1 Icshape
#Options for method: Paramatric Bootstrap Formula
#Options for Normalization: Simple Boxplot No
QC<-function(data,reconstruction="Difference",method="Parametric",Normalization="Simple"){

  #initialize the statistic that you will use in the process
  std=0;
  mean=0;
  SNR=0;

  #Calculate the standard deviation
  std=Std.Calculation(data,reconstruction,method)

  #Calculate the mean
  mean=Mean.Calculation(data,reconstruction)

  #To decide whether to use the normalization method
  SNR=Normalization.Determination(Normalization,mean,std)

  #To get the statistic from the replicate
  Statistic.Combination=Return.Statistic(data,reconstruction)
  statistic1=Statistic.Combination[2]
  statistic2=Statistic.Combination[3]

  #To plot relevant pictures
  SNR.plot=SNR;
  SNR.plot[which(SNR.plot<0)]=0
  plot(SNR.plot,type="h",main="The Picture of the SNR")
  plot(cbind(statistic1,statistic2),main="The Correlation Between Statistics")
  
  return (list(SNR=SNR,Pearson.Correlation=cor(statistic1,statistic2),Mean=mean,Standard.Deviation=std))
}

#Extract the data from the original dataset
Cal.data<-function(data){
  data.calculation=0
  d<-data[which(data[,1]>0),]
  rownames(d)<-d[,1]
  X.PLUS<-d[,4]
  X.MINUS<-d[,5]
  X.PLUS.1<-d[,6]
  X.MINUS.1<-d[,7]
  X.PLUS.2<-d[,8]
  X.MINUS.2<-d[,9]
  C.MINUS<-d[,10]+X.MINUS
  C.PLUS<-d[,11]+X.PLUS
  C.MINUS.1<-d[,12]+X.MINUS.1
  C.PLUS.1<-d[,13]+X.PLUS.1
  C.MINUS.2<-d[,14]+X.MINUS.2
  C.PLUS.2<-d[,15]+X.PLUS.2
  data.calculation=data.frame(X.PLUS,X.MINUS,X.PLUS.1,X.MINUS.1,X.PLUS.2,X.MINUS.2,C.PLUS,C.PLUS.1,C.PLUS.2,C.MINUS,C.MINUS.1,C.MINUS.2)
  return (data.calculation)
}

Normalization.Boxplot<-function(SNR){
  SNR=na.omit(SNR)
  lower.bound<-quantile(SNR,0.25)
  upper.bound<-quantile(SNR,0.75)
  distance=upper.bound-lower.bound
  SNR<-SNR[-which(SNR>=distance*1.5)]
  SNR<-SNR[-which(SNR<=-distance*1.5)]
  return (SNR)
}

Normalization.Two.And.Eight<-function(SNR){
  SNR=na.omit(SNR)
  two.percent<-quantile(SNR,0.98)
  ten.percent<-quantile(SNR,0.10)
  SNR<-SNR[-which(SNR>two.percent)]
  mean.ten<-mean(SNR[which(SNR>=ten.percent&&SNR<=two.percent)])
  SNR=SNR/mean.ten
  return (SNR)
}

#Calculation method for the mean
Mean_calculation<-function(data,reconstruction){
  mean=0;
  if(reconstruction=="Difference"){
    mean=apply(Difference(data),1,mean)
  }else if(reconstruction=="Ratio"){
    mean=apply(Ratio(data),1,mean)
  }else if(reconstruction=="Difference_minus1"){
    mean=apply(Difference_minus1(data),1,mean)
  }else if(reconstruction=="Icshape"){
    mean=apply(Icshape(data),1,mean)
  }else if(reconstruction=="Logform"){
    mean=apply(Logform(data),1,mean)
  }else{
    ;
  }
  return (mean)
}

Normalization.Determination<-function(Normalization,mean,std){
  SNR=0;
  if(Normalization=="Simple"){
    SNR=Normalization.Two.And.Eight(SNR.Calculation(mean,std))
  }else if(Normalization=="Boxplot"){
    SNR=Normalization.Boxplot(SNR.Calculation(mean,std))
  }else{
    SNR=SNR.Calculation(mean,std);
  }
  return (SNR)
}

#To estimate the variance through Bootstrap

Difference.Boot<-function(data,indices){
  D<-Cal.data(data)
  D<-D[indices,]
  statistic<-D$X.PLUS/D$C.PLUS-D$X.MINUS/D$C.MINUS
}

Ratio.Boot<-function(data,indices){
  D<-Cal.data(data)
  D<-D[indices,]
  statistic<-(D$X.PLUS/D$C.PLUS)/(D$X.MINUS/D$C.MINUS)
}

Ratio.Minus.Boot<-function(data,indices){
  D<-Cal.data(data)
  D<-D[indices,]
  statistic<-(D$X.PLUS/D$C.PLUS)/(D$X.MINUS/D$C.MINUS)-1
}

Icshape.Boot<-function(data,indices){
  alpha=0.25
  D<-Cal.data(data)
  D<-Cal.data(data)[indices,]
  statistic<-(D$X.PLUS-alpha*D$X.MINUS)/D$C.MINUS;
}

Logform.Boot<-function(data,indices){
  D<-Cal.data(data)
  D<-D[indices,]
  statistic<-log(D$X.PLUS+1)/(sum(log(D$X.PLUS)+1))*nrow(D)-log(D$X.MINUS+1)/(sum(log(D$X.MINUS)+1))*nrow(D)
}

#bt<-boot(data=d,statistic=Icshape_Boot,R=100)

#To estimate the variance through replicates
Difference.Replicates<-function(data){
  D<-Cal.data(data)
  statistic<-D$X.PLUS/D$C.PLUS-D$X.MINUS/D$C.MINUS
  statistic.1<-D$X.PLUS.1/D$C.PLUS.1-D$X.MINUS.1/D$C.MINUS.1
  statistic.2<-D$X.PLUS.2/D$C.PLUS.2-D$X.MINUS.2/D$C.MINUS.2
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}

Ratio.Replicates<-function(data){
  D<-Cal.data(data)
  statistic<-(D$X.PLUS/D$C.PLUS)/(D$X.MINUS/D$C.MINUS)
  statistic.1<-(D$X.PLUS.1/D$C.PLUS.1)/(D$X.MINUS.1/D$C.MINUS.1)
  statistic.2<-(D$X.PLUS.2/D$C.PLUS.2)/(D$X.MINUS.2/D$C.MINUS.2)
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}

Ratio.Minus.Replicates<-function(data){
  D<-Cal.data(data)
  statistic<-(D$X.PLUS/D$C.PLUS)/(D$X.MINUS/D$C.MINUS)-1
  statistic.1<-(D$X.PLUS.1/D$C.PLUS.1)/(D$X.MINUS.1/D$C.MINUS.1)-1
  statistic.2<-(D$X.PLUS.2/D$C.PLUS.2)/(D$X.MINUS.2/D$C.MINUS.2)-1
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}

Icshape_Replicates<-function(data){
  alpha=0.25
  D<-Cal.data(data)
  statistic<-(D$X.PLUS-alpha*D$X.MINUS)/D$C.MINUS;
  statistic.1<-(D$X.PLUS.1-alpha*D$X.MINUS.1)/D$C.MINUS.1;
  statistic.2<-(D$X.PLUS.2-alpha*D$X.MINUS.2)/D$C.MINUS.2;
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}

Logform_Replicates<-function(data){
  D<-Cal.data(data)
  statistic<-log(D$X.PLUS+1)/(sum(log(D$X.PLUS+1)))*nrow(D)-log(D$X.MINUS+1)/(sum(log(D$X.MINUS+1)))*nrow(D)
  statistic1<-log(D$X.PLUS.1+1)/(sum(log(D$X.PLUS.1+1)))*nrow(D)-log(D$X.MINUS.1+1)/(sum(log(D$X.MINUS.1+1)))*nrow(D)
  statistic2<-log(D$X.PLUS.2+1)/(sum(log(D$X.PLUS.2+1)))*nrow(D)-log(D$X.MINUS.2+1)/(sum(log(D$X.MINUS.2+1)))*nrow(D)                          
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}


#To estimate the variance through Formula
Std.Difference<-function(data){
  d<-Cal.data(data);
  Beta<-d$X.PLUS/d$C.PLUS;
  Gamma<-d$X.MINUS/d$C.MINUS;
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/d$C.PLUS+Gamma*(1-Gamma)/d$C.MINUS;
  return (sqrt(Var.Beta))
}

Std.Ratio<-function(data){
  d<-Cal.data(data);
  Beta<-d$X.PLUS/d$C.PLUS;
  Gamma<-d$X.MINUS/d$C.MINUS;
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2*d$C.PLUS)+(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*d$C.MINUS)
  return (sqrt(Var.Beta))
}

Std.Ratio.Minus<-function(data){
  d<-Cal.data(data);
  Beta<-d$X.PLUS/d$C.PLUS;
  Gamma<-d$X.MINUS/d$C.MINUS;
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2*d$C.PLUS)+(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*d$C.MINUS)
  return (sqrt(Var.Beta))
}

Std.Icshape<-function(data){
  alpha=0.25
  d<-Cal.data(data);
  Beta<-d$X.PLUS/d$C.PLUS;
  Gamma<-d$X.MINUS/d$C.MINUS;
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))*d$C.PLUS/d$C.MINUS+alpha^2*Gamma*(1-Gamma)/d$C.Minus;
  return (sqrt(Var.Beta))
}
#This one is unknown
#Variance.Logfrom<-function(data){
#  
#}


#Calculation method for the standard deviation
Std_calculation<-function(data,reconstruction="Difference",method="Parametric"){
  std=0;
  if(method=="Bootstrap"){
    if(reconstructoin=="Difference"){
      std=boot(data=data,statistic=Difference_Boot,R=100)
    }else if(reconstruction=="Ratio"){
      std=boot(data=data,statistic=Ratio_Boot,R=100)
    }else if(reconstruction=="Difference_minus1"){
      std=boot(data=data,statistic="Difference_minus1_Boot",R=100)
    }else if(reconstruction=="Icshape"){
      std=boot(data=data,statistic="Icshape_Boot",R=100)
    }else if(reconstruction=="Logform"){
      std=boot(data=data,statistic="Logform_Boot",R=100)
    }else{
      ;
    }
  }else if(method=="Formula"){
    if(reconstruction=="Difference"){
      std=Difference.Replicates(data)
    }else if(reconstruction=="Ratio"){
      std=Ratio.Replicates(data)
    }else if(reconstruction=="Difference_minus1"){
      std=Ratio.Minus.Replicates(data)
    }else if(reconstruction=="Icshape"){
      std=Icshape.Replicates(data)
    }else if(reconstruction=="Logform"){
      std=Logform.Replicates(data)
    }else{
      ;
    }
  }else if(method=="Parametric"){
    if(reconstruction=="Difference"){
      std=Std.Difference(data)
    }else if(reconstruction=="Ratio"){
      std=Std.Ratio(data)
    }else if(reconstruction=="Difference_minus1"){
      std=Std.Ratio.Minus(data)
    }else if(reconstruction=="Icshape"){
      std=Std.Icshape(data)
    }else if(reconstruction=="Logform"){
      std=Logform.Replicates(data)
    }else{
      ;
    }
  }else{
    ;
  }
    return (std)
}


CQI.Calculation.Difference<-function(data){
  d=Cal.data(data)
  percentage.error=0.4
  z.value=1.96
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-d$X.MINUS/d$C.MINUS
  Var=(Beta*perenctage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired=((Beta+Gamma*(1-Gamma))*(1-Beta-Gamma*(1-Beta))+Gamma*(1-Gamma)/s)/Var
  return (C.PLUS.desired)
}

CQI.Calculation.Ratio<-function(data){
  d=Cal.data(data)
  percentage.error=0.4
  z.value=1.96
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-d$X.MINUS/d$C.MINUS
  Var=(Beta*perenctage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2)+1/Var*(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*s)
  return (C.PLUS.desired)
}

CQI.Calculation.Ratio.Minus<-function(data){
  d=Cal.data(data)
  percentage.error=0.4
  z.value=1.96
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-d$X.MINUS/d$C.MINUS
  Var=(Beta*perenctage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2)+1/Var*(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*s)
  return (C.PLUS.desired)
}

CQI.Calculation.Icshape<-function(data){
  alpha=0.25
  d=Cal.data(data)
  percentage.error=0.4
  z.value=1.96
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-d$X.MINUS/d$C.MINUS
  Var=(Beta*perenctage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*((Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Gamma))/s^2+alpha^2*Gamma*(1-Gamma)/s)
  return (C.PLUS.desired)
}


#To calculate the statistic of each form
Difference<-function(data){
    statistics=0
    D<-Cal.data(data)
    statistic<-D$X.PLUS/D$C.PLUS-D$X.MINUS/D$C.MINUS
    statistic.1<-D$X.PLUS.1/D$C.PLUS.1-D$X.MINUS.1/D$C.MINUS.1
    statistic.2<-D$X.PLUS.2/D$C.PLUS.2-D$X.MINUS.2/D$C.MINUS.2
    statistics=data.frame(statistic,statistic.1,statistic.2)
    return (statistics)
}

Ratio<-function(data){
  statistics=0
  D<-Cal.data(data)
  statistic<-(D$X.PLUS/D$C.PLUS)/(D$X.MINUS/D$C.MINUS)
  statistic.1<-(D$X.PLUS.1/D$C.PLUS.1)/(D$X.MINUS.1/D$C.MINUS.1)
  statistic.2<-(D$X.PLUS.2/D$C.PLUS.2)/(D$X.MINUS.2/D$C.MINUS.2)
  statistics=data.frame(statistic,statistic.1,statistic.2)
  return (statistics)
}

Difference_minus1<-function(data){
  statistics=0
  D<-Cal.data(data)
  statistic<-(D$X.PLUS/C.PLUS)/(D$X.MINUS/C.MINUS)-1
  statistic.1<-(D$X.PLUS.1/D$C.PLUS.1)/(D$X.MINUS.1/D$C.MINUS.1)-1
  statistic.2<-(D$X.PLUS.2/D$C.PLUS.2)/(D$X.MINUS.2/D$C.MINUS.2)-1
  statistics=data.frame(statistic,statistic.1,statistic.2)
  return (statistics)
}

Icshape<-function(data){
  statistics=0
  alpha=0.25
  D<-Cal.data(data)
  statistic<-(D$X.PLUS-alpha*D$X.MINUS)/D$C.MINUS;
  statistic.1<-(D$X.PLUS.1-alpha*D$X.MINUS.1)/D$C.MINUS.1;
  statistic.2<-(D$X.PLUS.2-alpha*D$X.MINUS.2)/D$C.MINUS.2;
  statistics=data.frame(statistic,statistic.1,statistic.2)
  return (statistics)
}

Logform<-function(data){
  statistics=0
  D<-Cal.data(data)
  statistic<-log(D$X.PLUS+1)/(sum(log(D$X.PLUS+1)))*nrow(D)-log(D$X.MINUS+1)/(sum(log(D$X.MINUS+1)))*nrow(D)
  statistic.1<-log(D$X.PLUS.1+1)/(sum(log(D$X.PLUS.1+1)))*nrow(D)-log(D$X.MINUS.1+1)/(sum(log(D$X.MINUS.1+1)))*nrow(D)
  statistic.2<-log(D$X.PLUS.2+1)/(sum(log(D$X.PLUS.2+1)))*nrow(D)-log(D$X.MINUS.2+1)/(sum(log(D$X.MINUS.2+1)))*nrow(D)                          
  statistics=data.frame(statistic,statistic.1,statistic.2)
  return (statistics)
}


Return.Statistic<-function(data,reconstruction){
  statistic=0
  if(reconstruction=="Difference"){
    statistic=Difference(data)
  }else if(reconstruction=="Ratio"){
    statistic=Ratio(data)
  }else if(reconstruction=="Difference_minus1"){
    statistic=Difference_minus1(data)
  }else if(reconstruction=="Icshape"){
    statistic=Icshape(data)
  }else if(reconstruction=="Logform"){
    statistic=Logform(data)
  }else{
    ;
  }
}


#Calculation the SNR
SNR.Calculation<-function(mean_value,std_value){
  SNR=0;
  SNR=mean_value/std_value;
  return (SNR)
}