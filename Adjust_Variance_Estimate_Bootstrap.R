Difference.Boot<-function(data,replicates){
  Beta=0;
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  C.PLUS=C.PLUS
  C.MINUS=C.MINUS
  X.PLUS=X.PLUS
  X.MINUS=X.MINUS

  
  for ( i in 1:100){
    for(j in 1:sample.size){
      sample.index<-sample(c(0:1),C.PLUS[j],replace=TRUE,prob=c((1-X.PLUS[j]/C.PLUS[j]),X.PLUS[j]/C.PLUS[j]))
      resample.X.PLUS=sum(sample.index)
      sample.index<-sample(c(0:1),C.MINUS[j],replace=TRUE,prob=c((1-X.MINUS[j]/C.MINUS[j]),X.MINUS[j]/C.MINUS[j]))
      resample.X.MINUS=sum(sample.index)
      
      Beta[j]=resample.X.PLUS/C.PLUS[j]-resample.X.MINUS/C.MINUS[j]
    }
    print (i)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}



Ratio.Boot<-function(data,replicates){
  Beta=0;
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  
  for ( i in 1:100){
    for(j in 1:sample.size){
      sample.index<-sample(c(0:1),C.PLUS[j],replace=TRUE,prob=c((1-X.PLUS[j]/C.PLUS[j]),X.PLUS[j]/C.PLUS[j]))
      resample.X.PLUS=sum(sample.index)
      sample.index<-sample(c(0:1),C.MINUS[j],replace=TRUE,prob=c((1-X.MINUS[j]/C.MINUS[j]),X.MINUS[j]/C.MINUS[j]))
      resample.X.MINUS=sum(sample.index)
      
      Beta[j]=(resample.X.PLUS/C.PLUS[j])/(resample.X.MINUS/C.MINUS[j])
    }
    print (i)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Ratio.Minus.Boot<-function(data,replicates){
  Beta=0;
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    for(j in 1:sample.size){
      sample.index<-sample(c(0:1),C.PLUS[j],replace=TRUE,prob=c((1-X.PLUS[j]/C.PLUS[j]),X.PLUS[j]/C.PLUS[j]))
      resample.X.PLUS=sum(sample.index)
      sample.index<-sample(c(0:1),C.MINUS[j],replace=TRUE,prob=c((1-X.MINUS[j]/C.MINUS[j]),X.MINUS[j]/C.MINUS[j]))
      resample.X.MINUS=sum(sample.index)
      Beta[j]=(resample.X.PLUS/C.PLUS[j])/(resample.X.MINUS/C.MINUS[j])-1
    }
    print (i)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Icshape.Boot<-function(data,replicates){
  Beta=0;
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  alpha=0.25
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    for(j in 1:sample.size){
      sample.index<-sample(c(0:1),C.PLUS[j],replace=TRUE,prob=c((1-X.PLUS[j]/C.PLUS[j]),X.PLUS[j]/C.PLUS[j]))
      resample.X.PLUS=sum(sample.index)
      sample.index<-sample(c(0:1),C.MINUS[j],replace=TRUE,prob=c((1-X.MINUS[j]/C.MINUS[j]),X.MINUS[j]/C.MINUS[j]))
      resample.X.MINUS=sum(sample.index)
      Beta[j]=(resample.X.PLUS-alpha*resample.X.MINUS)/C.MINUS[j]
    }
    print (i)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}


Logform.Boot<-function(data,replicates){
  Beta=0;
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    for(j in 1:sample.size){
      sample.index<-sample(c(0:1),C.PLUS[j],replace=TRUE,prob=c((1-X.PLUS[j]/C.PLUS[j]),X.PLUS[j]/C.PLUS[j]))
      resample.X.PLUS[j]=sum(sample.index)
      sample.index<-sample(c(0:1),C.MINUS[j],replace=TRUE,prob=c((1-X.MINUS[j]/C.MINUS[j]),X.MINUS[j]/C.MINUS[j]))
      resample.X.MINUS[j]=sum(sample.index)
      #Beta[j]=Beta=log(resample.X.PLUS+1)/(sum(log(resample.X.PLUS+1)))*sample.size-log(resample.X.MINUS+1)/(sum(log(resample.X.MINUS+1)))*sample.size
    }
    Beta=log(resample.X.PLUS+1)/(sum(log(resample.X.PLUS+1)))*sample.size-log(resample.X.MINUS+1)/(sum(log(resample.X.MINUS+1)))*sample.size
    print (i)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}