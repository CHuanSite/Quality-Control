#To estimate the variance through Bootstrap Method
Difference.Boot<-function(data,replicates){
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data)
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])

  for ( i in 1:100){
    sample.index<-sample(1:sample.size,sum(X.PLUS),replace=TRUE,prob=X.PLUS/sum(X.PLUS))
    Probability.PLUS=table(c(sample.index,1:sample.size))/sum(X.PLUS)-table(c(1:sample.size))/sum(X.PLUS)
    resample.X.PLUS=Probability.PLUS*sum(X.PLUS)
    sample.index<-sample(1:sample.size,sum(X.MINUS),replace=TRUE,prob=X.MINUS/sum(X.MINUS))
    Probability.MINUS=table(c(sample.index,1:sample.size))/sum(X.MINUS)-table(c(1:sample.size))/sum(X.MINUS)
    resample.X.MINUS=Probability.MINUS*sum(X.MINUS)
    for( j in 1:sample.size){
      resample.C.PLUS[j]=sum(resample.X.PLUS[1:j])
      resample.C.MINUS[j]=sum(resample.X.MINUS[1:j])
    }
    Beta=resample.X.PLUS/resample.C.PLUS-resample.X.MINUS/resample.C.MINUS
    Beta=Normalization.Two.And.Eight(Beta)#To normalize the Beta in the Bootstrap method
    Probability.Resample=cbind(Probability.Resample,Beta)
    print (i)
  }
  Probability.Resample=Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Ratio.Boot<-function(data,replicates){
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data)
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    sample.index<-sample(1:sample.size,sum(X.PLUS),replace=TRUE,prob=X.PLUS/sum(X.PLUS))
    Probability.PLUS=table(c(sample.index,1:sample.size))/sum(X.PLUS)-table(c(1:sample.size))/sum(X.PLUS)
    resample.X.PLUS=Probability.PLUS*sum(X.PLUS)
    
    sample.index<-sample(1:sample.size,sum(X.MINUS),replace=TRUE,prob=X.MINUS/sum(X.MINUS))
    Probability.MINUS=(table(c(sample.index,1:sample.size)))/sum(X.MINUS)-table(c(1:sample.size))/sum(X.MINUS)
    resample.X.MINUS=Probability.MINUS*sum(X.MINUS)
    for( j in 1:sample.size){
      resample.C.PLUS[j]=sum(resample.X.PLUS[1:j])
      resample.C.MINUS[j]=sum(resample.X.MINUS[1:j])
    }
    Beta=(resample.X.PLUS/resample.C.PLUS)/(resample.X.MINUS/resample.C.MINUS)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
    print (i)
  }
  Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Ratio.Minus.Boot<-function(data,replicates){
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data)
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    sample.index<-sample(1:sample.size,sum(X.PLUS),replace=TRUE,prob=X.PLUS/sum(X.PLUS))
    Probability.PLUS=table(c(sample.index,1:sample.size))/sum(X.PLUS)-table(c(1:sample.size))/sum(X.PLUS)
    resample.X.PLUS=Probability.PLUS*sum(X.PLUS)
    
    sample.index<-sample(1:sample.size,sum(X.MINUS),replace=TRUE,prob=X.MINUS/sum(X.MINUS))
    Probability.MINUS=(table(c(sample.index,1:sample.size)))/sum(X.MINUS)-table(c(1:sample.size))/sum(X.MINUS)
    resample.X.MINUS=Probability.MINUS*sum(X.MINUS)
    for( j in 1:sample.size){
      resample.C.PLUS[j]=sum(resample.X.PLUS[1:j])
      resample.C.MINUS[j]=sum(resample.X.MINUS[1:j])
    }
    Beta=(resample.X.PLUS/resample.C.PLUS)/(resample.X.MINUS/resample.C.MINUS)-1
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Icshape.Boot<-function(data,replicates){
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  alpha=0.25
  d<-Cal.data(data)
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    sample.index<-sample(1:sample.size,sum(X.PLUS),replace=TRUE,prob=X.PLUS/sum(X.PLUS))
    Probability.PLUS=table(c(sample.index,1:sample.size))/sum(X.PLUS)-table(c(1:sample.size))/sum(X.PLUS)
    resample.X.PLUS=Probability.PLUS*sum(X.PLUS)
    
    sample.index<-sample(1:sample.size,sum(X.MINUS),replace=TRUE,prob=X.MINUS/sum(X.MINUS))
    Probability.MINUS=(table(c(sample.index,1:sample.size)))/sum(X.MINUS)-table(c(1:sample.size))/sum(X.MINUS)
    resample.X.MINUS=Probability.MINUS*sum(X.MINUS)
    for( j in 1:sample.size){
      resample.C.PLUS[j]=sum(resample.X.PLUS[1:j])
      resample.C.MINUS[j]=sum(resample.X.MINUS[1:j])
    }
    Beta=(resample.X.PLUS-alpha*resample.X.MINUS)/resample.C.MINUS
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
    print (i)
  }
  Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

Logform.Boot<-function(data,replicates){
  std=0;
  Probability.Resample=0
  Probability.PLUS=0
  Probability.MINUS=0
  resample.X.PLUS=0
  resample.X.MINUS=0
  resample.C.PLUS=0
  resample.C.MINUS=0
  
  d<-Cal.data(data)
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  sample.size=length(d[,1])
  for ( i in 1:100){
    sample.index<-sample(1:sample.size,sum(X.PLUS),replace=TRUE,prob=X.PLUS/sum(X.PLUS))
    Probability.PLUS=table(c(sample.index,1:sample.size))/sum(X.PLUS)-table(c(1:sample.size))/sum(X.PLUS)
    resample.X.PLUS=Probability.PLUS*sum(X.PLUS)
    
    sample.index<-sample(1:sample.size,sum(X.MINUS),replace=TRUE,prob=X.MINUS/sum(X.MINUS))
    Probability.MINUS=(table(c(sample.index,1:sample.size)))/sum(X.MINUS)-table(c(1:sample.size))/sum(X.MINUS)
    resample.X.MINUS=Probability.MINUS*sum(X.MINUS)
    for( j in 1:sample.size){
      resample.C.PLUS[j]=sum(resample.X.PLUS[1:j])
      resample.C.MINUS[j]=sum(resample.X.MINUS[1:j])
    }
    Beta=log(resample.X.PLUS+1)/(sum(log(resample.X.PLUS+1)))*length(resample.X.PLUS)-log(resample.X.MINUS+1)/(sum(log(resample.X.MINUS+1)))*length(resample.X.PLUS)
    Beta=Normalization.Two.And.Eight(Beta)
    Probability.Resample=cbind(Probability.Resample,Beta)
  }
  Probability.Resample[,-1]
  std=apply(Probability.Resample,1,sd)
  return (std)
}

