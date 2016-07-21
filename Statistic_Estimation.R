#To calculate the statistic of each form
Difference<-function(data,replicates){
    Beta=0
    d<-Cal.data(data);
    Output=Replicates.Output(d,replicates)
    X.PLUS=Output$X.PLUS
    X.MINUS=Output$X.MINUS
    C.PLUS=Output$C.PLUS
    C.MINUS=Output$C.MINUS
    Beta<-X.PLUS/C.PLUS-X.MINUS/C.MINUS
    return (Beta)
}

Ratio<-function(data,replicates){
  Beta=0
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-(X.PLUS/C.PLUS)/(X.MINUS/C.MINUS)
  Beta[which(Beta<1)]=0
  return (Beta)
}

Ratio.Minus<-function(data,replicates){
  Beta=0
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-(X.PLUS/C.PLUS)/(X.MINUS/C.MINUS)-1
  return (Beta)
}

Icshape<-function(data,replicates){
  alpha=0.25
  Beta=0
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-(X.PLUS-alpha*X.MINUS)/C.MINUS;

  return (Beta)
}

Logform<-function(data,replicates){
  Beta=0
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-log(X.PLUS+1)/(sum(log(X.PLUS+1)))*nrow(d)-log(X.MINUS+1)/(sum(log(X.MINUS+1)))*nrow(d)
  return (Beta)
}

