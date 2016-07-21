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

Icshape.Replicates<-function(data){
  alpha=0.25
  D<-Cal.data(data)
  statistic<-(D$X.PLUS-alpha*D$X.MINUS)/D$C.MINUS;
  statistic.1<-(D$X.PLUS.1-alpha*D$X.MINUS.1)/D$C.MINUS.1;
  statistic.2<-(D$X.PLUS.2-alpha*D$X.MINUS.2)/D$C.MINUS.2;
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}

Logform.Replicates<-function(data){
  D<-Cal.data(data)
  statistic<-log(D$X.PLUS+1)/(sum(log(D$X.PLUS+1)))*nrow(D)-log(D$X.MINUS+1)/(sum(log(D$X.MINUS+1)))*nrow(D)
  statistic.1<-log(D$X.PLUS.1+1)/(sum(log(D$X.PLUS.1+1)))*nrow(D)-log(D$X.MINUS.1+1)/(sum(log(D$X.MINUS.1+1)))*nrow(D)
  statistic.2<-log(D$X.PLUS.2+1)/(sum(log(D$X.PLUS.2+1)))*nrow(D)-log(D$X.MINUS.2+1)/(sum(log(D$X.MINUS.2+1)))*nrow(D)                          
  std=apply(data.frame(statistic,statistic.1,statistic.2),1,sd)
  return (std)
}