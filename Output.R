Replicates.Output<-function(data,replicates){
  X.PLUS=0;
  X.MINUS=0;
  C.PLUS=0;
  C.MINUS=0;
  d=data
  if(replicates==0){
    X.PLUS=d$X.PLUS
    X.MINUS=d$X.MINUS
    C.PLUS=d$C.PLUS
    C.MINUS=d$C.MINUS
  }else if(replicates==1){
    X.PLUS=d$X.PLUS.1
    X.MINUS=d$X.MINUS.1
    C.PLUS=d$C.PLUS.1
    C.MINUS=d$C.MINUS.1
  }else if(replicates==2){
    X.PLUS=d$X.PLUS.2
    X.MINUS=d$X.MINUS.2
    C.PLUS=d$C.PLUS.2
    C.MINUS=d$C.MINUS.2
  }else{
    X.PLUS=d$X.PLUS
    X.MINUS=d$X.MINUS
    C.PLUS=d$C.PLUS
    C.MINUS=d$C.MINUS
  }
  return (data.frame(X.PLUS,X.MINUS,C.PLUS,C.MINUS))
}