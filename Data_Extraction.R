#Extract the data from the original dataset and calculate the parameters in the plus channel and minus channel
Cal.data<-function(data){
  C.PLUS=0
  C.MINUS=0
  C.PLUS.1=0;
  C.MINUS.1=0;
  C.PLUS.2=0;
  C.MINUS.2=0;
  data.calculation=0
  d<-data[which(data[,1]>0),]
  #d<-data
  rownames(d)<-d[,1]
  X.PLUS<-d[,4]
  X.MINUS<-d[,5]
  X.PLUS.1<-d[,6]
  X.MINUS.1<-d[,7]
  X.PLUS.2<-d[,8]
  X.MINUS.2<-d[,9]
  # for ( i in 1:length(d[,1])){
  #   C.PLUS[i]=sum(X.PLUS[1:i])
  #   C.MINUS[i]=sum(X.MINUS[1:i])
  #   C.PLUS.1[i]=sum(X.PLUS.1[1:i])
  #   C.MINUS.1[i]=sum(X.MINUS.1[1:i])
  #   C.PLUS.2[i]=sum(X.PLUS.2[1:i])
  #   C.MINUS.2[i]=sum(X.MINUS.2[1:i])
  # }
  C.MINUS<-d[,10]+X.MINUS
  C.PLUS<-d[,11]+X.PLUS
  C.MINUS.1<-d[,12]+X.MINUS.1
  C.PLUS.1<-d[,13]+X.PLUS.1
  C.MINUS.2<-d[,14]+X.MINUS.2
  C.PLUS.2<-d[,15]+X.PLUS.2
  data.calculation=data.frame(X.PLUS,X.MINUS,X.PLUS.1,X.MINUS.1,X.PLUS.2,X.MINUS.2,C.PLUS,C.PLUS.1,C.PLUS.2,C.MINUS,C.MINUS.1,C.MINUS.2)
  return (data.calculation)
}