#To decide which method to use in order to do the normalization
Normalization.Determination<-function(Normalization,item){
  Normalization.data=0;
  if(Normalization=="Two.And.Eight"){
    Normalization.data=Normalization.Two.And.Eight(item)
  }else if(Normalization=="Boxplot"){
    Normalization.data=Normalization.Boxplot(item)
  }else{
    Normalization.data=item;
  }
  return (Normalization.data)
}