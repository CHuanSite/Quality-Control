#To use the Boxplot method to do the Normalization
Normalization.Boxplot<-function(item){
  Normalized.item=0
  lower.bound<-quantile(na.omit(item),0.25)
  upper.bound<-quantile(na.omit(item),0.75)
  distance=upper.bound-lower.bound
  item[which(item>=upper.bound+distance*1.5)]=0
  item[which(item<=0)]=0
  mean.ten.percent=mean(item[item>=quantile(na.omit(item),0.9)])
  Normalized.item=item/mean.ten.percent
  return (Normalized.item)
}