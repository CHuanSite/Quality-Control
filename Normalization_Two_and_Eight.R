#To use the 2% and 8% method to do the normalization
Normalization.Two.And.Eight<-function(item){
  Normalized.item=0
  item.two.percent<-quantile(na.omit(item),0.98)
  item.ten.percent<-quantile(na.omit(item),0.90)
  item.mean.ten<-mean(item[which(item>=item.ten.percent&item<=item.two.percent)])
  Normalized.item=item/item.mean.ten
  return (Normalized.item)
}