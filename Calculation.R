#To calculate the statistics for each method
Return.Statistic<-function(data,reconstruction){
  statistic=0
  if(reconstruction=="Difference"){
    statistic=Difference(data)
  }else if(reconstruction=="Ratio"){
    statistic=Ratio(data)
  }else if(reconstruction=="Ratio.Minus"){
    statistic=Ratio.Minus(data)
  }else if(reconstruction=="Icshape"){
    statistic=Icshape(data)
  }else if(reconstruction=="Logform"){
    statistic=Logform(data)
  }else{
    ;
  }
  return (statistic)
}