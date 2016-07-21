#To decide which method to use to calcualte the CQI
CQI.Calculation<-function(data,reconstruction){
  CQI.PLUS.desired=0;
  if(reconstruction=="Difference"){
    CQI.PLUS.desired=CQI.Calculation.Difference(data)
  }else if(reconstruction=="Ratio"){
    CQI.PLUS.desired=CQI.Calculation.Ratio(data)
  }else if(reconstruction=="Ratio.Minus"){
    CQI.PLUS.desired=CQI.Calculation.Ratio.Minus(data)
  }else if(reconstruction=="Icshape"){
    CQI.PLUS.desired=CQI.Calculation.Icshape(data)
  }else if(reconstruction=="Logform"){
    CQI.PLUS.desired=0;#The form of the Log form has not been decided yet
  }else{
    ;
  }
  return (CQI.PLUS.desired)
}