#Calculation method for the mean
Beta.Calculation<-function(data,reconstruction,method,replicates){
  Beta=0;
    if(reconstruction=="Difference"){
      Beta=Difference(data,replicates)
    }else if(reconstruction=="Ratio"){
      Beta=Ratio(data,replicates)
    }else if(reconstruction=="Ratio.Minus"){
      Beta=Ratio.Minus(data,replicates)
    }else if(reconstruction=="Icshape"){
      Beta=Icshape(data,replicates)
    }else if(reconstruction=="Logform"){
      Beta=Logform(data,replicates)
    }else{
      ;
    }
  return (Beta)
}