#Calculation method for the standard deviation
Std.Calculation<-function(data,reconstruction="Difference",method="Formula",replicates=0){
  std=0;
  if(method=="Bootstrap"){
    if(reconstruction=="Difference"){
      std=Difference.Boot(data,replicates)
    }else if(reconstruction=="Ratio"){
      std=Ratio.Boot(data,replicates)
    }else if(reconstruction=="Ratio.Minus"){
      std=Ratio.Minus.Boot(data,replicates)
    }else if(reconstruction=="Icshape"){
      std=Icshape.Boot(data,replicates)
    }else if(reconstruction=="Logform"){
      std=Logform.Boot(data,replicates)
    }else{
      ;
    }
  }else if(method=="Samples"){
    if(reconstruction=="Difference"){
      std=Difference.Replicates(data)
    }else if(reconstruction=="Ratio"){
      std=Ratio.Replicates(data)
    }else if(reconstruction=="Ratio.Minus"){
      std=Ratio.Minus.Replicates(data)
    }else if(reconstruction=="Icshape"){
      std=Icshape.Replicates(data)
    }else if(reconstruction=="Logform"){
      std=Logform.Replicates(data)
    }else{
      ;
    }
  }else if(method=="Formula"){
    if(reconstruction=="Difference"){
      std=Std.Difference(data,replicates)
    }else if(reconstruction=="Ratio"){
      std=Std.Ratio(data,replicates)
    }else if(reconstruction=="Ratio.Minus"){
      std=Std.Ratio.Minus(data,replicates)
    }else if(reconstruction=="Icshape"){
      std=Std.Icshape(data,replicates)
    }else if(reconstruction=="Logform"){
      std=Logform.Replicates(data,replicates)
    }else{
      ;
    }
  }else{
    ;
  }
    return (std)
}