QC<-function(data,reconstruction="Difference",method="Parametric",normalization="none")
  
Mean_calculation<-function(data,reconstruction)
Std_calculation<-function(data,reconstruction,method)
SNR.Calculation<-function(mean_value,std_value)
        
Difference<-function(data)
Ratio<-function(data)
Difference_minus1<-function(data)
Icshape<-function(data)
                
Difference_Boot<-function(data,indices)
Ratio_Boot<-function(data,indices)
Difference_minus1_Boot<-function(data)
Icshape_Boot<-function(data)
                        
Difference_REP<-function(data)
Ratio_REP<-function(data)
Difference_minus1_REP<-function(data)
Icshape_REP<-function(data)