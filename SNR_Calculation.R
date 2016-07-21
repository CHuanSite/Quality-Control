#Calculation the SNR
SNR.Calculation<-function(mean_value,std_value){
  SNR=0;
  SNR=mean_value/std_value;
  return (SNR)
}