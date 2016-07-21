#To Calcualte the CQI for every method
CQI.Calculation.Difference<-function(data){
  CQI=0;
  C.PLUS.desired=0;
  d=Cal.data(data)
  percentage.error=0.4
  z.value=qnorm(0.975)
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-d$X.MINUS/d$C.MINUS
  Variance=(Beta*percentage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired=((Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))+Gamma*(1-Gamma)/s)/Variance
  CQI=C.PLUS.desired/d$C.PLUS
  return (CQI)
}

CQI.Calculation.Ratio<-function(data){
  d=Cal.data(data)
  percentage.error=0.4
  z.value=qnorm(0.975)
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=(d$X.PLUS/d$C.PLUS)/(d$X.MINUS/d$C.MINUS)
  Var=(Beta*percentage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2)+1/Var*(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*s)
  return (C.PLUS.desired/d$C.PLUS)
}

CQI.Calculation.Ratio.Minus<-function(data){
  d=Cal.data(data)
  percentage.error=0.4
  z.value=qnorm(0.975)
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=(d$X.PLUS/d$C.PLUS)/(d$X.MINUS/d$C.MINUS)-1
  Var=(Beta*percentage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma*2)+1/Var*(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*s)
  return (C.PLUS.desired/d$C.PLUS)
}

CQI.Calculation.Icshape<-function(data){
  alpha=0.25
  d=Cal.data(data)
  percentage.error=0.4
  z.value=qnorm(0.975)
  Gamma=d$X.MINUS/d$C.MINUS
  Beta=d$X.PLUS/d$C.PLUS-alpha*d$X.MINUS/d$C.MINUS
  Var=(Beta*percentage.error/z.value)^2
  s=d$C.MINUS/d$C.PLUS
  C.PLUS.desired<-1/Var*((Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Gamma))/s^2+alpha^2*Gamma*(1-Gamma)/s)
  return (C.PLUS.desired/d$C.PLUS)
}

