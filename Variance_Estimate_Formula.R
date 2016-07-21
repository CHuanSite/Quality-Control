#To estimate the variance through Formula
Std.Difference<-function(data,replicates){
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-X.PLUS/C.PLUS-X.MINUS/C.MINUS;
  Gamma<-X.MINUS/C.MINUS;
  #Beta<-Normalization.Two.And.Eight(Beta)
  #Gamma<-Normalization.Two.And.Eight(Gamma)
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/C.PLUS+Gamma*(1-Gamma)/C.MINUS;
  #Var.Beta<-Normalization.Two.And.Eight(Var.Beta)
  return (sqrt(Var.Beta))
}

Std.Ratio<-function(data,replicates){
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-(X.PLUS/C.PLUS)/(X.MINUS/C.MINUS);
  Gamma<-X.MINUS/C.MINUS;
  X.PLUS.Expect=C.PLUS*(Beta+Gamma*(1-Beta))
  X.PLUS.Variation=C.PLUS*(Beta+Gamma*(1-Beta))*(1-Beta+Gamma*(1-Beta))
  X.PLUS.Expect.SQUARE=X.PLUS.Variation+X.PLUS.Expect^2
  X.MINUS.Expect=C.MINUS*Gamma
  X.MINUS.Varation=C.MINUS*Gamma*(1-Gamma)
  X.MINUS.Expect.SQUARE=X.MINUS.Varation+X.MINUS.Expect^2
  #Var.Beta=(C.MINUS/C.PLUS)^2*(X.PLUS.Expect.SQUARE/X.MINUS.Expect.SQUARE*(X.PLUS.Variation/X.PLUS.Expect.SQUARE+X.MINUS.Varation/X.MINUS.Expect.SQUARE))
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))/(Gamma^2*C.PLUS)+(Beta+Gamma*(1-Beta))^2*(1-Gamma)/(Gamma^3*C.MINUS)
  return (sqrt(Var.Beta))
}

Std.Ratio.Minus<-function(data,replicates){
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-(X.PLUS/C.PLUS)/(X.MINUS/C.MINUS)-1;
  Gamma<-X.MINUS/C.MINUS;
  Var.Beta<-(Beta+1+Gamma*(-(Beta)))*(-Beta-Gamma*(-(Beta)))/(Gamma^2*C.PLUS)+(Beta+1+Gamma*(-Beta))^2*(1-Gamma)/(Gamma^3*C.MINUS)
  return (sqrt(Var.Beta))
}

Std.Icshape<-function(data,replicates){
  alpha=0.25
  d<-Cal.data(data);
  Output=Replicates.Output(d,replicates)
  X.PLUS=Output$X.PLUS
  X.MINUS=Output$X.MINUS
  C.PLUS=Output$C.PLUS
  C.MINUS=Output$C.MINUS
  Beta<-X.PLUS/C.MINUS-alpha*X.MINUS/C.MINUS;
  Gamma<-X.MINUS/C.MINUS;
  Var.Beta<-(Beta+Gamma*(1-Beta))*(1-Beta-Gamma*(1-Beta))*C.PLUS/(C.MINUS)^2+alpha^2*Gamma*(1-Gamma)/C.MINUS;
  return (sqrt(Var.Beta))
}
#This one is unknown
# Std.Logfrom<-function(data){
#   return (Logform.Replicates(data))   
# }