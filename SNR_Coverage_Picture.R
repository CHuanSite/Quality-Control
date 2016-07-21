#out put the SNR of each RNA
RNAS<-c("aden","cdGMP","hepCIRES","rRNA5","samI","tetra.intronI","tpp.ribo","tRNA.phe")
SNR.STORE=0
SNR.STORE.1=0
SNR.STORE.2=0
C.PLUS.STORE=0
C.MINUS.STORE=0
C.PLUS.STORE.1=0
C.MINUS.STORE.1=0
C.PLUS.STORE.2=0
C.MINUS.STORE.2=0
Pearson.Correlation=0
Pearson.Correlation.1=0
Pearson.Correlation.2=0
Pearson.Correlation.COM=0
MEAN.SNR=0
MEAN.SNR.1=0
MEAN.SNR.2=0
MEAN.SNR.COM=0

X.PLUS.STORE=0
X.MINUS.STORE=0
X.PLUS.STORE.1=0
X.MINUS.STORE.1=0
X.PLUS.STORE.2=0
X.MINUS.STORE.2=0

for( i in 1:length(RNAS)){
  s<-QC(data.frame(shape2.coveragelist[i]),"Ratio.Minus","Bootstrap","Two.And.Eight")
  SNR.STORE[i]=s$SNR.MEAN
  SNR.STORE.1[i]=s$SNR.MEAN.1
  SNR.STORE.2[i]=s$SNR.MEAN.2
  C.PLUS.STORE[i]=s$C.PLUS.MEAN
  C.MINUS.STORE[i]=s$C.MINUS.MEAN
  C.PLUS.STORE.1[i]=s$C.PLUS.MEAN.1
  C.MINUS.STORE.1[i]=s$C.MINUS.MEAN.1
  C.PLUS.STORE.2[i]=s$C.PLUS.MEAN.2
  C.MINUS.STORE.2[i]=s$C.MINUS.MEAN.2
  X.PLUS.STORE[i]=s$X.PLUS.MEAN
  X.MINUS.STORE[i]=s$X.MINUS.MEAN
  X.PLUS.STORE.1[i]=s$X.PLUS.MEAN.1
  X.MINUS.STORE.1[i]=s$X.MINUS.MEAN.1
  X.PLUS.STORE.2[i]=s$X.PLUS.MEAN.2
  X.MINUS.STORE.2[i]=s$X.MINUS.MEAN.2
  MEAN.SNR[i]=mean(na.omit(c(s$SNR,s$SNR.1)))
  MEAN.SNR.1[i]=mean(na.omit(c(s$SNR,s$SNR.2)))
  MEAN.SNR.2[i]=mean(na.omit(c(s$SNR.1,s$SNR.2)))
  Pearson.Correlation[i]=cor(na.omit(cbind(s$Beta,s$Beta.1)))[1,2]
  Pearson.Correlation.1[i]=cor(na.omit(cbind(s$Beta,s$Beta.2)))[1,2]
  Pearson.Correlation.2[i]=cor(na.omit(cbind(s$Beta.1,s$Beta.2)))[1,2]
}
SNR.STORE.COM=c(SNR.STORE,SNR.STORE.1,SNR.STORE.2)
C.PLUS.COM=c(C.PLUS.STORE,C.PLUS.STORE.1,C.PLUS.STORE.2)
C.MINUS.COM=c(C.MINUS.STORE,C.MINUS.STORE.1,C.MINUS.STORE.2)
X.PLUS.COM=c(X.PLUS.STORE,X.PLUS.STORE.1,X.PLUS.STORE.2)
X.MINUS.COM=c(X.MINUS.STORE,X.MINUS.STORE.1,X.MINUS.STORE.2)
Pearson.Correlation.COM=c(Pearson.Correlation,Pearson.Correlation.1,Pearson.Correlation.2)
MEAN.SNR.COM=c(MEAN.SNR,MEAN.SNR.1,MEAN.SNR.2)
plot(SNR.STORE.COM,X.PLUS.COM)
Pearson.Correlation.COM=c(Pearson.Correlation,Pearson.Correlation.1,Pearson.Correlation.2)


SNR.1
Pearson.1
SNR.2
Pearson.2

