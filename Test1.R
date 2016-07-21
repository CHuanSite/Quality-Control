#Test the Program
s1<-QC(shape2.coveragelist$samI,"Difference","Formula","No")
s2<-QC(shape2.coveragelist$aden,"Icshape","Bootstrap","No")
s3<-QC(shape2.coveragelist$aden,"Icshape","Samples","No")

plot(s2$Standard.Deviation,main="The standard deviation of the Bootstrap",type="l",col="yellow")
plot(s3$Standard.Deviation,main="The standard deviation of the Samples",type="l",col="green")
plot(s4$Standard.Deviation,main="The standard deviation of the formula",type="l",col="blue")
s<-cbind(s1$Standard.Deviation,s2$Standard.Deviation)
#plot((s1$Standard.Deviation-s3$Standard.Deviation)/s3$Standard.Deviation,main="The standard deviation of the Bootstrap",type="l",col="yellow")