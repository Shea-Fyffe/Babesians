hd<-read.csv("Heart Disease.csv")
hdsex<-lm(TenYearCHD~male, hd)
summary(hdsex)
hdmen<-subset(hd, male==1)
hdwomen<-subset(hd, male==0)
sum(hdmen$TenYearCHD==1)
nrow(hdmen)
sum(hdwomen$TenYearCHD == 1)
nrow(hdwomen)
pmenhd<-sum(hdmen$TenYearCHD==1)/nrow(hdmen)
print(pmenhd)
pwomenhd<-sum(hdwomen$TenYearCHD==1)/nrow(hdwomen)
print(pwomenhd)
prob<-sum(hd$TenYearCHD==1)/nrow(hd)
print(prob)
matrix=matrix(
  c(1476,343,2118,301),
  nrow=2,
  ncol=2)
colnames(matrix)<-c("N","Y")
rownames(matrix) <- c("M","F")
matrix
