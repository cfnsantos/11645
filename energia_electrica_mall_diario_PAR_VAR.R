library(TSA)
library(vars)
ell <- read.table(file="C:\\pruebas_r\\elec_diario.dat", header=FALSE)
elma<-ell[1:5020,1]
elme<-ell[1:5020,2]
elmaa <-matrix(data=elma,ncol=1)
elmee <-matrix(data=elme,ncol=2)
elma <- ts(elmaa, start = c(1994, 1), frequency = 365)
elme <- ts(elmee, start = c(1994, 1), frequency = 365)
plot(log(elma))
elmaa2<-elmaa[1:(5020-365)]
elmaa2 <-matrix(data=elmaa2,ncol=1)
elma2 <- ts(elmaa2, start = c(1994, 1), frequency = 365)
t=seq(1,nrow(elma2))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
end1 <-log(elma2)
exo1 <-cbind(det_seas,t,t^2)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elma2_e=log(elma2)-exo1%*%rho
elma2_f=exo1%*%rho
elma2_e <- ts(elma2_e, start = c(1994, 1), frequency = 365)
elma2_f <- ts(elma2_f, start = c(1994, 1), frequency = 365)
ts.plot(log(elma2),elma2_f,col=1:2)
ts.plot(elma2_e)
elma2e_v7<-matrix(data=elma2_e,nrow=((5020-365)/7),ncol = 7)

dat_var7<-data.frame(elma2e_v7)
colnames(dat_var7) <-c("s1","s2","s3","s4","s5","s6","s7")

var7_ord<-VARselect(dat_var7,lag.max=12,type="const")
m2<- VAR(dat_var7,p=9,type = "const")

serial1 <-serial.test(m2,lags.pt=28,type="PT.adjusted")
plot(serial1, names="s1")
plot(serial1, names="s2")
plot(serial1, names="s3")
plot(serial1, names="s4")
plot(serial1, names="s5")
plot(serial1, names="s6")
plot(serial1, names="s7")

for_VAR7<-predict(m2,n.ahead=53)

ff_var7<-cbind(for_VAR7$fcst$s1[,1],for_VAR7$fcst$s2[,1],for_VAR7$fcst$s3[,1],for_VAR7$fcst$s4[,1],for_VAR7$fcst$s5[,1],for_VAR7$fcst$s6[,1],for_VAR7$fcst$s7[,1])

f_var7<-matrix(data=ff_var7,ncol=1)

t=seq(1,nrow(elma))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
exo11 <-cbind(det_seas,t,t^2)
exo1_f<-exo11[(5020-365+1):5020,]
ff_seas<-array(0,365)
for (i in 1:365){
ff_seas[i]=exo1_f[i,]%*%rho
}
ff_tot<-ff_seas+f_var7[1:365]
ff_tot<-ts(ff_tot, start = c(1, 1), frequency = 365)
elma_ori<-ts(log(elma[(5020-365+1):5020]), start = c(1, 1), frequency = 365)
ts.plot(ff_tot,elma_ori,col=1:2)



