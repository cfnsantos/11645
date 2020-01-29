library(TSA)
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
acf(elma2_e,lag.max=28)
pacf(elma2_e,lag.max=28)
acf(diff(elma2_e,7),lag.max=28)
pacf(diff(elma2_e,7),lag.max=28)
m1=arima(elma2_e,order=c(2,0,1),seasonal = list(order = c(0,1,0), period = 7))
res_m1=residuals(m1)
acf(res_m1,lag.max=28)
pacf(res_m1,lag.max=28)
m2=arima(elma2_e,order=c(2,0,1),seasonal = list(order = c(0,1,1), period = 7))
res_m2=residuals(m2)
acf(res_m2,lag.max=28)
pacf(res_m2,lag.max=28)
f_m2=fitted(m2)
ts.plot(elma2_e,f_m2,col=1:2)
ff_m2=f_m2+elma2_f
ts.plot(log(elma2),ff_m2,col=1:2)
ff_m2_er=log(elma2)-ff_m2
ts.plot(ff_m2_er)
f_m2<-predict(m2,n.ahead=365)

t=seq(1,nrow(elma))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
exo11 <-cbind(det_seas,t,t^2)
exo1_f<-exo11[(5020-365+1):5020,]
ff_seas<-array(0,365)
for (i in 1:365){
ff_seas[i]=exo1_f[i,]%*%rho
}
ff_tot<-ff_seas+f_m2$pred

ts.plot(log(elma[(5020-365+1):5020]))
ts.plot(ff_tot,log(elma[(5020-365+1):5020]),col=1:2)



