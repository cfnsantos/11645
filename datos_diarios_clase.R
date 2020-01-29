library(TSA)
ell <- read.table(file="C:\\pruebas_r\\elec_diario.dat", header=FALSE)
elma<-ell[1:5020,1]
elme<-ell[1:5020,2]
elmaa <-matrix(data=elma,ncol=1)
elmee <-matrix(data=elme,ncol=2)
elma <- ts(elmaa, start = c(1994, 1), frequency = 365)
elme <- ts(elmee, start = c(1994, 1), frequency = 365)
plot(log(elma))
t=seq(1,nrow(elma))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
end1 <-log(elma)
exo1 <-cbind(det_seas,t,t^2)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elma_e=log(elma)-exo1%*%rho
elma_f=exo1%*%rho
elma_e <- ts(elma_e, start = c(1994, 1), frequency = 365)
elma_f <- ts(elma_f, start = c(1994, 1), frequency = 365)
ts.plot(log(elma),elma_f,col=1:2)
ts.plot(elma_e)
acf(elma_e,lag.max=28)
pacf(elma_e,lag.max=28)
acf(diff(elma_e,7),lag.max=28)
pacf(diff(elma_e,7),lag.max=28)
m1=arima(elma_e,order=c(0,0,0),seasonal = list(order = c(0,1,1), period = 7))
res_m1=residuals(m1)
acf(res_m1,lag.max=28)
pacf(res_m1,lag.max=28)

m2=arima(elma_e,order=c(2,0,1),seasonal = list(order = c(0,1,1), period = 7))
res_m2=residuals(m2)
acf(res_m2,lag.max=28)
pacf(res_m2,lag.max=28)


tsdiag(m2)

spectrum(res_m2)
spectrum(res_m2,method="ar")
spectrum(res_m2, kernel("fejer",30,r=6))

f_m2=fitted(m2)


m3=arima(elma_e,order=c(1,0,1),seasonal = list(order = c(0,1,1), period = 7))
res_m3=residuals(m3)
acf(res_m3,lag.max=28)
pacf(res_m3,lag.max=28)

tsdiag(m3)


spectrum(res_m3)
spectrum(res_m3,method="ar")
spectrum(res_m3, kernel("fejer",30,r=6))

f_m3=fitted(m3)



ts.plot(elma_e,f_m2,col=1:2)
ff_m2=f_m2+elma_f
ts.plot(log(elma),ff_m2,col=1:2)
ff_m2_er=log(elma)-ff_m2
ts.plot(ff_m2_er)


ts.plot(elma_e,f_m3,col=1:2)
ff_m3=f_m3+elma_f
ts.plot(log(elma),ff_m3,col=1:2)
ff_m3_er=log(elma)-ff_m3
ts.plot(ff_m3_er)
