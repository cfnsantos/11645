library(TSA)
library(KFAS)
ell <- read.table(file="c:\\pruebas_r\\elec_diario.dat", header=FALSE)
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

elma2 <-elma[1:4655]

elma2 <- ts(elma2, start = c(1994, 1), frequency = 365)

t=seq(1,4655)
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
end1 <-log(elma2)
exo1 <-cbind(det_seas,t,t^2)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elma2_e=log(elma2)-exo1%*%rho
elma2_f=exo1%*%rho
elma2_e <- ts(elma2_e, start = c(1994, 1), frequency = 365)

m3d=arima(elma2_e,order=c(1,0,1),seasonal = list(order = c(0,1,1), period = 7))

f_m3d<-predict(m3d,n.ahead=365)

kk2 <-f_m3d$pred[1:365]

t2<-seq(4656,5020)

det_seas2=cos(0*t2)
for (j in 1:182) {det_seas2=cbind(det_seas2,cos(2*pi*t2*j/365),sin(2*pi*t2*j/365))}

exo2 <-cbind(det_seas2,t2,t2^2)

kk1<-exo2%*%rho
kkt <-kk1+kk2

kkt<- ts(kkt, start = c(1, 1), frequency = 365)

ori<-log(elma[4655:5020])
ori<- ts(ori, start = c(1, 1), frequency = 365)

ts.plot(ori,kkt,col=1:2)

ori2 <- exp(ori)
kkt2 <- exp(kkt)

ts.plot(ori2,kkt2,col=1:2)


dis1 <-ori2-kkt2
erc1 <-(t(dis1)%*%dis1)/365
ea1 <-sum(abs(dis1))/365
eap1 <-(sum(abs(dis1)/ori2))/365

mod2<-SSModel(elma2_e~SSMtrend(degree=2,Q=list(matrix(NA),matrix(NA)))+SSMseasonal(period=7,sea.type = "trigonometric",Q=NA),H=NA)
fit_mod2 <-fitSSM(mod2,inits=c(0,0,0,0,0,0,0,0,0),method="BFGS")
out_mod2 <-KFS(fit_mod2$model)
print(fit_mod2$model$H)
print(fit_mod2$model$Q)

h11=fit_mod2$model$H
QQ=matrix(fit_mod2$model$Q,8,8)

mod_pred<-SSModel(elma2_e~SSMtrend(degree=2,Q=list(matrix(QQ[1,1]),matrix(QQ[2,2])))+SSMseasonal(period=7,sea.type = "trigonometric",Q=QQ[3,3]),H=h11)
pred_mod2<-predict(mod_pred,n.ahead=365)

kkt_2 <-kk1+pred_mod2

kkt_2<- ts(kkt_2, start = c(1, 1), frequency = 365)

ts.plot(ori,kkt_2,col=1:2)

kktt_2 <- exp(kkt_2)
ts.plot(ori2,kktt_2,col=1:2)

dis2 <-ori2-kktt_2
erc2 <-(t(dis2)%*%dis2)/365
ea2 <-sum(abs(dis2))/365
eap2 <-(sum(abs(dis2)/ori2))/365


