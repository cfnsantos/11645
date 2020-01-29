el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
elec <- ts(el, start = c(1948, 1), frequency = 12)
plot(log(elec))
t=seq(1,nrow(elec))
det_seas=cbind(cos(0*t),cos(pi*t))
for (j in 1:5) {det_seas=cbind(det_seas,cos(2*pi*t*j/52),sin(2*pi*t*j/52))}
end1 <-log(elec)
exo1 <-cbind(t,det_seas)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elec_e=log(elec)-exo1%*%rho
elec_f=exo1%*%rho
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_f <- ts(elec_f, start = c(1948, 1), frequency = 12)
ser=cbind(log(elec),elec_e,elec_f)
ts.plot(log(elec),elec_f,col=1:2)
acf(log(elec),lag.max=36)
acf(diff(log(elec),1),lag.max=36)
pacf(diff(log(elec),1),lag.max=36)
acf(diff(diff(log(elec),1),12),lag.max=36)
pacf(diff(diff(log(elec),1),12),lag.max=36)
plot(diff(diff(log(elec),1),12))
m1 <- arima(log(elec), orde=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
tsdiag(m1)
library(TSA)
detectAO(m1)
detectIO(m1)
m2 <- arimax(log(elec), orde=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), io=c(62))
tsdiag(m2)
plot(log(elec))
ele=el[165:708,1]
elec2 <- ts(ele, start = c(1970, 1), frequency = 12)
plot(elec2)
acf(diff(diff(log(elec2),1),12), lag.max = 36)
pacf(diff(diff(log(elec2),1),12), lag.max = 36)
m3 <- arima(log(elec2), orde=c(0,1,1),seasonal=list(order=c(0,1,0), period=12))
tsdiag(m3)
res_m3 <- residuals(m3)
acf(res_m3,lag.max = 36)
pacf(res_m3,lag.max = 36)
m4 <- arima(log(elec2), orde=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
tsdiag(m4)
res_m4 <- residuals(m4)
acf(res_m4,lag.max = 36)
pacf(res_m4,lag.max = 36)
Box.test(res_m4, lag = 1, type = "Ljung")    
for (i in 1:36){
print(Box.test(res_m4, lag = i, type = "Ljung"))        
}
detectAO(m4)
detectIO(m4)
library(forecast)
maut <-auto.arima(log(elec2),d=1,D=1)
tsdiag(maut)
res_maut<-residuals(maut)
for (i in 1:36){
  print(Box.test(res_maut, lag = i, type = "Ljung"))        
}
for_m4<-predict(m4,n.ahead=24)
for_maut<-predict(maut,n.ahead=24)
kk1 <-for_m4$pred[1:24]
kk2 <-for_maut$pred[1:24]
lelec2_f<-c(log(ele),kk1)
lelec2_fa<-c(log(ele),kk2)
lelec2_f <- ts(lelec2_f, start = c(1970, 1), frequency = 12)
lelec2_fa <- ts(lelec2_fa, start = c(1970, 1), frequency = 12)
ts.plot(lelec2_f,lelec2_fa,col=1:2)
ell<-ele[1:520]
elec3 <- ts(ell, start = c(1970, 1), frequency = 12)
m_4 <- arima(log(elec3), orde=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
m_aut <-auto.arima(log(elec2),d=1,D=1)
f_m4<-predict(m_4,n.ahead=24)
f_maut<-predict(m_aut,n.ahead=24)
kk1 <-f_m4$pred[1:24]
kk2 <-f_maut$pred[1:24]
ori<-log(ele[521:544])
dis1 <-ori-kk1
dis2 <-ori-kk2
erc1 <-t(dis1)%*%dis1
erc2 <-t(dis2)%*%dis2
ea1 <-sum(abs(dis1))
ea2 <-sum(abs(dis2))


