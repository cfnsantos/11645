el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
elec <- ts(el, start = c(1948, 1), frequency = 12)
plot(log(elec))
t=seq(1,nrow(elec))
det_seas=cbind(cos(0*t),cos(pi*t))
for (j in 1:5) {det_seas=cbind(det_seas,cos(2*pi*t*j/12),sin(2*pi*t*j/12))}
end1 <-log(elec)
exo1 <-cbind(t,t^2,det_seas)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elec_e=log(elec)-exo1%*%rho
elec_f=exo1%*%rho
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_f <- ts(elec_f, start = c(1948, 1), frequency = 12)
ser=cbind(log(elec),elec_e,elec_f)
ts.plot(log(elec),elec_f,col=1:2)
acf(log(elec),lag.max=36)
pacf(log(elec),lag.max=36)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(log(elec),ylab='')
acf(log(elec),main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(log(elec),main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
spectrum(log(elec), method="ar")
spectrum(log(elec), kernel("fejer",50,r=6))
spectrum(log(elec))
par(op)



m1 <- arima(log(elec), orde=c(1,0,0))
acf(diff(log(elec),1),lag.max=36)
pacf(diff(log(elec),1),lag.max=36)
m1 <- arima(log(elec), orde=c(0,1,0),seasonal=list(order=c(1,0,0), period=12))

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(diff(log(elec),1),ylab='')
acf(diff(log(elec),1),main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(diff(log(elec),1),main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
spectrum(diff(log(elec),1), method="ar")
spectrum(diff(log(elec),1), kernel("fejer",25,r=6))
spectrum(diff(log(elec),1))
par(op)



acf(diff(diff(log(elec),1),12),lag.max=36)
pacf(diff(diff(log(elec),1),12),lag.max=36)
plot(diff(diff(log(elec),1),12))

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(diff(diff(log(elec),1),12),ylab='')
acf(diff(diff(log(elec),1),12),main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(diff(diff(log(elec),1),12),main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
spectrum(diff(diff(log(elec),1),12), method="ar")
spectrum(diff(diff(log(elec),1),12), kernel("fejer",25,r=6))
spectrum(diff(diff(log(elec),1),12))
par(op)

m1 <- arima(log(elec), orde=c(0,1,1),,seasonal=list(order=c(0,1,0), period=12))
res_m1<-residuals(m1)


op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(res_m1,ylab='')
acf(res_m1,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(res_m1,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
spectrum(res_m1, method="ar")
spectrum(res_m1, kernel("fejer",25,r=6))
spectrum(res_m1)
par(op)



m1 <- arima(log(elec), orde=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
res_m1<-residuals(m1)

acf(res_m1,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(res_m1,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
spectrum(res_m1, method="ar")
spectrum(res_m1, kernel("fejer",25,r=6))
spectrum(res_m1)
tsdiag(m1)

