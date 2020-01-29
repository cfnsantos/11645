library(TSA)
alpha0<-0.5
alpha1<-0.7
e <- rnorm(600,0,1)
sig2<- array(0,600)
x <- array(0,600)
for (i in 2:600){
sig2[i]=alpha0+alpha1*x[i-1]^2
x[i]=e[i]*sqrt(sig2[i])        

}
x<-x[101:600]
op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(x,ylab='')
acf(x,main="Autocorrelation x",lag=36)
pacf(x,main="Partial Autocorrelations x",lag=36 )
acf(x^2,main="Autocorrelation x^2",lag=36)
pacf(x^2,main="Partial Autocorrelations x^2",lag=36 )
McLeod.Li.test(y=x)
par(op)

m_1<-arima(x^2,order=c(1,0,0))

m1=garch(x,order=c(0,1))
McLeod.Li.test(y=m1$residuals)
gBox(m1,x=x,method='squared')


beta1<-0.4
alpha0<-0.5
alpha1<-0.5
e <- rnorm(600,0,1)
sig2<- array(0,600)
y <- array(0,600)
for (i in 2:600){
  sig2[i]=alpha0+alpha1*y[i-1]^2+beta1*sig2[i-1]
  y[i]=e[i]*sqrt(sig2[i])        
}
y<-y[101:600]
op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation y",lag=36)
pacf(y,main="Partial Autocorrelations y",lag=36 )
acf(y^2,main="Autocorrelation y^2",lag=36)
pacf(y^2,main="Partial Autocorrelations y^2",lag=36 )
McLeod.Li.test(y=y)
par(op)

m_2<-arima(y^2,order=c(1,0,1))

m2<-garch(y,order=c(1,2))
McLeod.Li.test(y=m2$residuals)
gBox(m2,x=y,method='squared')


