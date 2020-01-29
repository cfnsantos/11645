library(KFAS)
e <- rnorm(501,0,1)
x <- array(1,501)
for (i in 2:501){
        x[i]=x[i-1]+e[i]        
}
x<-x[2:501]

u <- rnorm(500,0,1)
y<-x+u
y<-matrix(y,nrow=500,ncol=1)


Zt<-matrix(1,1,1)
Ht=matrix(NA)
Tt=matrix(1,1,1)
Rt=matrix(1,1,1)
Qt=matrix(NA)
a1=matrix(1,1,1)
P1=matrix(0,1,1)
P1inf=diag(1)
y<-ts(data=y,start=1960,frequency = 1)
mod1<-SSModel(y~-1+SSMcustom(Z=Zt,T=Tt,R=Rt,Q=Qt,a1=a1,P1=P1,P1inf=P1inf),H=Ht)
fit_mod1 <-fitSSM(mod1,inits=c(0,0),method="BFGS")
out_mod1 <-KFS(fit_mod1$model)
print(fit_mod1$model$H)
print(fit_mod1$model$Q)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(y,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
ts.plot(out_mod1$a,y,col=1:2)
par(op)

mod2<-SSModel(y~SSMtrend(degree=1,Q=Qt),H=Ht)
fit_mod2 <-fitSSM(mod2,inits=c(0,0),method="BFGS")
out_mod2 <-KFS(fit_mod2$model)
print(fit_mod2$model$H)
print(fit_mod2$model$Q)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(y,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
ts.plot(out_mod2$a,y,col=1:2)
par(op)

