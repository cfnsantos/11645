library(KFAS)
e1 <- rnorm(502,0,1)
x <- array(0,502)
for (i in 2:502){
        x[i]=x[i-1]+e1[i]        
}
x<-x[2:502]

e2 <- rnorm(501,0,1)
z <- array(0,501)
for (i in 2:501){
  z[i]=z[i-1]+x[i-1]+e2[i]        
}
z<-z[2:501]


u <- rnorm(500,0,2)
y<-z+u
y<-matrix(y,nrow=500,ncol=1)


Zt<-matrix(c(1,0),1,2)
Ht=matrix(NA)
Tt=matrix(c(1,0,1,1),2,2)
Rt=diag(2)
Qt=matrix(c(NA,0,0,NA),2,2)
a1=matrix(c(0,0),2,1)
P1=matrix(c(0,0,0,0),2,2)
P1inf=diag(2)
y<-ts(data=y,start=1960,frequency = 1)
mod1<-SSModel(y~-1+SSMcustom(Z=Zt,T=Tt,R=Rt,Q=Qt,a1=a1,P1=P1,P1inf=P1inf),H=Ht)
#mod1<-SSModel(y~-1+SSMcustom(Z=Zt,T=Tt,R=Rt,Q=Qt),H=Ht)
fit_mod1 <-fitSSM(mod1,inits=c(0,0,0),method="BFGS")
out_mod1 <-KFS(fit_mod1$model)
print(fit_mod1$model$H)
print(fit_mod1$model$Q)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(y,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
ts.plot(out_mod1$a,y,col=1:3)
par(op)

mod2<-SSModel(y~SSMtrend(degree=2,Q=list(matrix(NA),matrix(NA))),H=1)
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

ts.plot(out_mod2$a[,1],out_mod1$a[,1],col=1:2)
ts.plot(out_mod2$a[,2],out_mod1$a[,2],col=1:2)
ts.plot(out_mod2$a[,1],out_mod1$a[,1],y,col=1:3)