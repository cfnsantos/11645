library(KFAS)
e1 <- rnorm(506,0,.25)
x <- array(0,506)
for (i in 2:506){
        x[i]=x[i-1]+e1[i]        
}
x<-x[2:506]

e2 <- rnorm(505,0,.5)
z <- array(0,505)
for (i in 2:505){
  z[i]=z[i-1]+x[i-1]+e2[i]        
}
z<-z[2:505]

e3 <- rnorm(504,0,4)
s <- array(0,504)
for (i in 4:504){
  s[i]=-s[i-1]-s[i-2]-s[i-3]+e3[i]        
}
s<-s[5:504]
z<-z[5:504]

u <- rnorm(500,0,1)
y<-z+s+u
y<-matrix(y,nrow=500,ncol=1)
plot.ts(y,ylab='')

Zt<-matrix(c(1,0,1,0,0),1,5)
Ht=matrix(NA)
Tt=matrix(c(1,0,0,0,0,1,1,0,0,0,0,0,-1,1,0,0,0,-1,0,1,0,0,-1,0,0),5,5)
Rt=rbind(diag(3),rep(0,1,3),rep(0,1,3))
Qt=matrix(c(NA,0,0,0,NA,0,0,0,NA),3,3)
a1=matrix(c(0,0,0,0,0),5,1)
P1=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),5,5)
P1inf=diag(5)
mod1<-SSModel(y~-1+SSMcustom(Z=Zt,T=Tt,R=Rt,Q=Qt,a1=a1,P1=P1,P1inf=P1inf),H=Ht)
#mod1<-SSModel(y~-1+SSMcustom(Z=Zt,T=Tt,R=Rt,Q=Qt),H=Ht)
fit_mod1 <-fitSSM(mod1,inits=c(0,0,0,0,0),method="BFGS")
out_mod1 <-KFS(fit_mod1$model)
print(fit_mod1$model$H)
print(fit_mod1$model$Q)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(y,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
ts.plot(out_mod1$a[,1:3],col=1:3)
par(op)

#SSMseasonal(period = 12, sea.type = "trigonometric", Q = NA)

mod2<-SSModel(y~SSMtrend(degree=2,Q=list(matrix(NA),matrix(NA)))+SSMseasonal(period=4,sea.type = "trigonometric",Q=NA),H=1)
fit_mod2 <-fitSSM(mod2,inits=c(0,0,0,0,0),method="BFGS")
out_mod2 <-KFS(fit_mod2$model)
print(fit_mod2$model$H)
print(fit_mod2$model$Q)

op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(y,ylab='')
acf(y,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(y,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
ts.plot(out_mod2$a[,1:3],col=1:3)
par(op)

ts.plot(out_mod2$a[,1],out_mod1$a[,1],col=1:2)
ts.plot(out_mod2$a[,2],out_mod1$a[,2],col=1:2)
ts.plot(out_mod2$a[,3],out_mod1$a[,3],col=1:2)

