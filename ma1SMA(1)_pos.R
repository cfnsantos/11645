ee <- rnorm(512,0,1)
e <- matrix(data=ee,nrow=512,ncol=1)
uu<-e[13:nrow(e)]-0.7*e[1:(nrow(e)-12)]
u <- matrix(data=uu,nrow=500,ncol=1)
x<-u[2:nrow(u)]-0.8*u[1:(nrow(u)-1)]
op <- par(no.readonly = TRUE)
layout(matrix(c(1,2),2,1,byrow=TRUE))
acf(x,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36)
pacf(x,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
par(op)
