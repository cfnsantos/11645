# set.seed(123456) # para q salga siempre igual
e <- rnorm(500,0,1)
x<-e[2:150]+0.8*e[1:149]        
op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot.ts(x,ylab='')
acf(x,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(x,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
par(op)
