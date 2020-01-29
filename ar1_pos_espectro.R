# set.seed(123456) # para tener siempre la misma realización
e <- rnorm(500,0,1)
x <- array(0,500)
for (i in 2:500){
x[i]=0.8*x[i-1]+e[i]        
}
x<-x[2:500]

end1 <-matrix(x[2:nrow(x)],nrow = nrow(x[2:nrow(x)]), ncol = 1)
exo1 <-matrix(x[1:nrow(x)-1],nrow = nrow(x[1:nrow(x)-1]), ncol = 1)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)


op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(x,ylab='')
acf(x,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(x,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
spectrum(x, method="ar")
spectrum(x, kernel("fejer",200,r=6))
spectrum(x)
par(op)



