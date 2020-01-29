# set.seed(123456) # para tener siempre la misma realización
e <- rnorm(500,0,1)
x <- array(0,500)
kk=0.8
phi1=2*cos(2*pi/3)*kk
phi2=-(kk*kk)

for (i in 3:500){
x[i]=phi1*x[i-1]+phi2*x[i-2]+e[i]        
}
x<-x[3:500]


op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
plot.ts(x,ylab='')
acf(x,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=12)
pacf(x,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=12 )
spectrum(x, method="ar")
spectrum(x, kernel("fejer",200,r=6))
spectrum(x)
par(op)



