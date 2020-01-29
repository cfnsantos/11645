ma <- read.table(file="C:\\pruebas_r\\mall_ar_diario.dat", header=FALSE)
mall <- ts(ma, start = c(1995, 1), frequency = 365)
#plot(log(mall))
t=seq(1,nrow(mall))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
end1 <-log(mall)
exo1 <-det_seas
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
mal_e=log(mall)-exo1%*%rho
mal_f=exo1%*%rho
mall_e <- ts(mal_e, start = c(1995, 1), frequency = 365)
mall_f <- ts(mal_f, start = c(1995, 1), frequency = 365)
ser=cbind(log(mall),mall_e,mall_f)


#op <- par(no.readonly = TRUE)
#layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
#plot.ts(mall_e,ylab='')
#acf(mall_e,main="Autocorrelation",ylab='',ylim=c(-1,1), ci.col="black",lag=36,)
#pacf(mall_e,main="Partial Autocorrelations",ylab='',ylim=c(-1,1), ci.col="black",lag=36 )
#spectrum(mall_e, method="ar")
#spectrum(mall_e, kernel("fejer",100,r=6))
#spectrum(mall_e)
#par(op)

ts.plot(log(mall),mall_f,col=1:2)



