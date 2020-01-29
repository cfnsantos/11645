library(forecast)
ell <- read.table(file="C:\\pruebas_r\\elec_diario.dat", header=FALSE)
elma<-ell[1:5020,1]
elme<-ell[1:5020,2]
elmaa <-matrix(data=elma,ncol=1)
elmee <-matrix(data=elme,ncol=2)
elma <- msts(elmaa, seasonal.periods=c(7,365.25))
m1<-bats(elma)
res_m1<-residuals(m1)
tsdisplay(res_m1)
acf(res_m1)
pacf(res_m1)
elma_f<-fitted(m1)
ts.plot(elma,elma_f,col=1:2)
