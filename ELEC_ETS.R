library(forecast)
el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
ele=el[165:708,1]
elec2 <- ts(ele, start = c(1970, 1), frequency = 12)
plot(elec2)
plot(log(elec2))

m1<-ets(elec2,model="AAA")
m2<-ets(log(elec2),model="AAA")
m3<-ets(elec2,model="MAM")
m4<-ets(elec2,model="ZZZ")
tsdiag(m1)
tsdiag(m2)
tsdiag(m3)
tsdiag(m4)

fit_m4<-fitted(m4)
ts.plot(elec2,fit_m4,col=1:2)