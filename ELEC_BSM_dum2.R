library(KFAS)
el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
ele=el[165:708,1]
elec2 <- ts(ele, start = c(1970, 1), frequency = 12)
plot(elec2)
plot(log(elec2))

mod2<-SSModel(log(elec2)~SSMtrend(degree=2,Q=list(matrix(NA),matrix(NA)))+SSMseasonal(period=12,sea.type = "dummy",Q=NA),H=NA)
fit_mod2 <-fitSSM(mod2,inits=c(8.5,0,0,0,0,0,0,0,0,0,0,0,0,0),method="BFGS")
out_mod2 <-KFS(fit_mod2$model)
print(fit_mod2$model$H)
print(fit_mod2$model$Q)


pred<-out_mod2$a[25:544,1]+out_mod2$a[25:544,3]
ts.plot(cbind(pred,log(elec2[25:544])),col=1:2)
res_mod2<-residuals(out_mod2)
res_mod22 <-ts(res_mod2[25:544], start = c(1972, 1), frequency = 12)
ts.plot(res_mod22)
acf(res_mod22)
pacf(res_mod22)

h11=fit_mod2$model$H
QQ=matrix(data=fit_mod2$model$Q,3,3)

mod22<-SSModel(log(elec2)~SSMtrend(degree=2,Q=list(matrix(QQ[1,1]),matrix(QQ[2,2])))+SSMseasonal(period=12,sea.type = "dummy",Q=QQ[3,3]),H=h11) #
for_mod2<-predict(mod22,n.ahead=24)