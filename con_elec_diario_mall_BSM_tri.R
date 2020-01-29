library(KFAS)
ell <- read.table(file="C:\\pruebas_r\\elec_diario.dat", header=FALSE)
elma<-ell[1:5020,1]
elme<-ell[1:5020,2]
elmaa <-matrix(data=elma,ncol=1)
elmee <-matrix(data=elme,ncol=2)
elma <- ts(elmaa, start = c(1994, 1), frequency = 365)
elme <- ts(elmee, start = c(1994, 1), frequency = 365)
plot(log(elma))
t=seq(1,nrow(elma))
det_seas=cos(0*t)
for (j in 1:182) {det_seas=cbind(det_seas,cos(2*pi*t*j/365),sin(2*pi*t*j/365))}
end1 <-log(elma)
exo1 <-cbind(det_seas,t,t^2)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elma_e=log(elma)-exo1%*%rho
elma_f=exo1%*%rho
elma_e <- ts(elma_e, start = c(1994, 1), frequency = 365)
elma_f <- ts(elma_f, start = c(1994, 1), frequency = 365)
ts.plot(log(elma),elma_f,col=1:2)
ts.plot(elma_e)

mod2<-SSModel(elma_e~SSMtrend(degree=2,Q=list(matrix(NA),matrix(NA)))+SSMseasonal(period=7,sea.type = "trigonometric",Q=NA),H=NA)
fit_mod2 <-fitSSM(mod2,inits=c(0,0,0,0,0,0,0,0,0),method="BFGS")
out_mod2 <-KFS(fit_mod2$model)
print(fit_mod2$model$H)
print(fit_mod2$model$Q)

h11=fit_mod2$model$H
QQ=matrix(fit_mod2$model$Q,8,8)


ts.plot(elma_e,out_mod2$a[,1]+out_mod2$a[,3]+out_mod2$a[,5]+out_mod2$a[,7],col=1:2) # prediccion con tri
res_mod2<-residuals(out_mod2)
res_mod22 <-ts(res_mod2, start = c(1994, 1), frequency = 365)
ts.plot(res_mod22)
acf(res_mod22)
pacf(res_mod22)


mod_pred<-SSModel(elma_e~SSMtrend(degree=2,Q=list(matrix(QQ[1,1]),matrix(QQ[2,2])))+SSMseasonal(period=7,sea.type = "trigonometric",Q=QQ[3,3]),H=h11)
pred_mod2<-predict(mod_pred,n.ahead=365)
