el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
elec <- ts(el, start = c(1948, 1), frequency = 12)
plot(log(elec))
t=seq(1,nrow(elec))
det_seas=cbind(cos(0*t),cos(pi*t))
for (j in 1:5) {det_seas=cbind(det_seas,cos(2*pi*t*j/52),sin(2*pi*t*j/52))}
end1 <-log(elec)
exo1 <-cbind(t,det_seas)
rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
elec_e=log(elec)-exo1%*%rho
elec_f=exo1%*%rho
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_e <- ts(elec_e, start = c(1948, 1), frequency = 12)
elec_f <- ts(elec_f, start = c(1948, 1), frequency = 12)
ser=cbind(log(elec),elec_e,elec_f)
ts.plot(log(elec),elec_f,col=1:2)

