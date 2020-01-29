resid_ma2 <- function(dat,theta1,theta2) {
e<- array(0,nrow(dat))
pis<- array(0,nrow(dat))
pis[1]<-theta1
pis[2]<-pis[1]*theta1+theta2
for (i in 3:nrow(pis)){
pis[i]<-pis[i-1]*theta1+pis[i-2]*theta2
}  

e[1]<-dat[1]
kk<-dat[1]
  
for (i in 2:nrow(e)){
e[i]<-dat[i]+t(pis[1:i-1])%*%kk
kk<-rbind(dat[i],kk)}

return(e)}
  

  


t=1000
theta1=.7
theta2=-.5
e <- rnorm(t+2,0,1)
x <- array(0,t+2)
for (i in 3:nrow(x)){
  x[i]=e[i]-theta1*e[i-1]-theta2*e[i-2]
}

x<-x[3:nrow(x)]

max_k <- round(max(log(2*nrow(x)),2*max(0,2)))

end1 <-matrix(x[(max_k+1):nrow(x)],nrow = nrow(x[(max_k+1):nrow(x)]), ncol = 1)
exo1 <-matrix(x[max_k:(nrow(x)-1)],nrow = nrow(x[max_k:(nrow(x)-1)]), ncol = 1)

for (i in 2:max_k){
  exo1 <-cbind(exo1,x[(max_k+1-i):(nrow(x)-i)])        
}

rho <-solve(t(exo1)%*%exo1)%*%(t(exo1)%*%end1)
res<-end1-exo1%*%rho

end2 <-matrix(x[(max_k+3):nrow(x)],nrow = nrow(x[(max_k+3):nrow(x)]), ncol = 1)
exo2 <-cbind(res[2:(nrow(res)-1)],res[1:(nrow(res)-2)])
rho2 <-solve(t(exo2)%*%exo2)%*%(t(exo2)%*%end2)


thetas_es <- array(0, c(2,100))

thetas_es[1,1]<-(-rho2[1,1])
thetas_es[2,1]<-(-rho2[2,1])
#thetas_es[1,1]<-(0)
#thetas_es[2,1]<-(0)


for (i in 2:ncol(thetas_es)){
resi <-resid_ma2(x,thetas_es[1,(i-1)],thetas_es[2,(i-1)])
ini <-x[3:nrow(x)]+thetas_es[1,(i-1)]*resi[2:(nrow(resi)-1)]+thetas_es[2,(i-1)]*resi[1:(nrow(resi)-2)]
grad <-rbind(t(ini)%*%resi[2:(nrow(resi)-1)],t(ini)%*%resi[1:(nrow(resi)-2)])
L1hess <-cbind(t(resi[2:(nrow(resi)-1)])%*%resi[2:(nrow(resi)-1)],t(resi[2:(nrow(resi)-1)])%*%resi[1:(nrow(resi)-2)])
L2hess<-cbind(t(resi[2:(nrow(resi)-1)])%*%resi[1:(nrow(resi)-2)],t(resi[1:(nrow(resi)-2)])%*%resi[1:(nrow(resi)-2)])
KKhess<- rbind(L1hess,L2hess)
thetas_es[,i]<- thetas_es[,(i-1)]-solve(KKhess)%*%grad
}

(fit1 <- arima(x, c(0, 0, 2)))


list(thetas_es = thetas_es)
                     
                                                                                                                                              