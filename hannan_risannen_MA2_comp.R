e <- rnorm(502,0,1)
x <- array(0,502)
for (i in 3:nrow(x)){
x[i]=e[i]-e[i-1]+0.8*e[i-2]
}

x<-x[3:502]

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

(fit1 <- arima(x, c(0, 0, 2)))
