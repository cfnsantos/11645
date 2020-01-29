d=0.45
phi=0.8
t=500;

md_nu=rep(0,t)
md_nu[1]=log(1)
md_nu[2]=log(d)

md_de=rep(0,t)

md_de[1]=log(1)
md_de[2]=log(1)

for (i in 2:t) {
md_nu[i]=md_nu[i-1]+log((d+(i-2)))
md_de[i]=md_de[i-1]+log(i-1)
}

md=matrix(data=exp(md_nu-md_de), nrow=500, ncol=1)

mat_med=diag(nrow(md))

for (i in 1:t) {
mat_med[i,1:i]=t(rev(md[1:i]))
}


e <- matrix(data=rnorm(t,0,1), nrow=500, ncol=1)
y <- mat_med%*%e

x <- array(0,500)
z <- array(0,500)

for (i in 2:500) {
x[i]=x[i-1]+e[i]
z[i]=0.7*z[i-1]+e[i]
}

x=x[2:500]
z=z[2:500]


op <- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE))
acf(y,lag.max = 50)
spectrum(y, method="ar")
acf(x,lag.max = 50)
spectrum(x, method="ar")
acf(z,lag.max = 50)
spectrum(z, method="ar")
par(op)




