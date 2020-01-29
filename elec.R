el <- read.table(file="C:\\pruebas_r\\elec.dat", header=FALSE)
elec <- ts(el, start = c(1948, 1), frequency = 12)
plot(elec)