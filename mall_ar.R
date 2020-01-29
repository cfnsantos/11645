ma <- read.table(file="C:\\pruebas_r\\mall_ar_diario.dat", header=FALSE)
mall <- ts(ma, start = c(1995, 1), frequency = 365)
plot(mall)
ma_1 <-mall[1:365]
mall_1 <- ts(ma_1, start = c(1995, 1), frequency = 365)
# plot(mall_1)