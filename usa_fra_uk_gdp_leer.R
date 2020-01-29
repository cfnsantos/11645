dat <- read.table(file="d:\\pruebas_r\\fra_uk_usa.dat", header=FALSE)
fra <- ts(dat[1:248,1], start = c(1955, 1), frequency = 4)
uk <- ts(dat[1:248,2], start = c(1955, 1), frequency = 4)
usa <- ts(dat[1:248,3], start = c(1955, 1), frequency = 4)

