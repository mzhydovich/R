# read data
dat <- read.table(file = "input_lab01.txt")
y <- t(dat)

# frequency
a <- table(y)

# create table
f <- as.data.frame(a)
f[,3] <- 100 * (a / sum(a))
f[,4] <- cumsum(a)
f[,5] <- cumsum(f[,3])

# change column names
colnames(f) <- c("Value", "Frequency", "Frequence", "Accumulated Frequency", "Accumulated Frequence")
print(f)

# poligon plot
plot(a, type="l", main="Poligon", xlab="Value", ylab="Frequency")

# get unique sorted elements
x<-unique(sort(as.numeric(y)))

# cumulat plots
plot(x, as.numeric(f[,4]), type="l", main="Accumulated Frequency", xlab="Value", ylab="Accumulated Frequency")
plot(x, as.numeric(f[,5]), type="l", main="Accumulated Frequence", xlab="Value", ylab="Accumulated Frequence")
