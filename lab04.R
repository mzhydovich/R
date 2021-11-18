# read data
dat <- read.table("input_lab04.txt")

# parse date
date <- strptime(dat[,1], format="%d.%m.%Y")

# create table
table <- data.frame(date, as.numeric(dat[,2]))
colnames(table) <- c("Date", "Exchange rate")
print(table)

# find median
mn <- mean(dat[,2])

# compare each value with mean
result <- numeric()
for(i in 1:length(dat[,2])) {
	if(dat[i,2] > mn) result <- c(result, 1)
    if(dat[i,2] < mn) result <- c(result, 0)
}

num_of_series <- 1
var <- result[1]

# find number of series
n <- length(result)
for(i in 2:n) {
  if(result[i] != var) {
    num_of_series <- num_of_series+1
    var <- result[i]
  }
}

# find trend
lft <- as.integer((n + 1) / 2 - 1.96 * sqrt(n - 1) / 2)
rht <- as.integer((n + 1) / 2 + 1.96 * sqrt(n - 1) / 2)

if(lft <= num_of_series & num_of_series <= rht) {
  trend <- "No"
} else {
  trend <- "Yes"
}

cat("\nMean: ", mn, '\n')
print(result)
cat("Number of series: ", num_of_series, '\n')
cat(lft, "<=", num_of_series, "<=", rht, '\n')
cat("Trend: ", trend, '\n')

# smoothing method
y <- dat[,2]
smooth <- numeric()
n <- length(y)
smooth <- c(smooth, (5 * y[1] + 2 * y[2] - y[3]) / 6)
for(i in 2:(n - 1)) {
  smooth <- c(smooth, (y[i - 1] + y[i] + y[i + 1]) / 3)
}
smooth<-c(smooth, (5 * y[n - 1] + 2 * y[n - 2] - y[n - 3]) / 6)
cat("\nSmoothing method:\n")
smooth 

# analytic method
t <- numeric()
counter <- as.integer((-1) * n / 2)

for(i in 1:n) {
  t <- c(t, counter)
  counter <- counter + 1
}

analytic_table <- data.frame(table[,1], table[,2])
analytic_table[,3] <- t
analytic_table[,4] <- t * t
analytic_table[,5] <- y * t
colnames(analytic_table) <- c("Date", "Exchange rate", "t", "t^2", "y*t")
cat("\nAnalytic table:\n")
analytic_table

a0 <- sum(analytic_table[,2]) / n
a1 <- sum(analytic_table[,5]) / sum(analytic_table[,4])

analytic_smooth <- numeric()
for(i in 1:n) {
  analytic_smooth <- c(analytic_smooth, a0 + a1 * (i - as.integer(n / 2) - 1))
}
cat("\nAnalytic method:\n")
analytic_smooth

# predict
y_next <- a0 + a1 * (max(t) + 1)
cat("\nNext value: ", y_next, '\n')

# draw plots
fun1 <- function(x) y
fun2 <- function(x) smooth
fun3 <- function(x) analytic_smooth

matplot(table[,1], cbind(fun1(x), fun2(x), fun3(x)), type="l", col=c("blue","red", "green"), xlab="date", ylab="medium progress", main="plot")
legend(x="bottomleft", y=0.92, legend=c("first", "smooth", "analytic_smooth"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("blue", "red", "green"))
