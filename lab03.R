# read data
dat <- read.table("input_lab03.txt", dec = ",")

# scatter diagram
plot(dat, type="p",main="Correlation field",xlab="X", ylab="Y")

# create intervals
x <- dat[,1]
sr <- mean(x)
sigma <- sd(x)
rows <- paste0("(", round(sr - (1:3) * sigma, 2), ", ", round(sr + (1:3) * sigma, 2), ")")

gr1 <- subset(x, ((sr - sigma) < x) & (x < (sr + sigma)))
gr2 <- subset(x, ((sr - 2 * sigma) < x) & (x < (sr + 2 * sigma)))
gr3 <- subset(x, ((sr - 3 * sigma) < x) & (x < (sr + 3 * sigma)))

# get len
n <- length(dat[,1])

# create table
tab <- rep(0, 3*4)
dim(tab) <- c(3,4)
tab <- as.data.frame(tab)

# fill table
tab[,1] <- rows[1:3]
tab[1:3,2] <- c (length (gr1), length (gr2), length(gr3))
tab[1:3,3] <- tab[1:3,2] / n * 100
tab[1:3,4] <- c (68.3, 95.4, 99.7)
print(tab)

range <- max(x) - min(x)

k <- 1 + floor(log(length(x), 2))
w <- range / k

x <- sort(x)

# create new table
table <- rep(0, k*4)
dim(table) <- c(k,4)
table <- as.data.frame(table)

# find new intervals and fill new table
for (i in 0:(k-1)) {
  l <- x[1] + i * w
  r <- x[1] + (i + 1) * w
  gr <- subset(x, l <= x & (x < r | i == k - 1 & x <= r))
  table[i + 1, 1:4] <- c(paste0("[", l, "; ", r, ")"), length(gr),sum(gr),mean (gr))
}

print(table)

coeffcor <- cor(dat[,1], dat[,2])
cat("\nCoefficient of correlation: ", coeffcor)

t <- coeffcor * sqrt((n - 2) / (1 - coeffcor ^ 2))
cat("\nT: ", t)

cor.test(dat[,1], dat[,2])

lm(dat[,2]~dat[,1])
abline(lm(dat[,2]~dat[,1]))
