# read data
dat <- read.table("input_lab02.txt", dec = ",")
print(t(dat))
d <- dat[,1]
n <- length(d)

# find mean
mn <- mean(d)
cat("Mean: ", mn)

# find disperse
disp <- var(d)
cat("\nDisperse: ", disp)

# find standard deviation
s <- sd(d)
cat("\nStandard deviation: ", s)

# find mode
mode <- which.max(table(d))
cat("\nMode: ", sort(d)[mode], " on ", mode, " position")

# find median
med <- median(d)
cat("\nMedian: ", med)

# find coefficients of asymmetry and kurtosis
zn <- sort(unique(d))
koeff_assim <- sum((zn - mn)^3 * table(d))/(n * s^3)
cat("\nCoefficient of asymmetry: ", koeff_assim)
kurtosis <- sum((zn - mn)^4 * table(d))/(n * s^4) - 3
cat("\nKurtosis: ", kurtosis)

# set truuncate %
tr <- 0.2
# find truncated mean
tr_mn <- sort(d)[-(1:(tr * n))]
tr_mn <- tr_mn[-((length(tr_mn) - tr * n):length(tr_mn))]
tr_mn <- mean(tr_mn)
cat("\nAverage truncated by 20%: ", tr_mn)

# find coefficient of variation
coeff_of_var <- (s / mn)
cat("\nCoefficient of variation: ", coeff_of_var)

# find linear deviation
lin_dev <- 0
for(i in 1:n)
	lin_dev <- lin_dev + abs(d[i] - mn)
lin_dev <- lin_dev / n

# find relative linear deviation
rel_lin_dev <- lin_dev / mn
cat("\nRelative linear deviation: ", rel_lin_dev, "\n")
