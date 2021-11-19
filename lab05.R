# read data
dat <- read.table("input_lab05.txt", dec=',')

# scale dat
scale_dat <- scale(dat)

# draw scattering diagram
plot(dat, type="p", main="Scattering diagram", xlab="X", ylab="Y")

cat("------------------------------")

# divide into 2 clusters
cl1 <- kmeans(scale_dat, 2)
cat("\nDivide into 2 clusters:")
table(cl1$cluster)
cat("\nCenters:\n")
cl1$centers

# create plots
plot(dat, col=ifelse(cl1$cluster==2,"red","green"))
legend("topright",legend=c("1","2"),fill=c("red","green"))

plot(dat, pch=ifelse(cl1$cluster==1,1,2))
legend("topright",legend=c("1","2"),pch=c(1,2))

cat("------------------------------")

# divide into 3 clusters
cl2<-kmeans(scale_dat, 3)
cat("\nDivide into 3 clusters:")
table(cl2$cluster)
cat("\nCenters:\n")
cl2$centers

# create plots
plot(dat, col=ifelse(cl2$cluster==1,"red", ifelse(cl2$cluster==2, "green", "blue")))
legend("topright",legend=c("1","2", "3"),fill=c("red","green", "blue"))

plot(dat, pch=ifelse(cl2$cluster==1,1, ifelse(cl2$cluster==2, 2, 3)))
legend("topright",legend=c("1","2","3"),pch=c(1,2,3))

cat("------------------------------\n")
