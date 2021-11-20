library(MASS)

# generate data
x1 <- runif(10, -5, 5)
y1 <- runif(10, -5, 5)
x2 <- rnorm(20, 7, 7/3)
y2 <- rnorm(20, 7, 7/3)

# create general table
xy<-cbind(c(x1, x2),c(y1, y2))
xy

# divide into 2 clusters
cl1 <- kmeans(xy, 2)
cat("\nDivide into 2 clusters:")
table(cl1$cluster)
cat("\nCenters:\n")
cl1$centers

# create plots
plot(xy, col=ifelse(cl1$cluster==2,"red","green"))
legend("topright",legend=c("1","2"),fill=c("red","green"))

n <- length(xy[,1])

n.train <- floor(n*0.7)            
n.test <- n - n.train           
idx.train <- sample(1:n,n.train)
idx.test <- (1:n)[!(1:n %in% idx.train)]

data.train <- xy[idx.train,]       
data.test <- xy[idx.test,] 

cl <- kmeans(xy,2)
cl.cluster <- cl$cluster
cl.train <- cl.cluster[idx.train]
cl.test <- cl.cluster[idx.test]

mod <- qda(data.train, cl.train)               
cl.test_est <- predict(mod, data.test)$class 
error1 <- sum(cl.test_est!=cl.test) / n.test 
cat("\nError value: ", error1, '\n')         
idx <- idx.test[cl.test_est!=cl.test]  

plot(xy, type="n")
points(data.train,pch=24, col=ifelse(cl.train==1,"blue","green"))
legend("topleft",legend=c("train - 1","train - 2"),pch=24,col=c("blue","green"))
points(data.test,pch=21, col=ifelse(cl.test==1,"blue","green"))
legend("bottomright",legend=c("test - 1","test - 2"),pch=21,col=c("blue","green"))
if (length(idx)==1){			           
  points(xy[idx,1],xy[idx,2],col="red", pch=4) 
}else
  points(xy[idx,],col="red", pch=4)
legend("bottom",legend=c("wrong"),pch=4,col="red")


idd<-sample(1:n.train,n.train * 0.2)         
for(i in idd) 
  cl.train[i]=ifelse(cl.train[i]==1,2,1)     

mod2<-qda(data.train, cl.train)     
cl.test_est<-predict(mod2, data.test)$class  
error2 = sum(cl.test_est!=cl.test)/n.test     
cat("\nError value: ", error2, '\n') 
idx2<-idx.test[cl.test_est!=cl.test]         

plot(xy, type="n")
points(data.train,pch=24, col=ifelse(cl.train==1,"blue","green"))
legend("topleft",legend=c("Train - 1","Train - 2"),pch=24,col=c("blue","green"))
points(data.test,pch=21, col=ifelse(cl.test==1,"blue","green"))
legend("bottomright",legend=c("Test - 1","Test - 2"),pch=21,col=c("blue","green"))
if (length(idx2)==1){
  points(xy[idx2,1],xy[idx2,2],col="red", pch=4) 
}else
  points(xy[idx2,],col="red", pch=4)
legend("bottom",legend=c("wrong"),pch=4,col="red")
