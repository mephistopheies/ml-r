rm(list = ls())
set.seed(1488)
source("Regression/regressionFunctions.R")
data <- read.csv("Data/Regression/wdbc.data.txt", sep=",", header=F)
data$V2 <- data$V2 == "M"
data$V1 <- NULL
names(data)[1] <- "IsMal"

x <- as.matrix(data[, 2:dim(data)[2]])
y <- matrix(as.numeric(data[, 1]), ncol=1, nrow=dim(data)[1])

if(F)
{
  par(mfrow=c(2, 2))
  
  plot(data$V3, data$V5,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V7, data$V6,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V19, data$V32,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V15, data$V20,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  
  par(mfrow=c(1, 1))
}

if(F)
{
  
  r.log.fit.all <- reg.logistic(x, y, lambda = 0.0001, maxIterations = 100000, 
                                accuracy=0, minError = -Inf, showLog = T)
  #load(file="Data/Regression/r.log.fit.all.Rda")
  
  plot(r.log.fit.all$cost[100:length(r.log.fit.all$cost)], col="red", type="l",
       xlab="iteration", ylab="cost", main="logistic regression training")
  
  p <- sigmoid(cbind(x, rep(1, dim(x)[1])) %*% r.log.fit.all$theta)
  o <- as.numeric(p > 0.5)
  d <- as.numeric(y) == o
  q <- sum(d)/length(o)
  
}