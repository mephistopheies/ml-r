rm(list = ls())
set.seed(1488)
source("Regression/regressionFunctions.R")
data <- read.csv("Data/Regression/wdbc.data.txt", sep=",", header=F)
data$V2 <- data$V2 == "M"
data$V1 <- NULL
names(data)[1] <- "IsMal"

x <- as.matrix(data[, 2:dim(data)[2]])
y <- matrix(as.numeric(data[, 1]), ncol=1, nrow=dim(data)[1])




if(T)
{
  #crossvalidation
  
  idx.ones.test <- which(y == 1)
  idx.zeros.test <- which(y == 0)  
  
  trainFactor <- 0.6
  ones.trainSize <- round(length(idx.ones.test) * trainFactor)
  zeros.trainSize <- round(length(idx.zeros.test) * trainFactor)
  cvFactor <- 0.2
  ones.cvSize <- round(length(idx.ones.test) * cvFactor)
  zeros.cvSize <- round(length(idx.zeros.test) * cvFactor)
  testFactor <- 0.2
  ones.testSize <- round(length(idx.ones.test) * testFactor)
  zeros.testSize <- round(length(idx.zeros.test) * testFactor)
  
  idx.ones.train <- idx.ones.test[sample(1:length(idx.ones.test), ones.trainSize)]
  idx.zeros.train <- idx.zeros.test[sample(1:length(idx.zeros.test), zeros.trainSize)]
  idx.ones.test <- idx.ones.test[which(!(idx.ones.test %in% idx.ones.train))]
  idx.zeros.test <- idx.zeros.test[which(!(idx.zeros.test %in% idx.zeros.train))]
  idx.ones.cv <- idx.ones.test[sample(1:length(idx.ones.test), ones.cvSize)]
  idx.zeros.cv <- idx.zeros.test[sample(1:length(idx.zeros.test), zeros.cvSize)]
  idx.ones.test <- idx.ones.test[which(!(idx.ones.test %in% idx.ones.cv))]
  idx.zeros.test <- idx.zeros.test[which(!(idx.zeros.test %in% idx.zeros.cv))]
  
  data.train <- x[union(idx.ones.train, idx.zeros.train), ]
  data.cv <- x[union(idx.ones.cv, idx.zeros.cv), ]
  data.test <- x[union(idx.ones.test, idx.zeros.test), ]
  
  y.train <- y[union(idx.ones.train, idx.zeros.train)]
  y.cv <- y[union(idx.ones.cv, idx.zeros.cv)]
  y.test <- y[union(idx.ones.test, idx.zeros.test)]
  
  
  #r.log.fit.all <- reg.logistic(data.train, y.train, lambda = 0.00001, maxIterations = 500000, 
  #                              accuracy=1, minError = -Inf, x.cv = data.cv, y.cv = y.cv, showLog = T,
  #                              iterStopSkip = 10000) 
  #save(r.log.fit.all, file = "Data/Regression/r.log.fit.all_cancer.Rda")
  load("Data/Regression/r.log.fit.all_cancer.Rda")
  
  plot(r.log.fit.all$cost[10000:length(r.log.fit.all$cost)], col="red", type="l",
       xlab="iteration", ylab="cost", main="logistic regression training")
  points(r.log.fit.all$cvCost[10000:length(r.log.fit.all$cvCost)], col="blue", type="l")
  
  o.test <- sigmoid(cbind(data.test, rep(1, dim(data.test)[1])) %*% r.log.fit.all$theta)
  test.error <- reg.logistic.cost(y.test, o.test)
  abline(test.error, 0, col="green")
  
  
  train.error <- r.log.fit.all$cost[length(r.log.fit.all$cost)]
  
  
  
  
  test.prediction <- reg.logistic.predict(r.log.fit.all$theta, data.test)
  
  
}