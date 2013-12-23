rm(list=ls())
source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
source("Regression/regressionFunctions.R")
m <- samsungData[, 1:(dim(samsungData)[2] - 2)]
m.cov <- cov(m)
#e <- IterativeQRMethod(m.cov, precision=0.001, 
#                       maxIterations=14, showLog=T)
#save(e, file="./Data/Spam/spamDF.Rda")
load("./Data/MobilePhone/eigen.Rda")
#e$eigenVectors <- svd(m.cov)$u
#e$eigenValues <- svd(m.cov)$d
m.proj <- ProjectData(m, e$eigenVectors[, 1:10])
y <- as.numeric(unclass(as.factor(samsungData$activity)) == 1)

if(F)
{
  evSorted <- sort(e$eigenValues, decreasing=T)
  evNorm <- sum(evSorted^2)
  ve <- (evSorted^2)/evNorm
  comVe <- rep(0, length(ve))
  comVe[1] = ve[1]
  for(i in 2:length(ve))
  {
    comVe[i] = ve[i] + comVe[i - 1]
  }
  
  plot(ve[2:15], type="l", col="red", xlab="Number of components", ylab="Variance",
       main="Variance explained")
  
  plot(comVe[1:50], type="l", col="red", xlab="Number of components", ylab="Variance",
       main="Commulative variance explained")
  abline(1, 0, col="blue")
}

if(F)
{
  plot(m.proj[, 1], m.proj[, 3], pch=19, 
       col=rainbow(6, alpha=0.2)[unclass(as.factor(samsungData$activity))], 
       xlab="first", ylab="third", main="Two components")
}

if(F)
{
  plot(m.proj[, 1], m.proj[, 3], pch=19, 
       col=c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.2))[(y + 1)], 
       xlab="first", ylab="third", main="Two components")
}

#r.log.fit.all <- reg.logistic(m.proj, y, lambda = 0.001, maxIterations = 100000, 
#                              accuracy=0, minError = -Inf, showLog = T)
#save(r.log.fit.all, file="Data/Regression/r.log.fit.all_smartphones.Rda")
load("Data/Regression/r.log.fit.all_smartphones.Rda")

if(F)
{
  plot(r.log.fit.all$cost[1:length(r.log.fit.all$cost)], col="red", type="l",
       xlab="iteration", ylab="cost", main="logistic regression training")
  
  p <- sigmoid(cbind(m.proj, rep(1, dim(m.proj)[1])) %*% r.log.fit.all$theta)
  o <- as.numeric(p > 0.5)
  q <- binaryClassifierEvaluation(y, o)
}

