rm(list=ls())
source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m <- samsungData[, 1:(dim(samsungData)[2] - 2)]

if(F)
{
  #plot features vs class
  dim1 <- 2
  dim2 <- 3
  plot(m[, dim1], m[, dim2], pch=19, 
       col=rainbow(6, alpha=0.5)[unclass(as.factor(samsungData$activity))], 
       xlab=names(m)[dim1], ylab=names(m)[dim2], main="Two features")
}

if(F)
{
  #plot distribution of features
  hist(m[, 1], col="red", breaks=100, freq=F, main=names(m)[1], xlab="feature value")
}

m.cov <- cov(m)
#e <- IterativeQRMethod(m.cov, precision=0.001, 
#                       maxIterations=14, showLog=T)
#save(e, file="./Data/Spam/spamDF.Rda")
load("./Data/MobilePhone/eigen.Rda")
#e$eigenVectors <- svd(m.cov)$u
#e$eigenValues <- svd(m.cov)$d
m.proj <- ProjectData(m, e$eigenVectors[, 1:3])


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
  par(mfrow=c(2,2))
  plot(e$eigenValues[1:3]^2/sum(e$eigenValues^2),
       xlab="Column",ylab="Percent of variance explained",
       main=paste("Variance explained with", 
                  3, "components", sep=" "),
       pch=19, col="red")
  plot(m.proj[, 1], m.proj[, 2], pch=19, 
       col=rainbow(6, alpha=0.2)[unclass(as.factor(samsungData$activity))], 
       xlab="first", ylab="second", main="Two components")
  plot(m.proj[, 1], m.proj[, 3], pch=19, 
       col=rainbow(6, alpha=0.2)[unclass(as.factor(samsungData$activity))], 
       xlab="first", ylab="third", main="Two components")
  plot(m.proj[, 2], m.proj[, 3], pch=19, 
       col=rainbow(6, alpha=0.2)[unclass(as.factor(samsungData$activity))], 
       xlab="secons", ylab="third", main="Two components")
}