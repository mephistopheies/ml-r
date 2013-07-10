rm(list=ls())
source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m <- samsungData[, 1:(dim(samsungData)[2] - 2)]
m.cov <- cov(m)
#e <- IterativeQRMethod(m.cov, precision=0.001, 
#                       maxIterations=14, showLog=T)
#save(e, file="./Data/Spam/spamDF.Rda")
load("./Data/MobilePhone/eigen.Rda")
#e$eigenVectors <- svd(m.cov)$u
#e$eigenValues <- svd(m.cov)$d
m.proj <- ProjectData(m, e$eigenVectors[, 1:3])
par(mfrow=c(2,2))
plot(e$eigenValues[1:3]^2/sum(e$eigenValues^2),
     xlab="Column",ylab="Percent of variance explained",
     main=paste("Variance explained with", 
                3, "components", sep=" "),
     pch=19, col="red")
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(6)[unclass(as.factor(samsungData$activity))], 
     xlab="first", ylab="second", main="Two components")
plot(m.proj[, 1], m.proj[, 3], pch=19, 
     col=rainbow(6)[unclass(as.factor(samsungData$activity))], 
     xlab="first", ylab="third", main="Two components")
plot(m.proj[, 2], m.proj[, 3], pch=19, 
     col=rainbow(6)[unclass(as.factor(samsungData$activity))], 
     xlab="secons", ylab="third", main="Two components")
