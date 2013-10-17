rm(list=ls())

source("KMeans/kmeansFunctions.R")
source("KMeans/kmeansDaviesBouldinIndexFunctions.R")
set.seed(148800)
k <- 3
#par(mfrow=c(3,2))
source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m.raw <- samsungData[, 1:(dim(samsungData)[2] - 2)]
load("./Data/MobilePhone/eigen.Rda")
m.proj <- ProjectData(m.raw, e$eigenVectors[, c(1, 3)])


KMeans.DB.result <- KMeans.DB(k, m.proj, HalfSqEuclidian.distance, 
                                                    HalfSqEuclidian.derivative, showLog = T)
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(k)[unclass(as.factor(KMeans.DB.result$labels))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; KMeans.gd \n cost = ", 
                KMeans.DB.result$cost[length(KMeans.DB.result$cost)], sep=""))