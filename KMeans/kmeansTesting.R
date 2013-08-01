rm(list=ls())
source("KMeans/kmeansFunctions.R")
set.seed(148800)
k <- 3
par(mfrow=c(3,2))
source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m.raw <- samsungData[, 1:(dim(samsungData)[2] - 2)]
load("./Data/MobilePhone/eigen.Rda")

m.proj <- ProjectData(m.raw, e$eigenVectors[, c(1, 3)])
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(6)[unclass(as.factor(samsungData$activity))], 
     xlab="first", ylab="third", 
     main="Two components; actions")

kmeans.inner <- kmeans(m.proj, 3)
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(3)[unclass(as.factor(kmeans.inner$cluster))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; kmeans \n cost = ", kmeans.inner$tot.withinss/2, sep=""))


KMeans.em.result <- KMeans.em(k, m.proj, HalfSqEuclidian.distance, 
                              HalfSqEuclidian.centroid, showLog = T)
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(k)[unclass(as.factor(KMeans.em.result$labels))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; KMeans.em \n cost = ", KMeans.em.result$cost[length(KMeans.em.result$cost)], sep=""))

KMeans.gd.result <- KMeans.gd(k, m.proj, HalfSqEuclidian.distance, 
                              HalfSqEuclidian.derivative, showLog = T)
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(k)[unclass(as.factor(KMeans.gd.result$labels))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; KMeans.gd \n cost = ", KMeans.gd.result$cost[length(KMeans.gd.result$cost)], sep=""))


plot(KMeans.em.result$cost, col="red", type="l", xlab="iteration", ylab="cost", main="kmeans.em training",
     ylim=c(0, 100000))

plot(KMeans.gd.result$cost, col="red", type="l", xlab="iteration", ylab="cost", main="kmeans.gd training",
     ylim=c(0, 100000))



