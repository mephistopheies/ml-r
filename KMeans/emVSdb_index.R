rm(list=ls())

source("KMeans/kmeansFunctions.R")
source("KMeans/kmeansDaviesBouldinIndexFunctions.R")
set.seed(148800)
k <- 3

source("PCA/pcaFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m.raw <- samsungData[, 1:(dim(samsungData)[2] - 2)]
load("./Data/MobilePhone/eigen.Rda")

m.proj <- ProjectData(m.raw, e$eigenVectors[, c(1, 3)])
plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(6)[unclass(as.factor(samsungData$activity))], 
     xlab="first", ylab="third", 
     main="Two components; actions")

InitNewCentroid <- function(data, m)
{
  c <- data[sample(1:dim(data)[1], m), ]
}

k <- 3
em.avd <- c()
em.dbi <- c()
db.dbi <- c()
db.avd <- c()
iterNumber <- 5
for(iter in 1:iterNumber)
{
  initCentroids <- InitNewCentroid(m.proj, k)
  
  KMeans.em.result <- KMeans.em(k, m.proj, HalfSqEuclidian.distance, 
                                HalfSqEuclidian.centroid, showLog = T, initialCentroids = initCentroids)
  
  KMeans.DB.result <- KMeans.DB(k, m.proj, HalfSqEuclidian.distance, HalfSqEuclidian.derivative, 
                                showLog = T, accuracy = 0.001, maxIterations = 1000, learningRate = 0.1,
                                initialCentroids = initCentroids)
  
  em.avd <- append(em.avd, KMeans.em.result$cost[length(KMeans.em.result$cost)]/dim(m.proj)[1])
  em.dbi <- append(em.dbi, DBIndex(HalfSqEuclidian.distance, m.proj, KMeans.em.result$centroids))
  
  db.dbi <- append(db.dbi, KMeans.DB.result$cost[length(KMeans.DB.result$cost)])
  db.avd <- append(db.avd, AveradeDistance(HalfSqEuclidian.distance, m.proj, KMeans.DB.result$centroids))
  
}












plot(m.proj[, 1], m.proj[, 2], pch=19, 
     col=rainbow(k)[unclass(as.factor(KMeans.em.result$labels))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; KMeans.em \n cost = ", KMeans.em.result$cost[length(KMeans.em.result$cost)], sep=""))