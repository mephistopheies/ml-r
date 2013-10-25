rm(list=ls())
library(ellipse)
set.seed(10488)

k <- 4
#par(mfrow=c(3,2))
source("PCA/pcaFunctions.R")
source("KMeans/kmeansMahalanobisFunctions.R")
source("KMeans/helpFunctions.R")
source("KMeans/kmeansFunctions.R")
source("KMeans/kmeansDaviesBouldinIndexFunctions.R")
load("Data/MobilePhone/samsungData.rda")
m.raw <- samsungData[, 1:(dim(samsungData)[2] - 2)]
load("./Data/MobilePhone/eigen.Rda")
m.proj <- ProjectData(m.raw, e$eigenVectors[, c(2, 3)])

InitNewCentroid <- function(m, data)
{
  c <- data[sample(1:dim(data)[1], m), ]
}


prms <- matrix(c(0, 0, 12, 5, 2000, 0,
                 45, 0, 2, 10, 1000, 45,
                 0, -30, 5, 5, 1000, 0,
                 -35, -20, 2, 10, 1000, 45), 
               nrow=4, ncol=6, byrow=T)
data <- generateSet(prms)


plot(data[, 1], data[, 2], pch=19, asp=1, 
     col=rainbow(length(unique(data[, 3])), alpha=0.2)[unclass(as.factor(data[, 3]))],
     xlab="x", ylab="y", 
     main="artificial data")
data <- data[, 1:2]
initCentroids <- InitNewCentroid(k, data)
points(initCentroids[, 1], initCentroids[, 2], col="black", pch=19)


#kmeans EM
if(F)
{  
KMeans.em.result <- KMeans.em(k, data, showLog=T, initialCentroids=initCentroids, distance=HalfSqEuclidian.distance,
                              centroid=HalfSqEuclidian.centroid)

plot(data[, 1], data[, 2], pch=19, asp=1,
     col=rainbow(k)[unclass(as.factor(KMeans.em.result$labels))],
     xlab="first", ylab="third", 
     main=paste(k, " clusters; KMeans.em \n cost = ", KMeans.em.result$cost[length(KMeans.em.result$cost)], sep=""))

points(KMeans.em.result$centroids[, 1], KMeans.em.result$centroids[, 2], col="black", pch=19)

kmeans.dbi <- DBIndex(HalfSqEuclidian.distance, data, KMeans.em.result$centroids, KMeans.em.result$labels)
}

#kmeans gradient
if(F)
{
  KMeans.gd.result <-  KMeans.gd(k, data, accuracy = 0.01, maxIterations = 1000, 
                                 distance=HalfSqEuclidian.distance, derivative=HalfSqEuclidian.derivative,
                                 learningRate = 0.1, initialCentroids = initCentroids, showLog = T)
  
  plot(data[, 1], data[, 2], pch=19, asp=1,
       col=rainbow(k)[unclass(as.factor(KMeans.gd.result$labels))],
       xlab="first", ylab="third", 
       main=paste(k, " clusters; KMeans.gd \n cost = ", KMeans.gd.result$cost[length(KMeans.gd.result$cost)], sep=""))
  points(KMeans.gd.result$centroids[, 1], KMeans.gd.result$centroids[, 2], col="black", pch=19)

}

#mahalanobis EM
if(F)
{
  KMeans.Mahalanobis.em.result <- KMeans.Mahalanobis.em(k, data, showLog=T, initialCentroids=initCentroids)
  
  plot(data[, 1], data[, 2], pch=19, asp=1,
       col=rainbow(k)[unclass(as.factor(KMeans.Mahalanobis.em.result$labels))],
       xlab="first", ylab="third", 
       main=paste(k, " clusters; KMeans.em \n cost = ", KMeans.Mahalanobis.em.result$cost[length(KMeans.Mahalanobis.em.result$cost)], sep=""))
  
  points(KMeans.Mahalanobis.em.result$centroids[, 1], KMeans.Mahalanobis.em.result$centroids[, 2], col="black", pch=19)
  
  for(i in 1:k)
  {
    points(ellipse(KMeans.Mahalanobis.em.result$covMatrices[[i]], 
                   centre=KMeans.Mahalanobis.em.result$centroids[i, ]), 
           col="black", type="l")
  }
  
  mahalanobis.dbi <- DBIndex(HalfSqEuclidian.distance, data, KMeans.Mahalanobis.em.result$centroids, KMeans.Mahalanobis.em.result$labels)
  
}

#mahalanobis gradient
if(T)
{
  KMeans.Mahalanobis.gd.result <- KMeans.Mahalanobis.gd(k, data, accuracy = 0.01, maxIterations = 1000, 
                                                        learningRate = 0.1, initialCentroids = initCentroids, showLog = T)
  
  if(T)
  {
    c <- KMeans.Mahalanobis.gd.result$centroids
    m <- KMeans.Mahalanobis.gd.result$covMatrices
    i <- KMeans.Mahalanobis.gd.result$clusterIdx
    a <- KMeans.Mahalanobis.gd.result$dataIdx
    x <- data[a, ]
    y <- c[i, ]
  }
  
  plot(data[, 1], data[, 2], pch=19, asp=1,
       col=rainbow(k)[unclass(as.factor(KMeans.Mahalanobis.gd.result$labels))],
       xlab="first", ylab="third", 
       main=paste(k, " clusters; KMeans.Mahalanobis.gd \n cost = ", KMeans.Mahalanobis.gd.result$cost[length(KMeans.Mahalanobis.gd.result$cost)], sep=""))
  points(KMeans.Mahalanobis.gd.result$centroids[, 1], KMeans.Mahalanobis.gd.result$centroids[, 2], col="black", pch=19)
  for(i in 1:k)
  {
    points(ellipse(KMeans.Mahalanobis.gd.result$covMatrices[[i]], 
                   centre=KMeans.Mahalanobis.gd.result$centroids[i, ]), 
           col="black", type="l")
  }
}





