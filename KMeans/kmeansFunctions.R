HalfSqEuclidian.distance <- function(u, v)
{
  # Half of Squeared of Euclidian distance between two vectors
  #
  # Args:
  #   u: first vector
  #   v: second vector
  #
  # Returns:
  #   value of distance
  return(sum((u-v)*(u-v))/2)   
}

HalfSqEuclidian.derivative <- function(u, v, i)
{
  # Partial derivative of Half of Squeared of Euclidian distance 
  # between two vectors
  #
  # Args:
  #   u: first vector
  #   v: second vector
  #   i: index of part of second vector
  #
  # Returns:
  #   value of derivative of the distance
  return(v[i] - u[i])
}

DistMatrix <- function(data, distance, triangle = F)
{
  # Calculate distance matrix using given metrics
  #
  # Args:
  #   data: data frame or matrix (rows are observations)
  #   distance: distance function
  #   triangle: should matrix be triangle
  #
  # Returns:
  #   symmetric matix of distances
  n <- dim(data)[1]
  d <- matrix(rep(NA, n*n), nrow=n, ncol=n)
  for(i in 1:(n - 1))
  {
    d[i, i] <- 0
    for(j in (i + 1):n)
    {
      d[i, j] <- distance(data[i, ], data[j, ])
      if(!triangle)
      {
        d[j, i] <- d[i, j] 
      }
    }
  }
  d[n, n] <- 0
  return(d)  
}

HalfSqEuclidian.centroid <- function(data)
{
  # Calculate centroid of data using Half of Squeared of Euclidian distance 
  #
  # Args:
  #   data: data frame or matrix (rows are observations)
  #
  # Returns:
  #   centroid vector
  return(colMeans(data))
}

GetMedoid <- function(data, distance)
{
  # Get medoid of data (item that has minimum distance to all others items)
  #
  # Args:
  #   data: data frame or matrix (rows are observations)
  #   distance: metrics function that returns distance between two vectors
  #
  # Returns:
  #   list that contains
  #     - index of medoid item in data
  #     - cost of medoid
  #     - medoid vector
  cost <- rowSums(DistMatrix(m.proj[1:10, ], HalfSqEuclidian.distance))
  idx <- which(cost == min(cost))
  return (list(
    index = idx,
    cost = cost[idx],
    medoid = data[idx, ]
  ))
}

AveradeDistance <- function(distance, data, centroids)
{
  cost <- 0
  labels <- rep(NA, dim(data)[1])
  k <- dim(centroids)[1]
  c <- centroids
  
  for(i in 1:dim(data)[1])
  {
    d <- rep(NA, k)
    for(j in 1:k)
    {
      d[j] <- distance(data[i, ], c[j, ])
    }
    labels[i] <- which(d == min(d))
  }
  
  for(i in 1:length(labels))
  {
    cost <- cost + distance(data[i, ], c[labels[i], ])
  }
  cost <- cost / dim(data)[1]
  
  return(cost)
}

KMeans.em <- function(k, data, distance, centroid, 
                      accuracy = 0.1, maxIterations = 1000, 
                      initialCentroids = NULL, showLog = F)
{
  # Expectation-Maximization kmeans clustering algorithm
  #
  # Args:
  #   k: number of clusters
  #   data: data frame or matrix (rows are observations)
  #   distance: cost function / metrics
  #   centroid: centroid function of data
  #   accuracy: accuracy of calculation
  #   initialCentroids: initizalization of centroids
  #   showLog: show log
  
  n <- dim(data)[2]
  c <- initialCentroids
  
  InitNewCentroid <- function(m)
  {
    c <- data[sample(1:dim(data)[1], m), ]
  }
  
  if(is.null(initialCentroids))
  {
    c <- InitNewCentroid(k)
  }
  
  costVec <- vector()
  labels <- rep(NA, dim(data)[1])
  lastCost <- Inf
  for(iter in 1:maxIterations)
  {
    #expectation
    for(i in 1:dim(data)[1])
    {
      d <- rep(NA, k)
      for(j in 1:k)
      {
        d[j] <- distance(data[i, ], c[j, ])
      }
      labels[i] <- which(d == min(d))
    }
    
    #maximization
    for(i in 1:dim(c)[1])
    {
      cIds <- which(labels == i)
      if(length(cIds) == 0)
      {
        c[i, ] <- InitNewCentroid(1)
        next
      }
      c[i, ] <- centroid(data[which(labels == i), ])
    }
    
    #cost
    cost <- 0
    for(i in 1:length(labels))
    {
      cost <- cost + distance(data[i, ], c[labels[i], ])
    }
    if(showLog)
    {
      print(paste("Iter: ", iter,"; Cost = ", cost, sep=""))
    }
    costVec <- append(costVec, cost)
    if(abs(lastCost - cost) < accuracy)
    {
      break
    }
    lastCost <- cost
  }
  
  return(list(
    labels = labels,
    cost = costVec,
    centroids = c
  ))
}

KMeans.gd <- function(k, data, distance, derivative,
                      accuracy = 0.1, maxIterations = 1000, 
                      learningRate = 1, 
                      initialCentroids = NULL, showLog = F)
{
  # Gradient descent version of kmeans clustering algorithm
  #
  # Args:
  #   k: number of clusters
  #   data: data frame or matrix (rows are observations)
  #   distance: cost function / metrics
  #   centroid: centroid function of data
  #   accuracy: accuracy of calculation
  #   learningRate: learning rate
  #   initialCentroids: initizalization of centroids
  #   showLog: show log
  
  n <- dim(data)[2]
  c <- initialCentroids
  
  InitNewCentroid <- function(m)
  {
    c <- data[sample(1:dim(data)[1], m), ]
  }
  
  if(is.null(initialCentroids))
  {
    c <- InitNewCentroid(k)
  }
  
  costVec <- vector()
  cost <- NA
  d <- NA
  lastCost <- Inf
  for(iter in 1:maxIterations)
  {
    g <- matrix(rep(NA, n*k), nrow=k, ncol=n)
    
    #calculate distances between centroids and data
    d <- matrix(rep(NA, k*dim(data)[1]), nrow=k, ncol=dim(data)[1])
    for(i in 1:k)
    {
      d[i, ] <- apply(data, 1, FUN = function(v) {distance(v, c[i, ])})
    }
    
    #calculate cost
    cost <- 0
    for(i in 1:dim(data)[1])
    {
      cost <- cost + min(d[, i])
    }
    if(showLog)
    {
      print(paste("Iter: ", iter,"; Cost = ", cost, sep=""))
    }
    costVec <- append(costVec, cost)
    
    #stop conditions
    if(abs(lastCost - cost) < accuracy)
    {
      break
    }
    lastCost <- cost
    
    #calculate gradients
    for(a in 1:k)
    {
      for(b in 1:n)
      {
        g[a, b] <- 0
        for(i in 1:dim(data)[1])
        {
          if(min(d[, i]) == d[a, i])
          {
            g[a, b] <- g[a, b] + derivative(data[i, ], c[a, ], b)
          }
        }
        
      }
    }
    
    #update centroids
    for(a in 1:k)
    {
      for(b in 1:n)
      {
        c[a, b] <- c[a, b] - learningRate*g[a, b]/dim(data)[1]
      }
    }
  }
  
  labels <- rep(NA, dim(data)[1])
  for(i in 1:dim(data)[1])
  {
    labels[i] <- which(d[, i] == min(d[, i]))
  }
  return(list(
    labels = labels,
    cost = costVec,
    centroids = c
  ))
}




