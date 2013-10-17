#http://en.wikipedia.org/wiki/Cluster_analysis#Internal_evaluation
#Davies–Bouldin index

DBIndex <- function(distance, data, centroids)
{
  # Davies–Bouldin index
  #
  # Args:
  #   data: data frame or matrix (rows are observations)
  #   distance: cost function / metrics
  #   centroid: centroid function of data

  
  k <- dim(centroids)[1]
  c <- centroids
  
  #calculate deltas: average distance of data in clusters
  d <- matrix(rep(NA, k*dim(data)[1]), nrow=k, ncol=dim(data)[1])
  for(i in 1:k)
  {
    d[i, ] <- apply(data, 1, FUN = function(v) {distance(v, c[i, ])})
  }
  labels <- apply(d, 2, which.min)
  deltas <- rep(NA, k)
  for(i in 1:k)
  {
    deltas[i] <- sum(d[i, which(labels == i)]) / sum(labels == i)
  }    
  
  #calculate cost
  cost <- 0
  partCostMatrix <- matrix(rep(NA, k*k), ncol=k, nrow=k)
  for(i in 1:k)
  {
    tmpVector <- rep(NA, k)
    for(j in 1:k)
    {
      if(i == j)
      {
        next
      }
      partCostMatrix[i, j] <- (deltas[i] + deltas[j]) / distance(c[i, ], c[j, ])
    }
    cost <- cost + max(partCostMatrix[i, ], na.rm=T)
  }
  cost <- cost / k
  
  return(cost)
}

  
KMeans.DB <- function(k, data, distance, derivative,
                                accuracy = 0.001, maxIterations = 1000, 
                                learningRate = 1, 
                                initialCentroids = NULL, showLog = F)
{
  # Gradient descent over Davies–Bouldin index
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
  
  dataCount <- dim(data)[1]
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
    #calculate deltas: average distance of data in clusters
    d <- matrix(rep(NA, k*dim(data)[1]), nrow=k, ncol=dim(data)[1])
    for(i in 1:k)
    {
      d[i, ] <- apply(data, 1, FUN = function(v) {distance(v, c[i, ])})
    }
    labels <- apply(d, 2, which.min)
    deltas <- rep(NA, k)
    for(i in 1:k)
    {
      deltas[i] <- sum(d[i, which(labels == i)]) / sum(labels == i)
    }    
    
    #calculate cost
    cost <- 0
    partCostMatrix <- matrix(rep(NA, k*k), ncol=k, nrow=k)
    for(i in 1:k)
    {
      tmpVector <- rep(NA, k)
      for(j in 1:k)
      {
        if(i == j)
        {
          next
        }
        partCostMatrix[i, j] <- (deltas[i] + deltas[j]) / distance(c[i, ], c[j, ])
      }
      cost <- cost + max(partCostMatrix[i, ], na.rm=T)
    }
    cost <- cost / k
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
    g <- matrix(rep(NA, n*k), nrow=k, ncol=n)
    tmp <- c()
    for(a in 1:k)
    {
      for(b in 1:n)
      {
        g[a, b] <- 0
        
        #loop over clusters, it is sum
        for(i in 1:k)
        {
          
          #loop over clusters, it is max
          for(j in 1:k)
          {
            if(i == j)
            {
              next
            }
            
            #derivative of max, case when a is NOT index of max
            if(which(max(partCostMatrix[i, ], na.rm=T) == partCostMatrix[i, ]) != a)
            {
              next
            }
            
            #derivative of max, case when a is index of max            
            firstItem <- 0
            secondItem <- 0
            if(i == a | j == a)
            {
              #first item in sum
              aCluster <- which(labels == a)
              for(p in 1:length(aCluster))
              {
                firstItem <- firstItem + derivative(data[p, ], c[a, ], b)
              }
              firstItem <- firstItem / (length(aCluster) * distance(c[i, ], c[j, ]))
              
              #second item in sum
              q <- i #i != a
              if(i == a)
              {
                q <- j
              }
              secondItem <- (deltas[i] + deltas[j]) *
                            derivative(c[q, ], c[a, ], b) / 
                            distance(c[i, ], c[j, ])^2
            }
            
            g[a, b] <- g[a, b] + firstItem - secondItem
            
            if(a == 2 & b == 1)
            {
              tmp <- append(tmp, firstItem - secondItem)
            }
            
          }
          
        }
        
        g[a, b] <- g[a, b] / k
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








