covMatrix <- function(data)
{
  # Compute covariance matrix of data, same as cov(data)
  #
  # Args:
  #   data: rows are observations
  #
  # Returns:
  #   covariance matrix
  
  n <- dim(data)[1]
  m <- dim(data)[2]
  
  data.cov <- matrix(rep(NA, m*m), nrow=m, ncol=m)
  
  #denominator is n
  #means <- unname(colMeans(m.proj))
  means <- unname(apply(data, 2, function(v) { sum(v) / n }))
  
  #denominator is (n - 1), ubbiased estimation
  for(i in 1:m)
  {
    for(j in i:m)
    {
      data.cov[i, j] <- (data[, i] - means[i])%*%(data[, j] - means[j]) / (n - 1)
    }
    if(i != j)
    {
      data.cov[j, i] <- data.cov[i, j]
    }
  }

  return(data.cov)
}


Mahalanobis.distance <- function(x, y, m, mIsInverted = F)
{
  # Compute Mahalanobis distance between vector x and y, using covariance matrix m
  #
  # Args:
  #   x, y: vectors
  #   m: covariance matrix
  #   mIsInverted: is matrix m is inverted (inverted covariance matrix is needed for Mahalanobis distance) 
  #
  # Returns:
  #   scalar value
  
  m.inv <- m
  if(!mIsInverted)
  {
    m.inv <- solve(m)
  }
  
  return(sqrt((x - y) %*% m.inv %*% (x - y)))  
}

KMeans.Mahalanobis.em <- function(k, data,
                                  accuracy = 0.1, maxIterations = 1000, 
                                  initialCentroids = NULL, showLog = F)
{
  # Expectation-Maximization kmeans clustering algorithm
  #
  # Args:
  #   k: number of clusters
  #   data: data frame or matrix (rows are observations)
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
  
  m <- rep(list(diag(n)), k)
  
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
        d[j] <- Mahalanobis.distance(data[i, ], c[j, ], m[[k]])
      }
      labels[i] <- which(d == min(d))
    }
    
    #cost
    cost <- 0
    for(i in 1:length(labels))
    {
      cost <- cost + Mahalanobis.distance(data[i, ], c[labels[i], ], m[[labels[i]]])
    }
    cost <- cost / dim(data)[1]
    if(showLog)
    {
      print(paste("Iter: ", iter,"; Cost = ", cost, sep=""))
    }
    costVec <- append(costVec, cost)
    if(abs(lastCost - cost) < accuracy)
    {
      break
    }
    if(lastCost < cost)
    {
      break
    }
    lastCost <- cost
    
    #maximization
    for(i in 1:dim(c)[1])
    {
      cIds <- which(labels == i)
      if(length(cIds) == 0)
      {
        c[i, ] <- InitNewCentroid(1)
        next
      }
      c[i, ] <- colMeans(data[which(labels == i), ])
    }
    
    #update covariance matrices
    if(iter > 0)
    {
      for(i in 1:k)
      {
        m[[i]] <- covMatrix(data[which(labels == i), ])
      }
    }
  }
  
  return(list(
    labels = labels,
    cost = costVec,
    centroids = c,
    covMatrices = m
  ))
}


KMeans.Mahalanobis.gd <- function(k, data, accuracy = 0.1, maxIterations = 1000, minCost = -Inf,
                                  learningRate = 1, initialCentroids = NULL, noChangeBreak = 100,
                                  showLog = F)
{
  # Gradient descent version of kmeans clustering algorithm
  #
  # Args:
  #   k: number of clusters
  #   data: data frame or matrix (rows are observations)
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
  
  m <- rep(list(diag(n)), k)
  
  costVec <- vector()
  cost <- NA
  d <- NA
  lastCost <- Inf
  lastLabels <- rep(0, dim(data)[1])
  noChangeCount <- 0
  for(iter in 1:maxIterations)
  {    
    #calculate distances between centroids and data
    d <- matrix(rep(NA, k*dim(data)[1]), nrow=k, ncol=dim(data)[1])
    for(i in 1:k)
    {
      #d[i, ] <- apply(data, 1, FUN = function(v) {Mahalanobis.distance(v, c[i, ], m[[i]], mIsInverted=T)})
      for(a in 1:dim(data)[1])
      {
        tmp <- (data[a, ] - c[i, ]) %*% m[[i]] %*% (data[a, ] - c[i, ])
        if(tmp < 0)
        {
          print("error sqrt")
          return(list(
            centroids = c,
            covMatrices = m,
            clusterIdx = i,
            dataIdx = a
          ))
        }
        d[i, a] <- Mahalanobis.distance(data[a, ], c[i, ], m[[i]], mIsInverted=T)
      }
    }
    labels <- apply(d, 2, FUN = which.min)    
    
    #calculate cost
    cost <- 0
    for(i in 1:dim(data)[1])
    {
      cost <- cost + min(d[, i])
    }
    cost <- cost / dim(data)[1]
    if(showLog)
    {
      print(paste("Iter: ", iter,"; Cost = ", cost, sep=""))
    }
    costVec <- append(costVec, cost)
    
    #stop conditions
    if(cost < minCost)
    {
      print("minCost break")
      break
    }
    if(abs(lastCost - cost) < accuracy)
    {
      print("accuracy break")
      break
    }
    lastCost <- cost
    if(sum(lastLabels == labels) == length(labels))
    {
      noChangeCount <- noChangeCount + 1
      if(noChangeCount >= noChangeBreak)
      {
        print("no change break")
        break
      }
    }
    else
    {
      noChangeCount <- 0
    }
    lastLabels <- labels
    
    g.c <- matrix(rep(0, n*k), nrow=k, ncol=n)
    g.m <- rep(list(matrix(rep(0, n*n), ncol=n, nrow=n)), k)
    
    #calculate gradients of centroids and matices
    for(b in 1:k)
    {
      y <- c[b, ]
      s <- m[[b]]
      
      clusterData <- data[which(labels == b), ]
      if(dim(clusterData)[1] == 0)
      {
        next
      }
      
      #gradient of centroids
      for(t in 1:n)
      {
        g.c[b, t] <- 0        
        for(a in 1:dim(clusterData)[1])
        {
          #calculate diff of Mahalanobis distance x_a = data[a, ], c_b = c[b, ]
          x <- clusterData[a, ]
          
          tmp.Gradient <- (y[t] - x[t]) * s[t, t]
          
          tmp <- 0
          if(t - 1 > 0)
          {
            for(i in 1:(t - 1))
            {
              tmp <- tmp + (y[i] - x[i]) * s[i, t]
            }  
          }
          tmp.Gradient <- tmp.Gradient + tmp
          
          tmp <- 0
          if(t + 1 <= n)
          {
            for(j in (t + 1):n)
            {
              tmp <- tmp + (y[j] - x[j]) * s[t, j]
            }
          }
          tmp.Gradient <- tmp.Gradient + tmp
          
          g.c[b, t] <- g.c[b, t] + 2*tmp.Gradient
        }
      }
      
      #gradient of matrices
      if(T)
      {
        for(i in 1:n)
        {
          for(j in i:n)
          {
            g.m[[b]][i, j] <- 0
            for(a in 1:dim(clusterData)[1])
            {
              x <- clusterData[a, ]
              tmp.Gradient <- (x[i] - y[i])*(x[j] - y[j]) + m[[b]][i, j]
              g.m[[b]][i, j] <- g.m[[b]][i, j] + tmp.Gradient
              if(i != j)
              {
                g.m[[b]][j, i] <- g.m[[b]][j, i] + tmp.Gradient
              }
            }
          }
        }
      }
      
    }
    
            
        
    
    
    
    
    #update parameters
    for(b in 1:k)
    {
      
      denominator <- sum(labels == b)
      
      #update centroids
      for(t in 1:n)
      {
        c[b, t] <- c[b, t] - learningRate*g.c[b, t] / denominator
      }
      
      #update matrices
      m.tmp <- m[[b]]
      lr.tmp <- learningRate/1000
      tries <- 0
      while(T)
      {
        flag <- T
        
        for(i in 1:n)
        {
          for(j in 1:n)
          {
            m[[b]][i, j] <- m[[b]][i, j] - lr.tmp * g.m[[b]][i, j] / denominator
          }
        }
        
        m.inv.tmp <- NA
        try(m.inv.tmp <- solve(m[[b]]), silent=T)
        if(sum(is.na(m.inv.tmp)) > 0)
        {
          m[[b]] <- m.tmp
          break
        }
        
        for(a in 1:dim(data)[1])
        {          
          if(data[a, ] %*% m.inv.tmp %*% data[a, ] < 0)
          {
            m[[b]] <- m.tmp
            flag <- F
            lr.tmp <- lr.tmp/10
            break
          }
        }
        
        if(flag)
        {
          #print(m[[b]])
          break
        }
        
        tries <- tries + 1
        if(tries > 100)
        {
          m[[b]] <- m.tmp
          break
        }
      }
      
                  
      #e <- eigen(m[[b]])
      #m[[b]] <- m[[b]]/(e$values[2])
      
    }
    
  }
  
  for(i in 1:k)
  {
    m[[i]] <- solve(m[[i]])
  }
  
  labels <- rep(NA, dim(data)[1])
  for(i in 1:dim(data)[1])
  {
    labels[i] <- which(d[, i] == min(d[, i]))
  }
  return(list(
    labels = labels,
    cost = costVec,
    centroids = c,
    covMatrices = m
  ))
}
