library(combinat)

reg.linear.cost <- function(x, y)
{
  # x and y have equal length
  # 
  # Returns:
  #   mean of residuals
  x <- as.vector(x)
  y <- as.vector(y)
  return(sum((x - y)^2)/length(x))
}

reg.linear <- function(x, y, lambda = 0.1, maxIterations = 1000, accuracy=0.0001, minError = 0, showLog = F)
{
  # returns regression coefficients
  # 
  # Args: 
  #   x: is a matrix where observations are locater in rows
  #   y: is vector of criterial variables
  #   lambda: learning rate
  #   maxIterations = 1000, accuracy=0.0001, minError = 0, showLog = F
  #
  # Returns:
  #   vector of regression coefficients including bias (last element)
  
  #add bias column and init others
  x <- cbind(x, rep(1, dim(x)[1]))
  theta <- matrix(runif(n=dim(x)[2], min=-1, max=1), ncol=1, nrow=dim(x)[2])
  y <- matrix(y, ncol=1, nrow=length(y))
  
  cost <- c()
  for(iter in 1:maxIterations)
  {
    #outputs
    o <- x %*% theta
    
    #stoppers
    cost <- append(cost, reg.linear.cost(y, o))
    if(showLog)
    {
      print(paste("Iteration #", iter, ", cost = ", cost[iter], sep=""))
    }
    if(iter > 1)
    {
      if(abs(cost[iter - 1] - cost[iter]) <= accuracy)
      {
        break
      }
      if(cost[iter] <= minError)
      {
        break
      }
    }    
    
    #residuals
    r <- o - y
    
    #gradient
    nabla <- t(2 * (t(r) %*% x) / dim(x)[1])
    
    #update weights
    theta <- theta - lambda*nabla
    
  }
  
  return(list(
      theta = theta,
      cost = cost
    ))
}


reg.linear.iterative <- function(x, y, lambda = 0.1, maxIterations = 1000, accuracy=0.0001, minError = 0, showLog = F)
{
  # returns regression coefficients
  # 
  # Args: 
  #   x: is a matrix where observations are locater in rows
  #   y: is vector of criterial variables
  #   lambda: learning rate
  #   maxIterations = 1000, accuracy=0.0001, minError = 0, showLog = F
  #
  # Returns:
  #   vector of regression coefficients including bias (last element)
  
  #add bias column and init others
  x <- cbind(x, rep(1, dim(x)[1]))
  theta <- matrix(runif(n=dim(x)[2], min=-1, max=1), ncol=1, nrow=dim(x)[2])
  y <- matrix(y, ncol=1, nrow=length(y))
  
  cost <- c()
  for(iter in 1:maxIterations)
  {
    #outputs
    o <- rep(NA, dim(x)[1])
    for(i in 1:dim(x)[1])
    {
      o[i] <- 0
      for(j in 1:dim(x)[2])
      {
        o[i] <- o[i] + theta[j]*x[i, j]
      }
    }
    
    #stoppers
    cost <- append(cost, reg.linear.cost(y, o))
    if(showLog)
    {
      print(paste("Iteration #", iter, ", cost = ", cost[iter], sep=""))
    }
    if(iter > 1)
    {
      if(abs(cost[iter - 1] - cost[iter]) <= accuracy)
      {
        break
      }
      if(cost[iter] <= minError)
      {
        break
      }
    }    
    
    #residuals
    r <- o - y
    
    #gradient
    nabla <- rep(NA, dim(x)[2])
    for(k in 1:dim(x)[2])
    {
      r <- 0
      for(i in 1:dim(x)[1])
      {
        r <- r + (y[i] - o[i])*x[i, k]
      }
      r <- -2*r/dim(x)[1]
      nabla[k] <- r
    }
    
    #update weights
    theta <- theta - lambda*nabla
    
  }
  
  return(list(
    theta = theta,
    cost = cost
  ))
}


generate2PolyFeatures <- function(m)
{
  c <- combn(dim(m)[2], 2)
  d <- matrix(rep(NA, dim(m)[1] * (dim(m)[2] + dim(c)[2])), nrow=dim(m)[1], ncol=dim(m)[2] + dim(c)[2])
  for(k in 1:dim(m)[1])
  {
    d[k, 1:dim(m)[2]] <- m[k, ]
    for(i in 1:dim(c)[2])
    {
      d[k, dim(m)[2] + i] <- m[k, c[1, i]]*m[k, c[2, i]]
    }
  }
  return(d)
}

sigmoid <- function(x)
{
  return(1/(1 + exp(-x)))
}

reg.logistic.cost <- function(y, o)
{
  #correct 1 and 0 with epsilon to avoid infinity 
  idx0 <- which(o == 0)
  o[idx0] <- o[idx0] + .Machine$double.eps
  idx1 <- which(o == 1)
  o[idx1] <- o[idx1] - .Machine$double.eps    
  
  # real output is x, expected is y
  return(-sum(y*log(o) + (1 - y)*log(1 - o))/length(y))
}

reg.logistic <- function(x, y, lambda = 0.1, maxIterations = 1000, accuracy=0.0001, minError = -Inf, 
                         x.cv = NA, y.cv = NA, cvCostIncreaseStop = F, iterStopSkip = 1, showLog = F)
{
  
  # returns regression coefficients
  # 
  # Args: 
  #   x: is a matrix where observations are locater in rows
  #   y: is vector of criterial variables
  #   lambda: learning rate
  #   maxIterations = 1000, accuracy=0.0001, minError = 0, showLog = F
  #
  # Returns:
  #   vector of regression coefficients including bias (last element)
  
  #add bias column and init others
  x <- cbind(x, rep(1, dim(x)[1]))
  theta <- matrix(rnorm(mean=0, sd=1, n=dim(x)[2]), ncol=1, nrow=dim(x)[2])
  y <- matrix(y, ncol=1, nrow=length(y))
  
  if(!is.na(x.cv[1]))
  {
    x.cv <- cbind(x.cv, rep(1, dim(x.cv)[1]))
  }
  
  cost <- c()
  cvCost<- c()
  for(iter in 1:maxIterations)
  {
    #outputs
    o <- sigmoid(x %*% theta)
    
    #stoppers
    cost <- append(cost, reg.logistic.cost(y, o))
    if(!is.na(x.cv[1]))
    {
      o.cv <- sigmoid(x.cv %*% theta)
      cvCost <- append(cvCost, reg.logistic.cost(y.cv, o.cv))
    }
    if(showLog)
    {
      if(!is.na(x.cv[1]))
      {
        print(paste("Iteration #", iter, ", cost = ", cost[iter], "; cv = ", cvCost[iter], sep=""))
      }
      else
      {
        print(paste("Iteration #", iter, ", cost = ", cost[iter], sep=""))
      }
    }
    if(iter > iterStopSkip)
    {
      if(cost[iter - 1] - cost[iter] > accuracy)
      {
        break
      }
      if(cost[iter] <= minError)
      {
        break
      }
      if(!is.na(x.cv[1]))
      {
        if(cvCostIncreaseStop && cvCost[iter - 1] < cvCost[iter])
        {
          break
        }
      }
    }    
    
    #gradient
    nabla <- -(t(x) %*% (y - o))/dim(x)[1]
    
    #update weights
    theta <- theta - lambda*nabla
    
  }
  
  return(list(
    theta = theta,
    cost = cost,
    cvCost = cvCost
  ))
  
  
  
  
  
}

reg.logistic.predict <- function(theta, x, p = 0.5)
{
  p <- sigmoid(cbind(x, rep(1, dim(x)[1])) %*% theta)
  o <- as.numeric(p > 0.5)
  return(o)
}

binaryClassifierEvaluation <- function(y, o, positiveLabel = 1, negativeLable = 0)
{
  # returns list with evaluation information
  # 
  # Args: 
  #   y: real labels
  #   o: predicted lables
  #
  # Returns:
  #   returns list with evaluation information
  tp <- sum((y == positiveLabel) & (o == positiveLabel))
  tn <- sum((y == negativeLable) & (o == negativeLable))
  fp <- sum((y == negativeLable) & (o == positiveLabel))
  fn <- sum((y == positiveLabel) & (o == negativeLable))
  
  ppv <- tp/(tp + fp)
  npv <- tn/(fn + tn)
  tpr <- tp/(tp + fn)
  tnr <- tn/(fp + tn)
  
  acc <- (tp + tn)/(tp + fp + fn + tn)
  
  f1score <- 2*ppv*tpr/(ppv + tpr)
  
  mcc.d <- rep(NA, 4)
  mcc.d[1] <- sqrt(tp + fp)
  mcc.d[2] <- sqrt(tp + fn)
  mcc.d[3] <- sqrt(tn + fp)
  mcc.d[4] <- sqrt(tn + fn)
  mcc.dv <- prod(mcc.d) 
  if(mcc.dv == 0)
  {
    mcc.dv <- 1
  }
  mcc <- (tp*tn - fp*fn)/mcc.dv
  
  return(list(
      TP = tp,
      TN = tn,
      FP = fp,
      FN = fn,
      PPV_Precision = ppv,
      TPR_Recall = tpr,
      NPV = npv,
      TNR = tnr,
      Accuracy = acc,
      F1_Score = f1score,
      MatthewsCorrelationCoefficient = mcc
    ))
}