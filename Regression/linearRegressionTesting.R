rm(list = ls())
set.seed(1488)
source("Regression/regressionFunctions.R")

hdata <- read.csv("Data/Regression/housing.data.csv")

if(F)
{
  hist(hdata$MEDV, breaks=50, col="red", 
       main="Median value of owner-occupied homes",
       xlab="price (in $1000's)")
}

if(F)
{
  xfield <- "DIS"
  yfield <- "NOX"
  pMin <- floor(min(hdata$MEDV))
  pMax <- ceiling(max(hdata$MEDV))
  c <- pMax - pMin
  v <- round(hdata$MEDV)
  x <- hdata[xfield]
  y <- hdata[yfield]
  plot(x[[1]], y[[1]], 
       xlab=xfield,
       ylab=yfield,
       main=paste("price VS ", xfield, " & ", yfield, sep=""),
       pch=19,
       col=heat.colors(c, alpha=0.8)[v])
}


if(F)
{
  x <- as.matrix(hdata[, 1:13])
  y <- as.double(hdata[, 14])
  
  r.lin.fit.all <- reg.linear.iterative(x, y, lambda = 0.000001, maxIterations = 1000000, 
                          accuracy=0.0001, minError = 0, showLog = T)
  plot(r.lin.fit.all$cost[10:length(r.lin.fit.all$cost)], type="l", col="red", 
       main="cost function", xlab="iteration", ylab="value")
  
  z.all <- cbind(x, rep(1, dim(x)[1]))
  o.all <- z.all %*% r.lin.fit.all$theta
}

if(F)
{
  x <- as.matrix(hdata[, 7])
  y <- as.double(hdata[, 14])
  
  r.lin.fit.age <- reg.linear(x, y, lambda = 0.0001, maxIterations = 100000, 
                          accuracy=0.0001, minError = 0, showLog = T)
  plot(r.lin.fit.age$cost[1:length(r.lin.fit.age$cost)], type="l", col="red", 
       main="cost function", xlab="iteration", ylab="value")
  
  z.age <- cbind(x, rep(1, dim(x)[1]))
  o.age <- z.age %*% r.lin.fit.age$theta
  
  plot(hdata$MEDV, hdata$AGE, 
       xlab="MEDV",
       ylab="AGE",
       main="MEDV vs AGE",
       pch=19,
       col=rgb(1, 0, 0, alpha = 0.8))
  abline(r.lin.fit.age$theta[2], r.lin.fit.age$theta[1], col="blue")
}

if(F)
{
  x <- as.matrix(hdata[, 1])
  y <- as.double(hdata[, 14])
  
  r.lin.fit.age <- reg.linear(x, y, lambda = 0.0001, maxIterations = 100000, 
                              accuracy=0.0001, minError = 0, showLog = T)
  plot(r.lin.fit.age$cost[1:length(r.lin.fit.age$cost)], type="l", col="red", 
       main="cost function", xlab="iteration", ylab="value")
  
  z.age <- cbind(x, rep(1, dim(x)[1]))
  o.age <- z.age %*% r.lin.fit.age$theta
  
  plot(hdata$MEDV, hdata$CRIM, 
       xlab="MEDV",
       ylab="CRIM",
       main="MEDV vs AGE",
       pch=19,
       col=rgb(1, 0, 0, alpha = 0.8))
  abline(r.lin.fit.age$theta[2], r.lin.fit.age$theta[1], col="blue")
}

if(F)
{
  y.tmp <- y
  idx <- rep(NA, length(y.tmp))
  for(i in 1:length(idx))
  {
    idx[i] <- which.max(y.tmp)
    y.tmp[idx[i]] <- -Inf
  }
      
  
  
  plot(y[idx], type="l", col="blue")
  lines(o.all[idx], col="red")
}


if(F)
{
  #x <- generate2PolyFeatures(as.matrix(hdata[, 1:5]))
  x <- as.matrix(hdata[, 1:5])
  y <- as.double(hdata[, 14])
  
  r.lin.fit.poly <- reg.linear(x, y, lambda = 0.00001, maxIterations = 100000000, 
                               accuracy=0.0001, minError = 0, showLog = T)
  
  plot(r.lin.fit.poly$cost[10:length(r.lin.fit.poly$cost)], type="l", col="red", 
       main="cost function", xlab="iteration", ylab="value")
  
  z.poly <- cbind(x, rep(1, dim(x)[1]))
  o.poly <- z.poly %*% r.lin.fit.poly$theta
}





