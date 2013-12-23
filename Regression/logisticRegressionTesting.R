rm(list = ls())
set.seed(1488)
source("Regression/regressionFunctions.R")
data <- read.csv("Data/Regression/wdbc.data.txt", sep=",", header=F)
data$V2 <- data$V2 == "M"
data$V1 <- NULL
names(data)[1] <- "IsMal"

x <- as.matrix(data[, 2:dim(data)[2]])
y <- matrix(as.numeric(data[, 1]), ncol=1, nrow=dim(data)[1])

if(F)
{
  par(mfrow=c(2, 2))
  
  plot(data$V3, data$V5,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V7, data$V6,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V19, data$V32,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  plot(data$V15, data$V20,
       pch=19, col=c(rgb(0, 0, 1, alpha=0.4), rgb(1, 0, 0, alpha=0.4))[data$IsMal + 1])
  
  
  par(mfrow=c(1, 1))
}

if(T)
{
  
  #r.log.fit.all <- reg.logistic(x, y, lambda = 0.0001, maxIterations = 100000, 
  #                              accuracy=0, minError = -Inf, showLog = T)
  load(file="Data/Regression/r.log.fit.all.Rda")
  
  #plot(r.log.fit.all$cost[100:length(r.log.fit.all$cost)], col="red", type="l",
  #     xlab="iteration", ylab="cost", main="logistic regression training")
  
  p <- sigmoid(cbind(x, rep(1, dim(x)[1])) %*% r.log.fit.all$theta)
  o <- as.numeric(p > 0.5)
  y <- as.numeric(y)
  
  q <- binaryClassifierEvaluation(y, o)
  
}


if(F)
{
  #plot recall and others
  tp <- 1:50
  fn <- 1:50
  r <- matrix(rep(NA, length(tp)*length(fn)), ncol=length(tp), nrow=length(fn))
  for(i in tp)
  {
    for(j in fn)
    {
      r[i, j] <- tp[i]/(tp[i] + fn[j])
    }
  }
  heatmap(r, Rowv=NA, Colv=NA, revC=F, scale = "none")
}

if(F)
{
  #f1 score
  p <- seq(0.01, 1, length.out=50)
  r <- seq(0.01, 1, length.out=50)
  f1 <- matrix(rep(NA, length(p)*length(r)), ncol=length(p), nrow=length(r))
  for(i in 1:length(p))
  {
    for(j in 1:length(r))
    {
      f1[i, j] <- 2*p[i]*r[j]/(p[i] + r[j])
    }
  }
  heatmap(f1, Rowv=NA, Colv=NA, revC=F, scale = "none")
}

if(F)
{
  #Matthews correlation coefficient
  
  n <- 100
  tp <- 1:50
  tn <- 1:50
  mcc <- matrix(rep(NA, length(tp)*length(tn)), ncol=length(tp), nrow=length(tn))
  for(i in tp)
  {
    for(j in tn)
    {
      f <- n - tp[i] - tn[j]
      e <- round(f / 4)
      fp <- e
      fn <- 3*e
      mcc[i, j] <- (tp[i]*tn[j] - fp*fn)/sqrt((tp[i] + fp)*(tp[i] + fn)*(tn[j] + fp)*(tn[j] + fn))
    }
  }
  heatmap(mcc, Rowv=NA, Colv=NA, revC=F, scale = "none", main="MCC")
  
}

if(F)
{
  #accuracy
  
  n <- 100
  tp <- 1:50
  tn <- 1:50
  acc <- matrix(rep(NA, length(tp)*length(tn)), ncol=length(tp), nrow=length(tn))
  for(i in tp)
  {
    for(j in tn)
    {
      f <- n - tp[i] - tn[j]
      e <- round(f / 4)
      fp <- e
      fn <- 3*e
      acc[i, j] <- (tp[i] + tn[j])/(tp[i] + tn[j] + fn + fp)
    }
  }
  heatmap(acc, Rowv=NA, Colv=NA, revC=F, scale = "none", main="ACC")
  
}