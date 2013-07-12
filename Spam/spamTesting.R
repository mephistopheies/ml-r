rm(list = ls())
source("./Spam/spam.R")
set.seed(14880)
fileName <- "./Data/Spam/SMSSpamCollection"
#df <- LoadData()
#save(df, file="./Data/Spam/spamDF.Rda")
load("./Data/Spam/spamDF.Rda")
ds <- CreateDataSet(df, proportions = c(0.8, 0.2, 0.2))
laplaceFactorValues <- 1:10
cvErrors <- CrossValidation(df[ds$train, ], 
                            df[ds$validation, ], 
                            0:10, showLog = T)
bestLaplaceFactor <- laplaceFactorValues[which(cvErrors == min(cvErrors))]
model <- CreateModel(data=df[ds$train, ], laplaceFactor=bestLaplaceFactor)
testResult <- TestModel(df[ds$test, ], model)
plot(cvErrors, 
     type="l", col="blue", 
     xlab="Laplace Factor", ylab="Error Value", 
     ylim=c(0, max(cvErrors)))
title("Cross validation and test error value")
abline(h=testResult, col="red")
legend(bestLaplaceFactor, 
       max(cvErrors), 
       c("cross validation values", "test value level"), 
       cex=0.8, col=c("blue", "red"), lty=1)