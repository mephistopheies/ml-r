PreprocessSentence <- function(s)
{
  # Cut and make some preprocessing with input sentence
  words <- strsplit(gsub(pattern="[[:digit:]]+", replacement="1", x=tolower(s)), '[[:punct:][:blank:]]+')
  return(words)
}


LoadData <- function(fileName = "./Data/Spam/SMSSpamCollection") 
{
  # Read data from text file and makes simple preprocessing: 
  #   to lower case -> replace all digit strings with 1 -> split with punctuation and blank characters
  con <- file(fileName,"rt")
  lines <- readLines(con)
  close(con)
  df <- data.frame(lab = rep(NA, length(lines)), data = rep(NA, length(lines)))
  for(i in 1:length(lines))
  {
    tmp <- unlist(strsplit(lines[i], '\t', fixed = T))
    df$lab[i] <- tmp[1]
    df$data[i] <- PreprocessSentence(tmp[2])
  }
  
  return(df)
}


CreateDataSet <- function(dataSet, proportions = c(0.6, 0.2, 0.2))
{
  # Creates a list with indices of train, validation and test sets  
  proportions <- proportions/sum(proportions)
  hamIdx <- which(df$lab == "ham")    
  nham <- length(hamIdx)  
  spamIdx <- which(df$lab == "spam")
  nspam <- length(spamIdx)
  hamTrainIdx <- sample(hamIdx, floor(proportions[1]*nham))
  hamIdx <- setdiff(hamIdx, hamTrainIdx)
  spamTrainIdx <- sample(spamIdx, floor(proportions[1]*nspam))
  spamIdx <- setdiff(spamIdx, spamTrainIdx)
  hamValidationIdx <- sample(hamIdx, floor(proportions[2]*nham))
  hamIdx <- setdiff(hamIdx, hamValidationIdx)
  spamValidationIdx <- sample(spamIdx, floor(proportions[2]*nspam))
  spamIdx <- setdiff(spamIdx, spamValidationIdx)  
  ds <- list(
    train = sample(union(hamTrainIdx, spamTrainIdx)), 
    validation = sample(union(hamValidationIdx, spamValidationIdx)), 
    test = sample(union(hamIdx, spamIdx))
    )  
  return(ds)
}


CreateModel <- function(data, laplaceFactor = 0)
{
  # creates naive bayes spam classifier based on data
  m <- list(laplaceFactor = laplaceFactor)
  m[["total"]] <- length(data$lab)
  m[["ham"]] <- list()  
  m[["spam"]] <- list()
  m[["hamLabelCount"]] <- sum(data$lab == "ham")
  m[["spamLabelCount"]] <- sum(data$lab == "spam")
  m[["hamWordCount"]] <- 0
  m[["spamWordCount"]] <- 0
  uniqueWordSet <- c()
  for(i in 1:length(data$lab))
  {
    sentence <- unlist(data$data[i])
    uniqueWordSet <- union(uniqueWordSet, sentence)
    for(j in 1:length(sentence))
    {
      if(data$lab[i] == "ham")
      {
        if(is.null(m$ham[[sentence[j]]]))
        {
          m$ham[[sentence[j]]] <- 1
        }
        else
        {
          m$ham[[sentence[j]]] <- m$ham[[sentence[j]]] + 1
        }
        m[["hamWordCount"]] <- m[["hamWordCount"]] + 1
      }
      else if(data$lab[i] == "spam")
      {
        if(is.null(m$spam[[sentence[j]]]))
        {
          m$spam[[sentence[j]]] <- 1
        }
        else
        {
          m$spam[[sentence[j]]] <- m$spam[[sentence[j]]] + 1
        }
        m[["spamWordCount"]] <- m[["spamWordCount"]] + 1
      }
    }
  }
  m[["uniqueWordCount"]] <- length(uniqueWordSet)
  return(m) 
}


ClassifySentense <- function(s, model, preprocess = T)
{
  # calculate class of the input sentence based on the model
  GetCount <- function(w, ls)
  {
    if(is.null(ls[[w]]))
    {
      return(0)
    }
    return(ls[[w]])
  }
  words <- unlist(s)
  if(preprocess)
  {
    words <- unlist(PreprocessSentence(s))
  }
  ham <- log(model$hamWordCount)
  spam <- log(model$spamWordCount)
  for(i in 1:length(words))
  {
    ham <- ham + log((GetCount(words[i], model$ham) + model$laplaceFactor)
                     /(model$hamWordCount + model$laplaceFactor*model$uniqueWordCount))
    spam <- spam + log((GetCount(words[i], model$spam) + model$laplaceFactor)
                       /(model$spamWordCount + model$laplaceFactor*model$uniqueWordCount))
  }
  if(ham >= spam)
  {
    return("ham")
  }
  return("spam")
}


TestModel <- function(data, model)
{
  # calculate percentage of errors
  errors <- 0
  for(i in 1:length(data$lab))
  {
    predictedLabel <- ClassifySentense(data$data[i], model, preprocess = F)
    if(predictedLabel != data$lab[i])
    {
      errors <- errors + 1
    }
  }
  return(errors/length(data$lab))
}


CrossValidation <- function(trainData, validationData, laplaceFactorValues, showLog = F)
{
  cvErrors <- rep(NA, length(laplaceFactorValues))
  for(i in 1:length(laplaceFactorValues))
  {
    model <- CreateModel(trainData, laplaceFactorValues[i])
    cvErrors[i] <- TestModel(validationData, model)
    if(showLog)
    {
      print(paste(laplaceFactorValues[i], ": error is ", cvErrors[i], sep=""))
    }
  }
  return(cvErrors)
}