rm(list=ls())
library(plotrix)

source("SpellingCorrection/NoisyChannelFunctions.R")

#s <- "ca"
#t <- "abc"
#r <- Levenshtein.DistanceWithBacktrace(s, t)
#Levenshtein.PlotBacktrace(r$btMatrix)
#b <- Levenshtein.GetBacktrace(s, t, r)
#print(b)


#lm <- LanguageModel.ReadUnigram()
#save(lm, file="./Data/SpellingCorrection/lm.Rda")
load("./Data/SpellingCorrection/lm.Rda")

#cm <- ChannelModel.Read()
#save(cm, file="./Data/SpellingCorrection/cm.Rda")
load("./Data/SpellingCorrection/cm.Rda")


w <- "rxxn"

d.max <- 2
w.cand <- list()
if(F)
{
  for(i in 1:length(lm$words))
  {
    t <- as.character(lm$words[i])
    if(abs(length(w) - length(t)) > d.max)
    {
      next
    }
    
    ld <- Levenshtein.DistanceWithBacktrace(w, t)
    if(ld$d == 0)
    {
      #return(w)
    }
    
    if(ld$d <= d.max)
    {
      b <- Levenshtein.GetBacktrace(w, t, ld)
      p.log <- lm$p.words[i]
      for(j in 1:length(b$symbols))
      {
        if(b$actions[j] == "d")
        {
          if(sum(names(cm$del) == b$symbols[j]) > 0)
          {
            p.log <- p.log + cm$del[b$symbols[j]]
          }
          else
          {
            p.log <- p.log + cm$p.del.unknown
          }
        }
        else if(b$actions[j] == "i")
        {
          if(sum(names(cm$ins) == b$symbols[j]) > 0)
          {
            p.log <- p.log + cm$ins[b$symbols[j]]
          }
          else
          {
            p.log <- p.log + cm$p.ins.unknown
          }
        }
        else if(b$actions[j] == "s")
        {
          if(sum(names(cm$sub) == b$symbols[j]) > 0)
          {
            p.log <- p.log + cm$sub[b$symbols[j]]
          }
          else
          {
            p.log <- p.log + cm$p.sub.unknown
          }
        }
      }
      w.cand[[t]] <- p.log
    }
    
    print(i)
  }Daaaa
}




