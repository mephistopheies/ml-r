#http://norvig.com/ngrams/


LanguageModel.ReadUnigram <- function(k = 1, logSpace=T, file="Data/SpellingCorrection/count_1w.txt")
{
  # k - laplace smoothing
  d <- read.table(file, 
                  sep="\t", 
                  header=F,
                  col.names=c("word", "count"), 
                  fill=FALSE, 
                  strip.white=TRUE)
  n <- sum(d$count)
  probs <- (d$count + k)/(n + k*length(d$word))
  if(logSpace)
  {
    probs <- log(probs)
  }
  
  p.unknown <- k/(n + k*length(d$word))
  if(logSpace)
  {
    p.unknown <- log(p.unknown)
  }
  
  return(
      list(words = d$word,
           p.words = probs,
           isLog=logSpace, 
           laplaceFactor=k, 
           denominator=n, 
           p.unknown = p.unknown
           )
    )
}

DamerauLevenshtein.Distance <- function(s, t)
{
  s <- unlist(strsplit(s, ""))
  t <- unlist(strsplit(t, ""))
  
  if(length(s) == 0 && length(t) == 0)
  {
    return(0)
  }
  
  if(length(s) == 0 && length(t) > 0)
  {
    return(length(t))
  }
  
  if(length(s) > 0 && length(t) == 0)
  {
    return(length(s))
  }
  
  d <- matrix(rep(NA, (length(s) + 2)*(length(t) + 2)), nrow=(length(s) + 2), ncol=(length(t) + 2))
  inf <- length(s) + length(t)
  d[1, 1] <- inf
  for(i in 1:(length(s) + 1))
  {
    d[i + 1, 1] <- inf
    d[i + 1, 2] <- i - 1
  }
  for(j in 1:(length(t) + 1))
  {
    d[1, j + 1] <- inf
    d[2, j + 1] <- j - 1
  }
  letters <- union(s, t)
  counts <- rep(0, length(letters))
  names(counts) <- letters
  for(i in 2:(length(s) + 1))
  {
    db <- 0
    for(j in 2:(length(t) + 1))
    {
      i1 <- counts[[t[j - 1]]]
      j1 <- db
      
      if(s[i - 1] == t[j - 1])
      {
        d[i + 1, j + 1] <- d[i, j]
        db <- j
      }
      else
      {
        d[i + 1, j + 1] <- min(c(
          d[i, j],
          d[i + 1, j],
          d[i, j + 1]
        )) + 1
      }
      
      d[i + 1, j + 1] <- min(c(
        d[i + 1, j + 1],
        d[i1, j1] + (i - i1 - 1) + 1 + (j - j1 - 1)
      ))
    }
    
    counts[[s[i - 1]]] <- i
  }
  return(list(
      d = d[length(s) + 2, length(t) + 2],
      dMatrix = d
    ))
}

Levenshtein.DistanceWithBacktrace <- function(s, t, d.max = Inf)
{
  # backtrace: 2 - del (up), 1 - sub (up-left), 3 - ins (left)
  ba <- c("d", "i", "s")
  
  s <- unlist(strsplit(s, ""))
  t <- unlist(strsplit(t, ""))
  
  dmax <- length(s) + length(t)
  d <- matrix(rep(dmax, (length(s) + 1)*(length(t) + 1)), nrow=(length(s) + 1), ncol=(length(t) + 1))
  b <- matrix(rep(NA, (length(s) + 1)*(length(t) + 1)), nrow=(length(s) + 1), ncol=(length(t) + 1))
  
  d[1, 1] <- 0
  for(i in 2:(length(s) + 1))
  {
    for(j in 2:(length(t) + 1))
    {
      cost <- as.numeric(s[i - 1] != t[j - 1])
      
      v <- c(              
              d[i - 1, j] + 1, # del: 1              
              d[i, j - 1] + 1, # ins: 2
              d[i - 1, j - 1] + cost #sub: 3
            )
      
      d[i, j] <- min(v)      
      b[i, j] <- ba[which.min(v)]
    }
  }
    
  return(list(
      d = d[length(s) + 1, length(t) + 1],
      dMatrix = d,
      btMatrix = as.matrix(b[2:dim(b)[1], 2:dim(b)[2]])
    ))
}

Levenshtein.PlotBacktrace <- function(m)
{
  # library(plotrix)
  mn <- m
  mn[which(mn == "d")] <- -1
  mn[which(mn == "i")] <- 1
  mn[which(mn == "s")] <- 0
  class(mn) <- "numeric"
  color2D.matplot(mn, show.values=T, axes=F, xlab="", ylab="", 
                  cs1=c(0, 0.5),cs2=c(0, 0.5),cs3=c(0, 0.5))
  i <- dim(m)[1]
  j <- dim(m)[2]
  cols <- terrain.colors(dim(m)[1] + dim(m)[2])
  colIdx <- 1
  while(T)
  {
    x <- i
    y <- j
    if(m[i, j] == "d")
    {
      x <- i - 1
      arrows(j - 0.48, dim(m)[1] - i, y - 0.48, dim(m)[1] - x, lwd=5,
             col=cols[colIdx])
      colIdx <- colIdx + 1
    }
    else if(m[i, j] == "s")
    {
      x <- i - 1
      y <- j - 1
      arrows(j, dim(m)[1] - i, y, dim(m)[1] - x, lwd=5,
             col=cols[colIdx])
      colIdx <- colIdx + 1
    }
    else if(m[i, j] == "i")
    {
      y <- j - 1 
      arrows(j, dim(m)[1] - i + 0.48, y, dim(m)[1] - x + 0.48, lwd=5,
             col=cols[colIdx])
      colIdx <- colIdx + 1
    }

    if(x == 0 | y == 0)
    {
      break
    }
    i <- x
    j <- y
  }
}

Levenshtein.GetBacktrace <- function(s, t, r)
{
  s <- unlist(strsplit(s, ""))
  t <- unlist(strsplit(t, ""))
  
  if(length(s) == 1 & length(t) == 1 & s[1] != t[1])
  {
    return(list(
      symbols = paste(s[1], t[1], sep=""),
      actions = c("s")
    ))
  }
  
  d <- r$dMatrix
  b <- r$btMatrix
  i <- dim(b)[1]
  j <- dim(b)[2]
  l <- c() # letters
  a <- c() # actions
  while(T)
  {
    x <- i
    y <- j
    if(b[i, j] == "d")
    {
      x <- i - 1
      l <- append(l, s[i])
      a <- append(a, "d")
    }
    else if(b[i, j] == "i")
    {
      y <- j - 1 
      l <- append(l, t[j])
      a <- append(a, "i")      
    }
    else if(b[i, j] == "s")
    {
      x <- i - 1
      y <- j - 1
      l <- append(l, paste(s[i], t[j], sep=""))
      a <- append(a, "s")
    }
    
    
    if(x == 0 | y == 0)
    {
      break
    }
    i <- x
    j <- y
  }
  return(list(
      symbols = rev(l),
      actions = rev(a)
    ))
}


ChannelModel.Read <- function(k = 1, logSpace=T, file="Data/SpellingCorrection/spell-errors.txt")
{
  text <- readLines(file)  
  del <- list()
  ins <- list()
  sub <- list()
  for(i in 1:length(text))
  {
    words <- unlist(strsplit(text[i], ","))
    t <- words[1]
    s <- words[2:length(words)]
    
    for(j in 1:length(s))
    {
      ld <- Levenshtein.DistanceWithBacktrace(s[j], t)
      b <- Levenshtein.GetBacktrace(s[j], t, ld)
      
      for(a in 1:length(b$symbols))
      {
        if(b$actions[a] == "d")
        {
          if(is.null(del[[b$symbols[a]]]))
          {
            del[[b$symbols[a]]] <- 0
          }
          del[[b$symbols[a]]] <- del[[b$symbols[a]]] + 1
        }
        else if(b$actions[a] == "i")
        {
          if(is.null(ins[[b$symbols[a]]]))
          {
            ins[[b$symbols[a]]] <- 0
          }
          ins[[b$symbols[a]]] <- ins[[b$symbols[a]]] + 1
        }
        else if(b$actions[a] == "s")
        {
          ss <- unlist(strsplit(b$symbols[a], ""))
          if(ss[1] != ss[2])
          {
            if(is.null(sub[[b$symbols[a]]]))
            {
              sub[[b$symbols[a]]] <- 0
            }
            sub[[b$symbols[a]]] <- sub[[b$symbols[a]]] + 1
          }
        }
      }
      
      
    }
  }
  
  ins <- unlist(ins)
  p.ins.unknown <- k/(sum(ins) + k*length(ins))
  ins <- (ins + k)/(sum(ins) + k*length(ins))
  del <- unlist(del)
  p.del.unknown <- k/(sum(del) + k*length(del))
  del <- (del + k)/(sum(del) + k*length(del))
  sub <- unlist(sub)
  p.sub.unknown <- k/(sum(sub) + k*length(sub))
  sub <- (sub + k)/(sum(sub) + k*length(sub))
  
  if(logSpace)
  {
    p.ins.unknown <- log(p.ins.unknown)
    ins <- log(ins)
    p.del.unknown <- log(p.del.unknown)
    del <- log(del)
    p.sub.unknown <- log(p.sub.unknown) 
    sub <- log(sub)
  }
  
  return(list(
      ins = ins,
      p.ins.unknown = p.ins.unknown,
      del = del,
      p.del.unknown = p.del.unknown,
      sub = sub,
      p.sub.unknown = p.sub.unknown
    ))
}                                                                                                            

