rotateData <- function(m, cx, cy, degree)
{  
  for(i in 1:dim(m)[1])
  {
    x <- m[i, 1]
    y <- m[i, 2]
    r <- sqrt((x - cx)^2 + (y - cy)^2)
    a <- atan2(y - cy, x - cx) + degree*pi/180
    dx <- r*cos(a)
    dy <- r*sin(a)
    
    m[i, 1] <- cx + dx
    m[i, 2] <- cy + dy 
  }
  return(m)
}

generateSet <- function(prms)
{
  ##mean x, mean y, sd x, sd y, count
  #prms <- matrix(c(0, 0, 12, 5, 2000, 0,
  #                 35, 0, 2, 10, 1000, 45,
  #                 0, -20, 5, 5, 1000, 0,
  #                 -25, -18, 2, 10, 1000, 45), 
  #               nrow=4, ncol=6, byrow=T)
  #data <- generateSet(prms)
  #plot(data[, 1], data[, 2], pch=19, asp=1, 
  #     col=rainbow(length(unique(data[, 3])), alpha=0.2)[unclass(as.factor(data[, 3]))],
  #     xlab="x", ylab="y", 
  #     main="artificial data")
  
  data <- matrix(rep(NA, 3*sum(prms[, 5])), ncol=3, nrow=sum(prms[, 5]))
  
  bias <- 0
  k <- dim(prms)[1]
  for(i in 1:k)
  {  
    x <- rnorm(prms[i, 5], prms[i, 1], prms[i, 3])
    y <- rnorm(prms[i, 5], prms[i, 2], prms[i, 4])
    
    data[(bias + 1):(bias + prms[i, 5]), 1] <- x
    data[(bias + 1):(bias + prms[i, 5]), 2] <- y
    data[(bias + 1):(bias + prms[i, 5]), 3] <- i
    
    data[(bias + 1):(bias + prms[i, 5]), 1:2] <- 
      rotateData(data[(bias + 1):(bias + prms[i, 5]), 1:2], prms[i, 1], prms[i, 2], prms[i, 6])
    
    bias <- bias + prms[i, 5]
  }
  return(data)
}

GetEllipsePoints <- function(m.x, m.y, sigma, q = 0.75, n = 100)
{
  k <- qchisq(q, 2)
  sigma <- k * sigma
  e <- eigen(sigma)
  angles <- seq(0, 2*pi, length.out=n)
  cir1.points <- rbind(cos(angles), sin(angles))
  ellipse.centered <- (e$vectors %*% diag(sqrt(abs(e$values)))) %*% cir1.points
  ellipse.biased <- ellipse.centered + c(m.x, m.y)
  return(ellipse.biased)
}

Euclidian.distance <- function(x, y)
{
  return(sqrt(sum((x - y)^2)))
}



