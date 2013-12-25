rm(list=ls())



pnorm <- function(v, p)
{
  return(sum(abs(v)^p)^(1/p))
}




plot(c(), xlim=c(-2, 2), ylim=c(-2, 2), axes=F, xlab="", ylab="", asp=1)
arrows(-2, 0, 2, 0, angle=20)
arrows(0, -2, 0, 2, angle=20)
box()



p.vals <- 1:5
cols <- rainbow(length(p.vals))

n <- 1000
a <- 0.01
dx <- seq(-2, 2, length.out=n)
dy <- seq(-2, 2, length.out=n)

for(k in 1:length(p.vals))
{
  p <- p.vals[k]
  
  dots <- list(x=c(), y=c())
  for(i in 1:n)
  {
    x <- dx[i]
    for(j in 1:n)
    {
      y <- dy[j]
      d <- pnorm(c(x, y), p)
      if(abs(1 - d) < a)
      {
        dots$x <- append(dots$x, x)
        dots$y <- append(dots$y, y)
      }
    }
  }
  
  points(dots$x, dots$y, pch=".", col=cols[k])
}











