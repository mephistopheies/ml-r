rm(list = ls())
set.seed(1488)
source("KMeans/helpFunctions.R")
source("KMeans/kmeansMahalanobisFunctions.R")

prms <- matrix(c(3, 2, 5, 2, 2000, runif(1, 0, 360)), 
               nrow=1, ncol=6, byrow=T)
data <- generateSet(prms)[, 1:2]

plot(data[, 1], data[, 2], pch=19, asp=1, 
     col=rgb(0, 0.5, 1, 0.2),
     xlab="x", ylab="y")

sigma <- cov(data)
m.x <- mean(data[, 1])
m.y <- mean(data[, 2])

points(m.x, m.y, pch=20, col="yellow")

q <- seq(0.1, 0.95, length.out=10)
palette <- cm.colors(length(q))
for(i in 1:length(q))
{
  p <- GetEllipsePoints(m.x, m.y, sigma, q = q[i])
  points(p[1, ], p[2, ], type="l", col=palette[i])
}



e <- eigen(sigma)
v <- (e$vectors %*% diag(sqrt(abs(e$values))))
arrows(c(m.x, m.x), c(m.y, m.y), 
       c(v[1, 1] + m.x, v[1, 2] + m.x), c(v[2, 1] + m.y, v[2, 2] + m.y), 
       length = 0.1, angle = 25, col="yellow")



