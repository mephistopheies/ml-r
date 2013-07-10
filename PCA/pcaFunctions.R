library(RUnit)

VProj <- function(a, b)
{
  # Ortogonal projection of the vector a on the vector b
  # 
  # Args: 
  #   a: vector which will be projected
  #   b: vector on which vector a will be projected
  #
  # Returns:
  #   projection of vector a on vector b
  c <- (a %*% b)/(b %*% b) * b
  return(c)
}

ModifiedGramSchmidtQRDecomposition <- function(m)
{
  # QR decomposition using Gram-Schmidt process
  #
  # Args:
  #   v: matrix
  # 
  # Returns:
  #   returns list of two matrices 
  #    - result$q: Orthogonal matrix (columns are vectors) 
  #    - result$r: upper triangle matrix
  v <- m
  for(i in 1:dim(v)[2])
  {
    v[, i] <- v[, i] / sqrt(v[, i] %*% v[, i])    
    if(i + 1 <= dim(v)[2])
    {
      for(j in (i + 1):dim(v)[2])
      {      
        v[, j] <- v[, j] - VProj(v[, j], v[, i])      
      }
    }
  }
  r <- matrix(0, nrow=dim(v)[2], ncol=dim(v)[2])
  for(i in 1:dim(v)[2])
  {
    for(j in 1:dim(v)[2])
    {
      if(i >= j)
      {
        r[i, j] <- v[, j] %*% m[, i]
      }
    }
  }
  r <- t(r)
  return(list(q = v, r = r))
}

Test.ModifiedGramSchmidtQRDecomposition <- function()
{
  m <- matrix(c(12, -51, 4, 6, 167, -68, -4, 24, -41), ncol=3, nrow=3, byrow=T)
  qr <- ModifiedGramSchmidtQRDecomposition(m)  
  tst <- sum(abs(m - qr$q %*% qr$r))
  checkEqualsNumeric(0, tst, "qr decomposition is wrong or weak precision", 1e-6)
}

IterativeQRMethod <- function(m, precision = 0.1, maxIterations = 100, showLog = F)
{
  # Itarative QR-method described in 
  # http://mathreview.uwaterloo.ca/archive/voli/1/panju.pdf
  # it extracts eigenvectors and eigenvalues from input matrix
  #
  # Args:
  #   m: input matrix (stable only with symmetric matrix)
  #   prcision: stop factor, precision of calculations
  #   maxIterations: stop factor, maximum number of iterations
  #   showLog: if should log shoild be shown
  #
  # Returns:
  #   list of two elements
  #    - result$eigenVectors: matrix with eigenvectors (columns)
  #    - result$eigenValues: vector of corresponding eigenvalues 
  res <- list(
    eigenVectors = diag(dim(m)[2]), 
    eigenValues = rep(NA, dim(m)[2])
    )  
  mTmp <- m
  mTmp.last <- m
  for(k in 1:maxIterations)
  {
    qr <- ModifiedGramSchmidtQRDecomposition(mTmp)
    mTmp <-qr$r %*% qr$q
    res$eigenVectors <- res$eigenVectors %*% qr$q
    delta <- abs(sum(mTmp) - sum(mTmp.last))
    mTmp.last <- mTmp
    if(delta < precision)
    {
      break
    }
    if(showLog)
    {
      print(paste("Iteration ", k, ": ", delta, sep=""))
    }
  }
  for(i in 1:dim(m)[2])
  {
    res$eigenValues[i] <- mTmp[i, i]
  }
  return(res)
}

Test.IterativeQRMethod <- function()
{
  m <- matrix(c(1, 2, 4, 2, 9, 8, 4, 8, 2), ncol=3, nrow=3, byrow=T)
  res <- IterativeQRMethod(m, precision = 1e-6, maxIterations = 100, showLog = F)  
  checkEqualsNumeric(c(15.2964, 4.3487, 1.0523), abs(res$eigenValues), "iterative QR-method is wrong or weak precision", 1e-4)
}

ProjectData <- function(obs, base)
{
  # Project data observations (rows) into base (columns)
  #
  # Args:
  #   obs: matrix with observations (rows)
  #   base: matrix with basis (columns are base vectors)
  #
  # Returns:
  #   matrix with projected data
  return(as.matrix(obs) %*% base)
}



