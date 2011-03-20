calc.correlations <- function
### Calculates correlations of matrix rows and columns, 
### Code inspired by hetcore.data.frame from library(polycor)
(dat, 
    missing.method=c("complete.obs", "pairwise.complete.obs"), 
    correlation.method=c("pearson", "spearman")) 
{
  if (missing.method == "complete.obs") dat <- na.omit(dat)
  p <- length(dat)
  if (p < 2) stop("fewer than 2 variables.")
  mycorr <- matrix(1, p, p)
  for (i in 2:p) {
    for (j in 1:(i-1)){
        x <- dat[[i]]
        y <- dat[[j]]
        r <- cor(x, y, method=correlation.method, use=missing.method)
        mycorr[i, j] <- mycorr[j, i] <- r
     }
  }
  rownames(mycorr) <- colnames(mycorr) <- names(dat)
  
  mycorr
  ### Return the correlation matrix
} 




