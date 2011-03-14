calc.correlations <- function
### Calculates correlations of matrix rows and columns, 
### Code inspired by hetcore.data.frame from library(polycor)
(data, 
    missing.method=c("complete.obs", "pairwise.complete.obs"), 
    correlation.method=c("pearson", "spearman")) 
{
  if (missing.method == "complete.obs") data <- na.omit(data)
  p <- length(data)
  if (p < 2) stop("fewer than 2 variables.")
  mycorr <- matrix(1, p, p)
  for (i in 2:p) {
    for (j in 1:(i-1)){
        x <- data[[i]]
        y <- data[[j]]
        r <- cor(x, y, method=correlation.method, use=missing.method)
        mycorr[i, j] <- mycorr[j, i] <- r
     }
  }
  rownames(mycorr) <- colnames(mycorr) <- names(data)
  
  mycorr
  ### Return the correlation matrix
} 




