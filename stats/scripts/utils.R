#colourblind friendly palettes from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)
cbgFillPalette <- scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
cbgColourPalette <- scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

write.table.gzip <- function
## Simple function to write out a dataframe and then gzip it
(data, ##< data frame to save
basedir,  ##< directory
filename   ##< filename
) {
    write.table(data, file=file.path(basedir, filename), row.names=FALSE, sep="\t", col.names=names(data), na="NA")
    system(sprintf("gzip -f %s", file.path(basedir, filename)))
    ##<<note To see the first few lines of the file when it is zipped
    ## run this from a command prompt or inside an R call to system():
    ##   cat ../data/raw/event_counts_wos_raw.txt.gz | zcat | head -5
}

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
