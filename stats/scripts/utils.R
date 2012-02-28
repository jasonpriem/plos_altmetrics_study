#colourblind friendly palettes from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)
library(ggplot2)
cbgRaw = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbgFillPalette <- scale_fill_manual(values=cbgRaw)
cbgColourPalette <- scale_colour_manual(values=cbgRaw)
cbgColorPalette = cbgColourPalette

# String concat operator from https://stat.ethz.ch/pipermail/r-help/2005-February/066709.html
"&" <- function(...) UseMethod("&")
"&.default" <- .Primitive("&")
"&.character" <- function(...) paste(...,sep="")
# "abc" & "def" & "ghi"


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


# PLoS standards from http://www.plosone.org/static/figureGuidelines.action#quickref
MAX.FIGURE.WIDTH = 6.83
MAX.FIGURE.HEIGHT = 8

setup.eps.figure = function
### Set up a graphics object for plotting a journal-ready .eps figure
### call this, then do the plot, then call dev.off()
(
    filename,   ##<< filename for resulting eps file.  Will get .eps appended
    width=6.83, ##<< figure width, in inches
    height=8    ##<< figure height, in inches
) {
  filename.with.extension = paste(filename, ".eps", sep="")
  postscript(file=filename.with.extension,
              paper="special",
              width=width,
              height=height,
              family="sans",              
              onefile=FALSE,
              pagecentre=FALSE,
              horizontal=FALSE)
   return()
   
   ##examples<<
   setup.eps.figure("figure1", 6, 8)
   plot(c(1, 2, 3), c(2, 4, 6))
   close.eps.figure("figure1")
}


close.eps.figure = function
### Closes a graphics object and embeds fonts in he associated .eps figure
### To be used with setup.eps.figure
(
    filename    ##<< filename of eps file.  Will get .eps appended
) {
    filename.with.extension = paste(filename, ".eps", sep="")
    dev.off()
    embedFonts(filename.with.extension, outfile=filename.with.extension)    
    return()
    
   ##examples<<
   setup.eps.figure("figure1", 6, 8)
   plot(c(1, 2, 3), c(2, 4, 6))
   close.eps.figure("figure1")
}

