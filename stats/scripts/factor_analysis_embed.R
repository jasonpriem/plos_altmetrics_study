mycorr = read.csv("../data/derived/dat_corr_spearman_normalized.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat = read.csv("../data/derived/dat_eventcounts_norm.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

### @export "factor analysis"

num.factors=6
fit.fa.1st = fa(mycorr, num.factors, fm="minres", rotate="oblimin", 
                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat)))

factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
colnames(results[["fa"]]$loadings) = factor.labels
#to show the loadings sorted by absolute value
print(fit.fa.1st, sort=TRUE)

