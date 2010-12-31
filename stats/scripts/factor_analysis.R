mycorr = read.csv("../data/derived/dat_corr_spearman_normalized.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat = read.csv("../data/derived/dat_eventcounts_norm.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

### @export "scree"

eigenvectors.1st <- eigen(mycorr) # get eigenvalues

#quartz()
png(paste("../artifacts/scree_plot.png", sep=""))
plot(eigenvectors.1st$values)
dev.off()


### @export "factor analysis function"

do.factor.analysis = function(num.factors) {
	# Maximum liklihood 
	mycorr.matrix = as.matrix(mycorr)
	names(mycorr.matrix) = names(mycorr)
	rownames(mycorr.matrix) = names(mycorr)
	fit.ml = factanal(factors=num.factors, rotation="oblimin", covmat=mycorr.matrix)
	print(fit.ml, sort=TRUE)


	# Use princip axis when maximum liklihood fails to converge:

	fit.fa.1st = fa(mycorr, num.factors, fm="minres", rotate="oblimin", 
	                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat)))

	#to show the loadings sorted by absolute value
	print(fit.fa.1st, sort=TRUE)
	return(list(ml=fit.ml, fa=fit.fa.1st))
}

results = do.factor.analysis(5)
factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs")
colnames(results[["fa"]]$loadings) = factor.labels

results = do.factor.analysis(6)
factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
colnames(results[["fa"]]$loadings) = factor.labels


### @export "factor analysis results"
results[["fa"]]

### @export "factor analysis graphs"

#quartz()
png(paste("../artifacts/factor_analysis_diagram.png", sep=""))
fa.diagram(results[["fa"]])
dev.off()

#quartz()
factors.corr = results[["fa"]]$score.cor
colorRange = round(range(factors.corr) * 15) + 16
colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
png(paste("../artifacts/heatmap_factors_dend.png", sep=""))
heatmap.2(factors.corr, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, 
	labRow=factor.labels, labCol=factor.labels,
	dend = "col", Colv=T, Rowv=F,
	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
	trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
dev.off()	

#quartz()
png(paste("../artifacts/heatmap_factors_nodend.png", sep=""))
heatmap.2(factors.corr, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, 
	labRow=factor.labels, labCol=factor.labels,
	dend = "none", Colv=F, Rowv=F,
	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
	trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
dev.off()

# More confusing than helpful
## also see factor.plot
#quartz()
#plot(results[["fa"]])

# look at residuals
fit.fa.1st.residuals = results[["fa"]]$residual
#quartz()
png(paste("../artifacts/heatmap_factor_residuals.png", sep=""))
heatmap.2(fit.fa.1st.residuals, col=bluered(16), cexRow=0.5, cexCol = .8, symm = TRUE, dend = "row", trace = "none", 
	margins=c(15,15), key=FALSE, keysize=0.1)
dev.off()



