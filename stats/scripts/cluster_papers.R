(flip = t(dat.indep.stats.norm)
#flip.df = as.data.frame(flip[,sample(1:dim(flip)[2], 100, T)])
flip.df = as.data.frame(flip[,20000:20050])

myhetcorr = hetcor.modified(flip.df, 
							use="pairwise.complete.obs", 
							std.err=FALSE, 
							pd=FALSE, 
							type="spearman")
mycor.unadjusted = myhetcorr$correlations
#write.table(mycor.unadjusted,"/Users/hpiwowar/statslink/mycor.unadjusted.txt",append=F,quote=F,sep="\t",row.names=T)

# Are some correlations NA?
which(is.na(mycor.unadjusted))
#mycor.unadjusted[which(is.na(mycor.unadjusted))] = 1

# Now fix the correlation matrix if it is not positive-definite
mycor = adjust.to.positive.definite(mycor.unadjusted)

#display
library(gplots)
colorRange = round(range(mycor) * 15) + 16
colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
heatmap.2(mycor, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(10,10), key=FALSE, keysize=0.1)

showpanel <- function(col) {
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )
}
quartz()
showpanel(colorChoices)
showpanel(bluered(32))

#pdf("heatmap.pdf", height=10, width=10)
#heatmap.2(mycor, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(10,10), key=FALSE, keysize=0.1)
#dev.off

############## FIRST ORDER ANALYSIS

##############  Determine number of First-Order factors

# Determine Number of Factors to Extract
library(nFactors)
eigenvectors.1st <- eigen(mycor) # get eigenvalues
# this line takes a long time
aparallel.1st <- parallel(subject=nrow(dat.indep.stats.norm), var=ncol(dat.indep.stats.norm), rep=100, cent=.05)
scree.results.1st <- nScree(eigenvectors.1st$values, aparallel.1st$eigen$qevpea)
summary(scree.results.1st)
plotnScree(scree.results.1st) 

# Pull out the "Optimal Coordinate"
# defined in nScree help page as 
# The optimal coordinates (OC) corresponds to an extrapolation of the preceding eigenvalue by a regression line between the eignvalue coordinates and the last eigenvalue coordinate
#number.factors.1st = scree.results.1st$Components$noc
number.factors.1st = 5


##############  Do First-Order Factor Analysis

# Maximum liklihood doesn't converge because too 
fit.ml = factanal(dat.indep.stats.norm, number.factors.1st, rotation="promax", covmat=mycor)
print(fit.ml, sort=TRUE)


# Use princip axis when maximum liklihood fails to converge:
library(psych)
fit.fa.1st = fa(mycor, number.factors.1st, fm="minres", rotate="promax", 
                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat.indep.stats.norm)))

#to show the loadings sorted by absolute value
print(fit.fa.1st, sort=TRUE)

