
library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

library(altmetrics.analysis)

### @export "scree"

data(dat_research_norm)
dat.research.norm.transform = dat.research.norm
dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")

eigenvectors.1st <- eigen(mycor) # get eigenvalues
png("scree_plot.png")
plot(eigenvectors.1st$values)
dev.off()

### @export "factor analysis function"


### @export "factor analysis graphs"

make_graphs = function(fa.results, factor.labels) {
    #quartz()
    png(paste("factor_analysis_diagram.png", sep=""), width=500, height=500)
    fa.diagram(fa.results)
    dev.off()

    factors.cor = fa.results$score.cor
    colorRange = round(range(factors.cor) * 15) + 16
    colorChoices = bluered(32)[colorRange[1]:colorRange[2]]

    #quartz()
    png(paste("heatmap_factors_nodend.png", sep=""), width=500, height=500)
    heatmap.2(factors.cor, col=colorChoices, cexRow=1.5, cexCol = 1.5, symm = TRUE, 
    	labRow=factor.labels, labCol=factor.labels,
    	dend = "none", Colv=F, Rowv=F,
    	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
    	trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
    dev.off()

    #quartz()
    png(paste("heatmap_factors_dend.png", sep=""), width=500, height=500)
    heatmap.2(factors.cor, col=colorChoices, cexRow=1.5, cexCol = 1.5, symm = TRUE, 
    	labRow=factor.labels, labCol=factor.labels,
    	dend = "both", Colv=T, Rowv=T,
    	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
    	trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
    dev.off()	


    # More confusing than helpful
    ## also see factor.plot
    #quartz()
    #plot(results[["fa"]])

    # look at residuals
    fit.fa.1st.residuals = fa.results$residual
    
    #quartz()
    #png(paste("heatmap_factor_residuals.png", sep=""), width=500, height=500)
    #heatmap.2(fit.fa.1st.residuals, col=bluered(16), cexRow=0.5, cexCol = .5, symm = TRUE, dend = "row", trace = "none", 
    #	margins=c(10,10), key=FALSE, keysize=0.1)
    #dev.off()

}

heatmap_of_articles = function(dat, cols, filename, year=2008) {
    # Now a heatmap with a subsample of articles and the variables
    set.seed(43)
    inYear = which(dat$year == year)
    dat.tosample = dat[inYear,cols]
    dat.tosample = dat.tosample[complete.cases(dat.tosample),]
    dat.subsample = as.matrix(dat.tosample[sample(1:dim(dat.tosample)[1], 1000, TRUE), ])
    m=200
    png(filename, width=500, height=500)
    heatmap.2(t(dat.subsample), col=bluered(m*2)[1:(m*2-1)], 
     	cexRow=1, cexCol=.1, dend = "both", trace="none", 
     	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
     	margins=c(1,10), key=FALSE, keysize=0.1, scale="row", symbreaks=T)
    title(paste("\narticles vs altmetrics", year))
    dev.off()
}

factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
fa.results = do_factor_analysis(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)
make_graphs(fa.results, factor.labels)

dat.with.factor.scores = get_factor_scores(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)
heatmap_of_articles(dat.with.factor.scores, factor.labels, filename = "heatmap_articles_factor_dend.png")

y = dat.with.factor.scores[complete.cases(dat.with.factor.scores[,factor.labels]),]
x = y[,factor.labels]

# Determine number of clusters
wss <- (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(x, centers=i)$withinss)
quartz()
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  
NUMBER.CLUSTERS = 8
cl <- kmeans(x, NUMBER.CLUSTERS)
round(cl$centers, 1)
quartz()
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex=2)

# append cluster assignment
z <- data.frame(y, cl$cluster)


# vary parameters for most readable graph
library(cluster) 
clusplot(x[1:200], cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(x, cl$cluster, shade=F, labels=4, lines=0, color=F, lty=4, col.txt=1:8)
clusplot(x, cl$cluster, shade=F, labels=4, lines=0, color=F, col.txt=1:8, plotchar=T, col = cl$cluster)

names(x)[5] = "wikipedia"
#shuttle.tr <- tree(citations ~ ., x[1:2000,])
shuttle.tr <- tree(citations ~ downloads + facebook + comments + wikipedia + bookmarks, x[1:2000,])
#shuttle.tr <- tree(citations ~  facebook + comments + wikipedia + bookmarks, x[1:2000,])

shuttle.tr
shuttle1 <- x[2002:2005, ] # 3 missing cases
predict(shuttle.tr, shuttle1)
quartz()
plot(shuttle.tr)
text(shuttle.tr)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(x, cl$cluster)

