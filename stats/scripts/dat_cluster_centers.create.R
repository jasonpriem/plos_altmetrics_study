### Necessary to avoid problems with fancy quotes in p-value reporting!
options(useFancyQuotes = FALSE)

library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

library(altmetrics.analysis)




### @export "factor analysis graphs"

plot_factor_analysis = function(fa.results, factor.labels) {
    #quartz()
    if (length(factor.labels) > 1) {
        colnames(fa.results$loadings) = factor.labels
    }
    
    png(paste("img/factor_analysis_diagram.png", sep=""), width=500, height=500)
    fa.diagram(fa.results)
    dev.off()
}

plot_factor_heatmap = function(mycor, factor.labels, filename, dend="none") {

  colorChoices = rgb((0:512)/512, green=0, blue=0)

    #quartz()
    if (dend=="both") {
        Colv=T
        Rowv=T
    } else {
        Colv=F
        Rowv=F
    }
    png(paste(filename, sep=""), width=500, height=500)
    heatmap.2(mycor, col=colorChoices, cexRow=1.5, cexCol = 1.5, symm = TRUE, 
    	labRow=factor.labels, labCol=factor.labels,
    	dend = dend, Colv=Colv, Rowv=Rowv,
    	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
    	trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
    dev.off()
}

old_function = function() {
    # More confusing than helpful
    ## also see factor.plot
    #quartz()
    #plot(results[["fa"]])

    # look at residuals
    fit.fa.1st.residuals = fa.results$residual
    
    #quartz()
    #png(paste("img/heatmap_factor_residuals.png", sep=""), width=500, height=500)
    #heatmap.2(fit.fa.1st.residuals, col=bluered(16), cexRow=0.5, cexCol = .5, symm = TRUE, dend = "row", trace = "none", 
    #	margins=c(10,10), key=FALSE, keysize=0.1)
    #dev.off()

}




### @export "factor score graphs"

heatmap_of_articles_factors = function(dat, cols, filename, year=2008) {
    # Now a heatmap with a subsample of articles and the variables
    set.seed(43)
    inYear = which(as.numeric(dat$year) == year)
    dat.tosample = dat[,cols]
    
    ## Note that there aren't many conplete cases!
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

get_complete_cases = function(dat, columns){
    dat.complete = dat[complete.cases(dat[,columns]),]
    return(dat.complete ) 
}

do_scree_wss_plot = function(dat, add=F){
    wss <- (nrow(dat)-1) * sum(apply(dat,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(dat, centers=i, iter.max=1000)$withinss)
    if (add) {
        lines(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    } else {
        plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    }
}

scree_plot_for_number_clusters = function(dat){
    # Determine number of clusters
    png("img/scree_article_clusters.png", width=500, height=500)  
    #quartz()
    do_scree_wss_plot(dat, F)
    for (i in 1:100){
        do_scree_wss_plot(dat, T)        
    }  
    dev.off()
}

cluster_assignments = function(dat, number.clusters){
    fit <- kmeans(dat, number.clusters, iter.max=1000)
    round(fit$centers, 1)
    png("img/article_factor_cluster_pairs.png", width=500, height=500)        
    plot(dat, col = fit$cluster)
    points(fit$centers, col = 1:2, pch = 8, cex=2)
    dev.off()
    return(fit)
}

plot_cluster_centers = function(cluster_fit, prettyColumnNames){
  round(cluster_fit$centers, 1)
  m=200
  png("img/article_cluster_centers.png", width=500, height=500) 
  
  #quartz()
  #cluster_labels = paste(colnames(t(cluster_fit$centers)), " (", table(cluster_fit$cluster), ")", sep="")       
  #cluster_labels = paste("flavour ", colnames(t(cluster_fit$centers)), " (", round(100*cluster_fit$size/sum(cluster_fit$size), 0), "%)", sep="")       
  cluster_labels = paste("flavour ", LETTERS[1:length(cluster_fit$size)], " (", round(100*cluster_fit$size/sum(cluster_fit$size), 0), "%)", sep="")
  metric_labels = prettyColumnNames$pretty[match(colnames(cluster_fit$centers),prettyColumnNames$col)]
  #colorRange = round(range(cluster_fit$centers) * 15) + 16
  #colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
  colorChoices = rgb((0:64)/64, green=0, blue=0)

  heatmap.2(t(min(cluster_fit$centers) + cluster_fit$centers[order(cluster_fit$size),]), col=colorChoices, 
     	cexRow=2, cexCol=2, dend = "none", trace="none", 
     	labCol = cluster_labels[order(cluster_fit$size)],
     	labRow = metric_labels,
     	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
     	margins=c(1,20), key=FALSE, keysize=0.1, scale="none", symbreaks=T)
  title(paste("n=", length(cluster_fit$cluster), sep=""))
  dev.off()
}

### @export "scree"

#data(dat_research_norm)
#dat.research.norm.transform = dat.research.norm
#dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
#mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")

#eigenvectors.1st <- eigen(mycor) # get eigenvalues
#png("img/scree_plot.png")
#plot(eigenvectors.1st$values)
#dev.off()

#factor.labels = c("citations", "facebookLike", "downloads", "comments", "bookmarks", "facebookClick")
#fa.results = do_factor_analysis(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)
#make_graphs(fa.results, factor.labels)

do_cluster_viz = function(dat) {
    #dat = subset(dat.research.norm.transform, (journal=="pone") & (year=="2009")))
    #dat = subset(dat.research.norm.transform, (journal=="pgen"))
    #dat = subset(dat.research.norm.transform, (journal=="pbio"))
    #dat = subset(dat.research, ((journal=="pbio") & (year=="2009")))
    #dat = subset(dat.research, ((journal=="pmed") & (year=="2009")))
    #dat = subset(dat.research, ((journal=="pone") & (year=="2010")))
    #dat = subset(dat.research.norm.transform, ((journal=="pone") & (year=="2009")))
    
    dat = subset(dat.research.norm.transform, (journal=="pone"))
    dat.for.cluster.unscaled = dat

    clusterColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor","wikipediaCites")
    dat.for.cluster.unscaled$altmetricsCombo = with(dat.for.cluster.unscaled, facebookShareCount + deliciousCount + almBlogsCount + backtweetsCount)
    clusterColumns = append(clusterColumns, "altmetricsCombo")

    dat.for.cluster.unscaled = dat.for.cluster.unscaled[complete.cases(dat.for.cluster.unscaled[,clusterColumns]),]
    dat.for.cluster = dat.for.cluster.unscaled
    dat.for.cluster[,clusterColumns] = scale(dat.for.cluster.unscaled[,clusterColumns])
    dim(dat.for.cluster)
    summary(dat.for.cluster[,clusterColumns])
    scree_plot_for_number_clusters(dat.for.cluster[,clusterColumns])

    NUMBER.CLUSTERS = 5
    set.seed(42)
    for (i in seq(1:5)){
        cluster_fit = cluster_assignments(dat.for.cluster[,clusterColumns], NUMBER.CLUSTERS)
        dat_with_cluster_assignments <- data.frame(dat.for.cluster, cluster=cluster_fit$cluster)
        plot_cluster_centers(cluster_fit)
        print(rbind(t(round(cluster_fit$centers, 1)), percent=round(cluster_fit$size/sum(cluster_fit$size), 2)))
        print(cluster_fit$size)
        print(cluster_fit$tot.withinss)
    }

    round(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2), 2)
    round(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(0, 2, 5, 10, 200))), 2), 2)

    set.seed(42)
    exemplars = by(dat_with_cluster_assignments, list(dat_with_cluster_assignments$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "cluster", "year", "title", "plosSubjectTags", clusterColumns)])
    exemplars

    set.seed(42)
    field="Genomics"
    in_field = by(dat_with_cluster_assignments, 1:nrow(dat_with_cluster_assignments), function(x) { grepl(field,x$plosSubjectTags) })
    specific_dat_with_cluster_assignments = dat_with_cluster_assignments[in_field,]
    print(round(table(specific_dat_with_cluster_assignments$cluster)/(dim(specific_dat_with_cluster_assignments)[1]), 2))
    print(round(table(specific_dat_with_cluster_assignments$cluster)))

    specific_exemplars = by(specific_dat_with_cluster_assignments, list(specific_dat_with_cluster_assignments$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "cluster", "year", "title", "plosSubjectTags", clusterColumns)])
    specific_exemplars

}







do_factor_analysis_viz = function() {
    
    dat.with.factor.scores = get_factor_scores(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)

    factor.labels.plus = c(factor.labels, "f1000Factor", "wikipediaCites")
    #factor.labels.plus = factor.labels
    heatmap_of_articles_factors(dat.with.factor.scores, factor.labels.plus, filename = "img/heatmap_articles_factor_dend.png")

    dat.complete = get_complete_cases(dat.with.factor.scores, c(factor.labels.plus, "journal.x", "year", "authorsCount", "doi"))
    scree_plot_for_number_clusters(dat.complete[,factor.labels.plus])


    #source("clustergram.R") 
    #set.seed(42)
    #quartz(); clustergram(dat.complete[1:2000,factor.labels.plus], line.width = .001)

    NUMBER.CLUSTERS = 8

    set.seed(42)
    cluster_fit = cluster_assignments(dat.complete[,factor.labels.plus], NUMBER.CLUSTERS)

    t(round(cluster_fit$centers, 1))

    dat_with_cluster_assignments <- data.frame(dat.complete, cluster=cluster_fit$cluster)
    plot_cluster_centers(cluster_fit)

    round(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$journal.x), 2), 2)
    round(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2), 2)
    round(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(0, 2, 5, 10, 200))), 2), 2)

    cluster_fit$size

    set.seed(42)
    exemplars = by(dat_with_cluster_assignments, list(dat_with_cluster_assignments$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "cluster", factor.labels)])
    exemplars

}

