### Necessary to avoid problems with fancy quotes in p-value reporting!
options(useFancyQuotes = FALSE)

library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

library(altmetrics.analysis)

data(dat_research_norm)
dat.research.norm.transform = dat.research.norm
dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")

### These functions are duplicated from do_factor_analysis_viz.R
### Should really reuse that code instead
get_complete_cases = function(dat, columns){
    dat.complete = dat[complete.cases(dat[,columns]),]
    return(dat.complete ) 
}

cluster_assignments = function(dat, number.clusters){
    fit <- kmeans(dat, number.clusters, iter.max=100)
    round(fit$centers, 1)
    png("article_factor_cluster_pairs.png", width=500, height=500)        
    plot(dat, col = fit$cluster)
    points(fit$centers, col = 1:2, pch = 8, cex=2)
    dev.off()
    return(fit)
}

### @export "factor analysis results"

factor.labels = c("citations", "facebookLike", "downloads", "comments", "bookmarks", "facebookClick")
fa.results = do_factor_analysis(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)

### @export "factor analysis low communality"

fa.results$communality[which(fa.results$communality < .15)]


### @export "factor analysis cluster calculations"

dat.with.factor.scores = get_factor_scores(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)

factor.labels.plus = c(factor.labels, "f1000Factor", "wikipediaCites")

dat.complete = get_complete_cases(dat.with.factor.scores, c(factor.labels.plus, "journal.x", "year", "authorsCount", "doi"))

#scree_plot_for_number_clusters(dat.complete[,factor.labels.plus])

#source("clustergram.R") 
#set.seed(42)
#quartz(); clustergram(dat.complete[1:2000,factor.labels.plus], line.width = .001)


### @export "factor analysis cluster centers"

factor.labels.plus
NUMBER.CLUSTERS = 8
set.seed(42)
cluster_fit = cluster_assignments(dat.complete[,factor.labels.plus], NUMBER.CLUSTERS)
t(round(cluster_fit$centers, 1))
cluster_fit$size

### @export "factor analysis cluster plot"

dat_with_cluster_assignments = data.frame(dat.complete, cluster=cluster_fit$cluster)

### @export "factor analysis cluster tables year"

round(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$journal.x), 2), 2)

### @export "factor analysis cluster tables journal"

round(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2), 2)

### @export "factor analysis cluster tables authorsCount"

round(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(9, 2, 5, 10, 200))), 2), 2)

### @export "factor analysis cluster exemplars"

set.seed(42)
exemplars = by(dat_with_cluster_assignments, list(dat_with_cluster_assignments$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "cluster", factor.labels)])
exemplars



