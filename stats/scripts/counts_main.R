    
get_research_articles <- function
### Do processing to get the research articles
(
    dat_raw_event_counts, ##<< raw event counts data from crawler SQL
    dat_raw_wos_2010,  ##<< raw ISI Web of Science event counts data from consolidation script
    dat_raw_wos_2011  ##<< raw ISI Web of Science event counts data from consolidation script
)
{
  ##note<< documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

  ##details<< First clean the altmetrics data from scraper
  dat_altmetrics_cleaned = clean_crawler_counts(dat_raw_event_counts)
  #write.table.gzip(dat_altmetrics_cleaned, basedir, "derived/event_counts_altmetrics_cleaned.txt")

  ##details<<  Then clean the ISI Web of Science event counts and extract relevant columns
  dat_extracted.wos.2010 = clean_wos_counts(dat_raw_wos_2010)  
  dat.extracted.wos.2011 = clean_wos_counts(dat_raw_wos_2011)  
  
  ##details<< Merge together the altmetrics and the WoS data
  dat.merged = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos.2010)
  dat.merged = merge_crawler_and_wos_counts(dat.merged, dat.extracted.wos.2011)
  #write.table.gzip(dat.merged, basedir, "derived/event_counts_merged_cleaned.txt")

  ##details<<  Filter to just research articles
  dat.research = research_articles_only(dat.merged)
  
  return(dat.research)
  ### Return rearch articles

  ##examples<<
  data(dat_raw_event_counts)
  dat_raw_wos_2010 = data(dat_raw_wos_2010)
  dat_raw_wos_2011 = data(dat_raw_wos_2011)
  get_research_articles = get_research_articles(dat_raw_event_counts, dat_raw_wos_2010, dat_raw_wos_2011)
}

     
counts_main <- function
### Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study
(
    dat_raw_event_counts, ##<< raw event counts data from crawler SQL
    dat_raw_wos_2010,  ##<< raw ISI Web of Science event counts data from consolidation script
    dat_raw_wos_2011,  ##<< raw ISI Web of Science event counts data from consolidation script
    altmetricsColumns,  ##<< column names of metrics to include in correlation
    number_factors ##<< Number of factors in factor analysis
)
{
  ##note<< documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

  ##details<<  Filter to just research articles
  dat.research = get_research_articles(dat_raw_event_counts, dat_raw_wos_2010, dat_raw_wos_2011)
  #save(dat.research, file="altmetrics.analysis/data/dat_research.RData", compress="gzip")
  
  ##details<<  Do normalization
  ## on the selected altmetrics columns
  dat.research.norm = normalize_altmetrics(dat.research, altmetricsColumns)
  #save(dat.research.norm, file="altmetrics.analysis/data/dat_research_norm.RData", compress="gzip")
  
  ##details<< Do transformation
  dat.research.norm.transform = dat.research.norm
  dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
  
  ##details<< Calculate correlation matrix
  ## using pairwise-complete observations and Pearson correlations
  mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")
  
  ##details<<  Run exploratory factor analysis and calculate factor scores
  ## Applying factor names that we know only because we've already run this and 
  ## looked at the results
  factor.labels = c("citations", "facebookLike", "downloads", "comments", "bookmarks", "facebookClick")
  dat.with.factor.scores = get_factor_scores(dat.research.norm.transform, mycor, number_factors, factor.labels)
  #save(dat.with.factor.scores, file="../data/dat_with_factor_scores.RData", compress="gzip")
  
  return(dat.with.factor.scores)
  ### Returns dataframe that contains factor scores
  
  ##examples<<
  library(altmetrics.analysis)
  data(dat_raw_event_counts)
  data(dat_raw_wos_2010)
  data(dat_raw_wos_2011)
  dat.with.factor.scores = counts_main(dat_raw_event_counts, dat.raw.wos, altmetricsColumns, 6)
  dim(dat.with.factor.scores)
}

setup = function() {    


##### create dat.research
    
setwd("~/Documents/Projects/PLoSimpact/new/plos_altmetrics_study/stats/scripts")
source("counts_clean_merge_filter.R")
    
dat_raw_wos_2010 = read.csv("../data/raw/raw_wos_2010.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
dat_raw_wos_2011 = read.csv("../data/raw/raw_wos_2011.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="", row.names=NULL)
dat_raw_event_counts = read.csv("../data/raw/raw_event_counts.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

dat_altmetrics_cleaned = clean_crawler_counts(dat_raw_event_counts)
dat.extracted.wos.2010 = clean_wos_counts(dat_raw_wos_2010)  
dat.extracted.wos.2011 = clean_wos_counts(dat_raw_wos_2011)  

dat.merged.2010 = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos.2010)
names(dat.merged.2010)[names(dat.merged.2010)=="wosCount"] = "wosCountThru2010"
dat.merged.2011 = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos.2011)
names(dat.merged.2011)[names(dat.merged.2011)=="wosCount"] = "wosCountThru2011"
dat.merged.2010.2011 = merge_crawler_and_wos_counts(dat.merged.2010, subset(dat.merged.2011, select=c(doi, wosCountThru2011)))
dat.merged = dat.merged.2010.2011

dat.research = research_articles_only(dat.merged)
dim(dat.research)
save(dat.research, file = "../data/derived/dat_research.RData", compress="gzip")

#### create dat.research.norm.transform

source("counts_normalize.R")
source("do_normalization_viz.R")

altmetricsColumns = c( "wosCountThru2010", "wosCountThru2011",
"almScopusCount",
"almPubMedCentralCount",
"almCrossRefCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",
"almBlogsCount",            
"facebookCommentCount",          
"facebookLikeCount",          
"facebookShareCount",          
"facebookClickCount",          
"f1000Factor",              
"wikipediaCites",           
"backtweetsCount")

WINDOW_WIDTH_IN_DAYS = 365

dat.backgrounds = backgrounds_for_each_journal(dat.research, altmetricsColumns, WINDOW_WIDTH_IN_DAYS)
save(dat.backgrounds, file = "../data/derived/dat_backgrounds.RData", compress="gzip")
plot_all_backgrounds_separately(dat.research, dat.backgrounds, altmetricsColumns)
plot_all_backgrounds_overlay(dat.research, dat.backgrounds, altmetricsColumns)

dat.research.norm = normalize_altmetrics(dat.research, altmetricsColumns, dat.backgrounds)
save(dat.research.norm, file = "../data/derived/dat_research_norm.RData", compress="gzip")


##details<< Do transformation
transformation_function = function(x) {log(1+x)}  
dat.research.norm.transform = dat.research.norm
dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
save(dat.research.norm.transform, file = "../data/derived/dat_research_norm_transform.RData", compress="gzip")



#### look at clusters

source("do_clustering_viz.R")

dat.for.cluster = subset(dat.research.norm.transform, journal=="pone")

dat.for.cluster$shareCombo = dat.for.cluster$facebookShareCount + dat.for.cluster$deliciousCount + dat.for.cluster$almBlogsCount + dat.for.cluster$backtweetsCount

#clusterColumns = c("htmlDownloadsCount","plosCommentCount","mendeleyReadersCount","wosCountThru2011","f1000Factor","wikipediaCites", "shareCombo")
clusterColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor","wikipediaCites", "shareCombo")
clusterColumnsRawData = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
dat.for.cluster = dat.for.cluster[complete.cases(dat.for.cluster[,clusterColumns]),]
dim(dat.for.cluster)
summary(dat.for.cluster[,clusterColumns])
set.seed(42)
scree_plot_for_number_clusters(dat.for.cluster[,clusterColumns])

library(clValid)
intern = clValid(dat.for.cluster[1:1000,clusterColumns], 2:9, maxitems=1000, clMethods=c("hierarchical","kmeans","pam"), validation="internal")
stab = clValid(dat.for.cluster[1:1000,clusterColumns], 2:9, maxitems=1000, clMethods=c("kmeans"), validation="stability")


NUMBER.CLUSTERS = 5
set.seed(42)
for (i in seq(1:4)){
    cluster_fit = cluster_assignments(dat.for.cluster[,clusterColumns], NUMBER.CLUSTERS)
    t(round(cluster_fit$centers, 1))
    dat_with_cluster_assignments <- data.frame(dat.for.cluster, cluster=factor(cluster_fit$cluster))
    plot_cluster_centers(cluster_fit)
    
    print(cluster_fit$tot.withinss)

    rbind(t(round(cluster_fit$centers, 1)), percent=round(cluster_fit$size/sum(cluster_fit$size), 2))
    cluster_fit$size
}

round(prop.table(table(dat_with_cluster_assignments$cluster, format(dat_with_cluster_assignments$pubDate, "%Y")), 2), 2)
round(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(9, 2, 5, 10, 200))), 2), 2)


set.seed(42)
exemplars = by(dat_with_cluster_assignments, list(dat_with_cluster_assignments$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "title", "cluster", clusterColumns)])
exemplars
write.csv(exemplars[[1]], file="cluster_exemplars.csv", append=F)

#  htmlDownloadsCount mendeleyReadersCount wosCountThru2011 f1000Factor wikipediaCites sharingCombo percent
#1                0.7                  0.7              0.8         3.4            0.1             0.8    0.03	expert fav
#2                0.8                  0.8              0.6         0.0            0.3             2.5    0.17	read and shared
#3                1.9                  1.4              0.7         0.4            1.1             9.7    0.02	popular hit
#4                0.5                  0.3              0.4         0.0            0.0             0.1    0.53	not much attention
#5                0.7                  0.9              0.9         0.0            0.2             0.2    0.26	read and cited



#########  factor analysis


data(dat_research_norm)
dat.research.norm.transform = dat.research.norm
dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])

factorColumns = c("wosCountThru2011",
"wosCountThru2010",
#"almScopusCount",
#"almCrossRefCount",
#"almPubMedCentralCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",
"almBlogsCount",            
"facebookCommentCount",          
"facebookLikeCount",          
"facebookShareCount",          
"facebookClickCount",          
"f1000Factor",              
"wikipediaCites",           
"backtweetsCount")

mycor = calc.correlations(dat.research.norm.transform[, factorColumns], "pairwise.complete.obs", "pearson")

get_complete_cases = function(dat, columns){
    dat.complete = dat[complete.cases(dat[,columns]),]
    return(dat.complete ) 
}

### @export "factor analysis results"


### do I want this to be jsut for plos one, or all ????  needs to be consistent with below
scree_plot_for_number_clusters(get_complete_cases(dat.research.norm.transform[,factorColumns]))

fa.results = do_factor_analysis(dat.research.norm.transform[,factorColumns], mycor, 5)


### @export "factor analysis low communality"

fa.results$communality[which(fa.results$communality < .15)]


### @export "factor analysis cluster calculations"

factor.labels = c("facebook", "downloads", "citations", "comments", "bookmarks")
factor.labels.plus = c(factor.labels, "f1000Factor", "wikipediaCites")

make_graphs(fa.results, factor.labels)

dat.with.factor.scores = get_factor_scores(dat.for.cluster, mycor, length(factor.labels), factor.labels)

factorcor = calc.correlations(dat.with.factor.scores[, factor.labels.plus], "pairwise.complete.obs", "pearson")
heatmap(factorcor)


######### predict citations

library(party)

predictionColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011", "f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
#fit <- ctree(wosCountThru2011 ~ ., data=subset(dat.research, format(pubDate, "%Y")=="2009", predictionColumns))
#plot(fit, main="Conditional Inference Tree for wosCountThru2011")

fit <- JRip(cluster ~ ., data=subset(dat_for_tree, format(pubDate, "%Y")=="2010", append(predictionColumns, "cluster")), control = Weka_control(R = TRUE, N=50))
fit
evaluate_Weka_classifier(fit)

predictionColumnsNoCitation = c("htmlDownloadsCount","mendeleyReadersCount","f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
fit <- JRip(cluster ~ ., data=subset(dat_for_tree, format(pubDate, "%Y")=="2010", append(predictionColumnsNoCitation, "cluster")), control = Weka_control(R = TRUE, N=50))
fit
evaluate_Weka_classifier(fit)


fit <- JRip(cut(wosCountThru2011, c(0, 10, 1000)) ~ ., data=subset(dat_for_tree, format(pubDate, "%Y")=="2009", predictionColumns), control = Weka_control(R = TRUE, N=100)); e = evaluate_Weka_classifier(fit); fit; e
chisq.test(e$confusionMatrix)

fit_htmlonly <- JRip(cut(wosCountThru2011, c(0, 10, 1000)) ~ ., data=subset(dat_for_tree, format(pubDate, "%Y")=="2009", c("wosCountThru2011", "htmlDownloadsCount")), control = Weka_control(R = TRUE, N=100)); e_htmlonly = evaluate_Weka_classifier(fit); fit_htmlonly; e_htmlonly

chisq.test(matrix(c(3194, 976, 3096, 1074), ncol = 2))

}
