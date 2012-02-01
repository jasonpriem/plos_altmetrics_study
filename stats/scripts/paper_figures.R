####### GET SET UP

#setwd("/home/jason/projects/Plos altmetrics study")
#PATH_TO_RAW_DATA = "./datasets/"
#PATH_TO_DERIVED_DATA = "./datasets/"

setwd("~/Documents/Projects/PLoSimpact/new/plos_altmetrics_study/stats/scripts")
PATH_TO_RAW_DATA = "../data/raw/"
PATH_TO_DERIVED_DATA = "../data/derived/"

options(scipen=100)
options(digits=2)
options(width=50)

#colourblind friendly palettes from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)
library(ggplot2)
cbgFillPalette <- scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
cbgColourPalette <- scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))


####### GET DATA

load(paste(PATH_TO_DERIVED_DATA, "dat_research.RData", sep=""))
load(paste(PATH_TO_DERIVED_DATA, "dat_research_norm_transform.RData", sep=""))
d <-read.csv(paste(PATH_TO_RAW_DATA, "raw_events.txt.gz", sep=""), sep="\t")
d$pubDate  = strptime(d$date, "%Y-%m-%dT")


##### TABLE ON PAPERS PER YEAR, JOURNAL
addmargins(table(dat.research$year, dat.research$journal))

##### FIGURE ON PAPERS PER YEAR, JOURNAL

png("img/stackedjournalhist.png", width=500, height=500)
ggplot(dat.research, aes(x=year)) + stat_bin(aes(y=..count.., fill=journal)) + cbgFillPalette
#ggplot(dat.research, aes(x=year)) + stat_bin(aes(y=..count.., fill=journal)) + scale_fill_brewer(palette="Set1")
dev.off()


######## FIGURE ON PROPORTION OF PAPERS WITH NONZERO METRICS, BY METRIC

dat.nonzero = dat.research
dat.nonzero[,altmetricsColumns][dat.nonzero[,altmetricsColumns] > 1] = 1
summary(dat.nonzero[,altmetricsColumns])

nonzero = apply(dat.nonzero[,altmetricsColumns], 2, mean, na.rm=T)
nonzero <- nonzero[sort.list(nonzero, decreasing = T)]

nonzero.df = data.frame(names=names(nonzero), frq=nonzero)
nonzero.df$names <- factor(nonzero.df$names, levels=names(nonzero), ordered=T)

png("img/nonzero_by_metric.png", width=500, height=500)
ggplot(nonzero.df) + geom_bar(aes(names, frq)) + scale_y_continuous("", formatter="percent") + labs(x="") + coord_flip() + theme_bw() + opts(title = "") + cbgFillPalette
dev.off()

######## FIGURE ON NUMBER OF NONZERO METRICS PER PAPER

hist.nonzero = table(apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))

png("img/hist_research_nonzero_event_counts.png", width=500, height=500)
plot(hist.nonzero/sum(hist.nonzero), main="how many different non-zero metrics do papers receive?")
dev.off()

######## FIGURE ON NUMBER OF NONZERO METRICS PER PAPER, BY YEAR

hist.nonzero.by.year = table(dat.nonzero$year, apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))
nonzero.by.year = melt(hist.nonzero.by.year)
names(nonzero.by.year) = c("year", "num_metrics", "histcount")
png("img/hist_research_nonzero_event_counts_by_year.png", width=500, height=500)
qplot(num_metrics, histcount, data=nonzero.by.year, geom="bar", stat="identity") + facet_grid(year ~ .) + cbgFillPalette
dev.off()

######### FIGURE ON NUMBER OF UNIQUE CREATORS PER METRIC, PER YEAR

dois_per_year = ddply(d, .(year=format(pubDate, "%Y")), summarise, numdois = length(unique(doi)))
creatorevents = subset(d, eventType %in% c("citeulike","delicious", "backtweets"))
unique_creators_per_year = ddply(creatorevents, .(eventType, year=format(pubDate, "%Y")), summarise, numcreators = length(unique(creator)))
proportion_unique_creators_per_year = merge(unique_creators_per_year, dois_per_year, by="year")
proportion_unique_creators_per_year$fraction = with(proportion_unique_creators_per_year, numcreators/numdois)

png("img/unique_creators_per_year.png", width=500, height=500)
ggplot(proportion_unique_creators_per_year, aes(x=year, y=fraction*1000, fill=factor(eventType))) + geom_bar() + opts(title="unique creators in each year, per 1000 papers") + cbgFillPalette
dev.off()

############ FIGURE ON CUMULATIVE OVER TIME
d[which(d[,"latency"]<0),"latency"]=NA
png("img/event_timeline.png", width=200, height=750)
ggplot(d) + stat_bin(aes(x=latency/(60*60*24*365), y=..density.., position="stack")) + labs(x="years since publication") + cbgFillPalette + facet_grid(eventType ~ .)
dev.off()

##this one would be good, is superimposed, but it looks funny.  have to do binning beforehand, then use geom_density or something?
ggplot(d) + stat_bin(aes(x=latency/(60*60*24*365), y=..density.., position="stack", group=eventType, fill=eventType)) + labs(x="years since publication") + cbgFillPalette


############ TABLES ON CORRELATIONS AND ODDS RATIOS WITH CITATIONS

library(plyr)
source("utils.R")

split_upperquartile = colwise(function(x) ifelse(x > quantile(x, na.rm=T)[4], 1, 0), altmetricsColumns)
#b = a(dat.research.norm.transform)

results = c()
for (myjournal in c("pone", "ppat", "pbio")) {
    cat("\n", myjournal)
    dat_journal2010 = subset(dat.research, journal==myjournal)
    dat_journal2010 = subset(dat_journal2010, year=="2010")
    dat_journal2010_upperquartile = split_upperquartile(dat_journal2010)
    summary(dat_journal2010_upperquartile)

    for (col in altmetricsColumns) {
         cat("\n\n", col)
         table_quartile = table(dat_journal2010_upperquartile[,"wosCountThru2011"], dat_journal2010_upperquartile[,col])
         prop_table_quartile = prop.table(table_quartile, 1)
         #print(pt)
         if (dim(table_quartile)[2] > 1) {
             fisher_test_quartile = fisher.test(table_quartile)
             cat("\n", round(fisher_test_quartile$estimate, 1), " [", round(fisher_test_quartile$conf.int[1:2],1), "]", " p-value: ", round(fisher_test_quartile$p.value, 3), sep=" ")
         }
    }
    
    mycor = calc.correlations(dat_journal2010[, altmetricsColumns], "pairwise.complete.obs", "pearson")
    results = cbind(mycor[,"wosCountThru2011"], results)
    colnames(results)[1] = myjournal
}


########## PLOT CLUSTER CENTERS

clusterColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor")
clusterColumns = append(clusterColumns, "shareCombo")

prettyColumnNames = data.frame(col=clusterColumns, pretty=c("HTML page views", "Mendeley readers", "Web of Science cites", "F1000 rating", "Sharing combo"), stringsAsFactors=FALSE)

source("dat_cluster_centers.create.R")
    combo_and_scale = function(dat) {
        #dat = subset(dat.research.norm.transform, (journal=="pone") & (year==2010))
        dat.for.cluster.unscaled = dat

        dat.for.cluster.unscaled$shareCombo = with(dat.for.cluster.unscaled, facebookShareCount + deliciousCount + almBlogsCount + backtweetsCount)

        dat.for.cluster.unscaled = dat.for.cluster.unscaled[complete.cases(dat.for.cluster.unscaled[,clusterColumns]),]
        dat.for.cluster = dat.for.cluster.unscaled
        dat.for.cluster[,clusterColumns] = scale(dat.for.cluster.unscaled[,clusterColumns])
        dim(dat.for.cluster)
        summary(dat.for.cluster[,clusterColumns])
        return(dat.for.cluster)
    }
    
    dat = subset(dat.research.norm.transform, (journal=="pone") & (as.numeric(year) < 2010))
    dat.for.cluster = combo_and_scale(dat)
    scree_plot_for_number_clusters(dat.for.cluster[,clusterColumns])
    title("PLoS ONE (2007-2009)")

    NUMBER.CLUSTERS = 5
    set.seed(43)
    ### DID TRY THIS MORE TIMES TO VERIFY THIS SEED GETS THE LOWEST WSS RESULT AT THE END
    for (i in seq(1:1)){
        cluster_fit = cluster_assignments(dat.for.cluster[,clusterColumns], NUMBER.CLUSTERS)
        dat_with_cluster_assignments <- data.frame(dat.for.cluster, cluster=cluster_fit$cluster)
        plot_cluster_centers(cluster_fit, prettyColumnNames)
        
        cluster_labels = paste("flavour ", LETTERS[1:length(cluster_fit$size)], " (", round(100*cluster_fit$size/sum(cluster_fit$size), 0), "%)", sep="")
        metric_labels = prettyColumnNames$pretty[match(colnames(cluster_fit$centers),prettyColumnNames$col)]

        sorted_size = sort.list(cluster_fit$size, decr=T)
        print(cluster_labels[sorted_size])
        print(rbind(t(round((cluster_fit$centers)[sorted_size,clusterColumns], 1)), percent=round((cluster_fit$size)[sorted_size]/sum(cluster_fit$size), 2)))
        print(cluster_fit$tot.withinss)
    }
    
########## TABLES OF CORRELATES WITH CLUSTERS

    round(t(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2)), 2)
    round(t(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(0, 2, 5, 10, 200))), 2)), 2)

########## CLUSTER EXEMPLARS

predict_centers = function(dat_with_cluster_assignments, cluster_fit) {
        closest_cluster = NULL
        cluster_wss = NULL
        for (i in seq(1:dim(dat_with_cluster_assignments)[1])) {
        #for (i in seq(1:10)) {
            minwss=2147483647
        
            for (j in seq(1:NUMBER.CLUSTERS))   {
                wss = sum((as.matrix(dat_with_cluster_assignments[i,clusterColumns]) - cluster_fit$centers[j,]) ^2)
                if (wss < minwss) {
                    minwss = wss
                    closest_cluster[i] = j
                    cluster_wss[i] = wss
                }
            }
        }
        dat_with_cluster_assignments$cluster_guess = closest_cluster
        dat_with_cluster_assignments$cluster_wss = cluster_wss
        return(dat_with_cluster_assignments)
    }
    
    dat_with_cluster_wss = predict_centers(dat_with_cluster_assignments, cluster_fit)

    get_center_exemplars = function(dat) {
        center_exemplars = ddply(dat, .(cluster), function(x) x[(sort.list(x$cluster_wss, decr=FALSE))[1:10],])    
        return(center_exemplars)
    }
    
    get_random_exemplars = function(dat) {
        random_exemplars = by(dat, list(dat$cluster), FUN=function(x) x[sample(1:nrow(x), 10), c("doi", "cluster", "year", "title", "plosSubjectTags", clusterColumns)])
        return(center_exemplars)
    }

    center_exemplars = get_center_exemplars(dat_with_cluster_wss)
    set.seed(42)
    random_exemplars = get_random_exemplars(dat_with_cluster_wss)
    random_exemplars

    field="Genomics"
    in_field = by(dat_with_cluster_wss, 1:nrow(dat_with_cluster_wss), function(x) { grepl(field,x$plosSubjectTags) })
    specific_dat_with_cluster_assignments = dat_with_cluster_wss[in_field,]
    print(round(table(specific_dat_with_cluster_assignments$cluster)/(dim(specific_dat_with_cluster_assignments)[1]), 2))
    print(round(table(specific_dat_with_cluster_assignments$cluster)))

    specific_center_exemplars = get_center_exemplars(specific_dat_with_cluster_assignments)
    set.seed(42)
    specific_random_exemplars = get_random_exemplars(specific_dat_with_cluster_assignments)
    specific_center_exemplars
    

########## CLUSTER RULES

    dat = subset(dat.research.norm.transform, (journal=="pone") & (as.numeric(year) >= 2010))
    dat.for.tree.scaled = combo_and_scale(dat)
    dat_for_tree_with_clusters = predict_centers(dat.for.tree.scaled, cluster_fit)
    dat_for_tree_with_clusters$cluster = dat_for_tree_with_clusters$cluster_guess
    table(dat_for_tree_with_clusters$cluster)

    field="Genomics"
    in_field = by(dat_for_tree_with_clusters, 1:nrow(dat_for_tree_with_clusters), function(x) { grepl(field,x$plosSubjectTags) })
    specific_dat_with_cluster_assignments2010 = dat_for_tree_with_clusters[in_field,]

    specific_center_exemplars2010 = get_center_exemplars(specific_dat_with_cluster_assignments2010)
    set.seed(42)
    specific_random_exemplars2010 = get_random_exemplars(specific_dat_with_cluster_assignments2010)
    specific_center_exemplars2010


library(party)
library(RWeka)

dat = subset(dat.research.norm.transform, (journal=="pone") & (as.numeric(year) < 2010))

      
    dat.for.tree.unnormalized = merge(dat.research, dat_for_tree_with_clusters[,c("doi", "cluster")], by="doi")

predictionColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011", "f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
library(randomForest)
fit <- randomForest(wosCountThru2011 ~ ., data=dat.for.tree.unnormalized[,predictionColumns])
print(fit)

fit <- randomForest(factor(cluster) ~ ., data=dat.for.tree.unnormalized[,append(predictionColumns, "cluster")])
fit

#fit <- ctree(wosCountThru2011 ~ ., data=subset(dat.research, format(pubDate, "%Y")=="2009", predictionColumns))
#plot(fit, main="Conditional Inference Tree for wosCountThru2011")

fit <- JRip(factor(cluster) ~ ., data=dat.for.tree.unnormalized[,append(predictionColumns, "cluster")], control = Weka_control(R = TRUE, N=50))
fit
evaluate_Weka_classifier(fit)

#predictionColumnsNoCitation = c("htmlDownloadsCount","mendeleyReadersCount","f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
#fit <- JRip(factor(cluster) ~ ., data=dat.for.tree.unnormalized[,append(predictionColumnsNoCitation, "cluster")], control = Weka_control(R = TRUE, N=50))
#fit
#evaluate_Weka_classifier(fit)

#fit <- JRip(cut(wosCountThru2011, c(0, 10, 1000)) ~ ., data=subset(dat.for.tree, format(pubDate, "%Y")=="2009", predictionColumns), control = Weka_control(R = TRUE, N=100)); e = evaluate_Weka_classifier(fit); fit; e
#chisq.test(e$confusionMatrix)

#fit_htmlonly <- JRip(cut(wosCountThru2011, c(0, 10, 1000)) ~ ., data=subset(dat.for.tree, format(pubDate, "%Y")=="2009", c("wosCountThru2011", "htmlDownloadsCount")), control = Weka_control(R = TRUE, N=100)); e_htmlonly = evaluate_Weka_classifier(fit_htmlonly); fit_htmlonly; e_htmlonly

#predictionColumnsAltOnly = c("mendeleyReadersCount","f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")
#fit_alt <- JRip(cut(wosCountThru2011, c(0, 10, 1000)) ~ ., data=subset(dat.for.tree, format(pubDate, "%Y")=="2009", append(predictionColumnsAltOnly, "wosCountThru2011")), control = Weka_control(R = TRUE, N=100)); e_alt = evaluate_Weka_classifier(fit_alt); fit_alt; e_alt
#chisq.test(e_alt$confusionMatrix)

#chisq.test(matrix(c(3194, 976, 3096, 1074), ncol = 2))
    
