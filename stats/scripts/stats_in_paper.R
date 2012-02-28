####### GET SET UP

PATH_TO_RAW_DATA = "../data/raw/"
PATH_TO_DERIVED_DATA = "../data/derived/"
PATH_TO_TABLES = "../results/tables/"
PATH_TO_FIGURES = "../results/figures/"

options(scipen=100)
options(digits=2)
options(width=50)

source("lookup_tables.R")
source("utils.R")


########## How many papers total

source("preprocessing_eventcounts_clean.R")

raw_crawler_eventcounts = read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_eventcounts.txt.gz", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
df_altmetrics_cleaned = clean_crawler_counts(raw_crawler_eventcounts)

cat("Total papers with altmetrics data:", dim(df_altmetrics_cleaned)[1])

##### TABLE 1: assembled manually
#nothing

##### FIGURE 1:  Hamming window

library(signal)
window = hamming(WINDOW_WIDTH_IN_DAYS)
png(PATH_TO_FIGURES & "figure1.png")
days = seq(window)- (length(window)/2)
plot(days, window)
dev.off()


##### TABLE 2:  Table on papers per year, per journal
tablePaperDist = addmargins(table(dat.research$year, dat.research$journal))
print(xtable(rbind(plos_journals[colnames(tablePaperDist)], tablePaperDist), digits=0), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table2.html")
write.table(rbind(plos_journals[colnames(tablePaperDist)], round(tablePaperDist, 0)), PATH_TO_TABLES & "table2.csv", sep=",", col.names=FALSE)

# FIGURE ON PAPERS PER YEAR, JOURNAL
#ggplot(dat.research, aes(x=year)) + stat_bin(aes(y=..count.., fill=journal)) + cbgFillPalette


######## FIGURE 2: proportion of papers with prettyAltmetricsColumns metrics, by metric

dat.nonzero.indicator = dat.research
dat.nonzero.indicator[,altmetricsColumns][dat.nonzero.indicator[,altmetricsColumns] > 1] = 1
summary(dat.nonzero.indicator[,altmetricsColumns])

nonzero.freq = apply(dat.nonzero.indicator[,altmetricsColumns], 2, mean, na.rm=T)
nonzero.freq <- nonzero.freq[sort.list(nonzero.freq, decreasing = T)]

prettyNames = prettyAltmetricsColumns[names(nonzero.freq)]

nonzero.df = data.frame(prettyNames=prettyNames, col=names(nonzero.freq), freq=nonzero.freq)
nonzero.df$names <- factor(nonzero.df$prettyNames, levels=nonzero.df$prettyNames, ordered=T)

png(PATH_TO_FIGURES & "figure2.png", width=500, height=500)
ggplot(nonzero.df) + geom_bar(aes(names, freq)) + scale_y_continuous("", formatter="percent", breaks=c(0, .2, .4, .6, .8, 1)) + labs(x="") + coord_flip() + theme_bw() + opts(title = "") + cbgFillPalette
dev.off()

print(nonzero.freq)

nonzero.freq.plosone = apply(subset(dat.nonzero.indicator, journal=="pone", altmetricsColumns), 2, mean, na.rm=T)
nonzero.freq.plosone <- nonzero.freq.plosone[sort.list(nonzero.freq.plosone, decreasing = T)]

print(nonzero.freq.plosone)

######## FIGURE 3: NUMBER OF NONZERO METRICS PER PAPER

dat.nonzero.indicator.engaged = dat.nonzero.indicator
dat.nonzero.indicator.engaged$num_nonzero = apply(subset(dat.nonzero.indicator[,altmetricsColumns], select= -c(wosCountThru2011, almScopusCount, almPubMedCentralCount, almCrossRefCount)), 1, sum, na.rm=T)

print(summary(dat.nonzero.indicator.engaged$num_nonzero))

print(cumsum(table(dat.nonzero.indicator.engaged$num_nonzero)))/length(dat.nonzero.indicator.engaged$num_nonzero)

png(PATH_TO_FIGURES & "figure3.png", width=500, height=500)
ggplot(subset(dat.nonzero.indicator.engaged, num_nonzero>=2), aes(x=num_nonzero)) + geom_histogram(aes(y=..density..), alpha=0.5, binwidth=1, position="identity", breaks=1:14) + labs(x="Number of engaged sources", y="Proportion of papers") + theme_bw() + opts(title = "") + cbgFillPalette + cbgColourPalette
#ggplot(subset(dat.nonzero.indicator.engaged, num_nonzero>0), aes(x=num_nonzero, fill=year)) + geom_density(aes(y=..density..), alpha=0.5, adjust=4) + labs(x="Number of engaged sources", y="Density") + theme_bw() + opts(title = "") + cbgFillPalette
#ggplot(subset(dat.nonzero.indicator.engaged, num_nonzero>0), aes(x=num_nonzero)) + geom_freqpoly(aes(y=..density.., color=year), alpha=0.5, binwidth=1, position="identity") + labs(x="Number of engaged sources", y="Number of papers") + theme_bw() + opts(title = "") + cbgFillPalette + cbgColourPalette
dev.off()


########## FIGURE 5: EVENT CREATION BY CREATOR

#load(paste(PATH_TO_DERIVED_DATA, "dat_research.RData", sep=""))
#load(paste(PATH_TO_DERIVED_DATA, "dat_research_norm_transform.RData", sep=""))
d <-read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_events.txt.gz", sep=""), sep="\t")
d$pubDate  = strptime(d$date, "%Y-%m-%dT")

art <-read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_eventcounts.txt.gz", sep=""), sep="\t")

# restrict events to those on research articles
art.r <- art[art$articleType=="Research Article", ]
d <- d[d$doi %in%  art.r$doi,]

# column for which journal
d$journal <- substr(d$doi, 17, 20)

#######

# get the events
bt = subset(d, eventType %in% c("citeulike","delicious", "backtweets"))

# frame for just Plos ONE, where we have much more data
bt.pone <- bt[bt$journal == "pone",]
bt.pone$doi <- factor(bt.pone$doi[1:nrow(bt.pone)]) #drop unused levels
bt.pone$creator <- factor(bt.pone$creator[1:nrow(bt.pone)])

# remove tweets with negative latency
nrow(bt.pone[bt.pone$latency <= 0,]) / nrow(bt.pone) # 2%
bt.pone = bt.pone[bt.pone$latency > 0,]

# check out the distributions (as expected, mostly power-law)
#creators <- ddply(bt.pone, .(creator, eventType), summarize, length(unique(creator)))

creators.df = data.frame(creator=NULL, num_creations=NULL, eventType=NULL)
for (eType in c("citeulike","delicious", "backtweets")) {
    sub = subset(bt.pone, eventType==eType)
    creators <- rev(sort(table(sub$creator)))
    creators.df = rbind(creators.df, data.frame(creator=names(creators), num_creations=creators, eventType=eType))
#plot(creators, log="xy")
}

png(PATH_TO_FIGURES & "figure5.png", width=600, height=300)
ggplot(data=creators.df, aes(x=num_creations)) + geom_histogram(binwidth=0.35, position="identity") + scale_x_log10(formatter="comma", breaks=c(0, 1, 10, 100, 1000)) + scale_y_log10(formatter="comma", breaks=c(0, 1, 10, 100, 1000)) + labs(x="number of events", y="number of distinct event creators") + theme_bw() + cbgFillPalette + facet_grid(~ eventType)
dev.off()

############ FIGURE 6:   EVENTS OVER TIME, BY METRIC

df_events = d

# remove tweets with negative latency
nrow(df_events[df_events$latency <= 0,]) / nrow(df_events) 
df_events = df_events[df_events$latency > 0,]
df_events = df_events[df_events$latency < (60*60*24*365*5),]

months.df = data.frame(month=NULL, num_events=NULL, eventType=NULL)
for (eType in c("html views", "pdf views", "native comments", "citeulike", "backtweets", "delicious")) {
    sub = subset(df_events, eventType==eType)
    sub$latency_round = round(sub$latency/(60*60*24*7), 0)
    sub = ddply(sub, .(latency_round), summarise, total=sum(count))
    months.df = rbind(months.df, data.frame(month=sub$latency_round, num_events=sub$total, eventType=eType))
    #plot(as.numeric(names(months)), months)
#plot(creators, log="xy")
}


png(PATH_TO_FIGURES & "figure6.png", width=600, height=500)
ggplot(data=months.df, aes(x=month, y=num_events, color=eventType)) + 
    geom_point(aes(alpha=0.5)) + 
    scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels=c(1, 10, 100, 1000, 10000, 100000)) + 
    scale_x_continuous(breaks=c((1:5)*52)) + 
    labs(x="weeks since publication", y="total events each week") + 
    theme_bw() + cbgColourPalette + cbgFillPalette + 
    geom_smooth(span=0.3) + scale_alpha(legend = FALSE)
dev.off()



############  FIGURE 7:  events by latency

source("do_normalization_viz.R")

load(file = "../data/derived/dat_backgrounds.RData")

journals = names(dat.backgrounds)
dat = dat.research
yrange = get_ranges(dat, altmetricsColumns)

cols = altmetricsColumns
dat$pubDateVal = strptime(dat$pubDate, "%Y-%m-%d")
dat$pubDateVal = as.POSIXct(dat$pubDateVal)
xrange = range(dat$pubDateVal)

png(paste(PATH_TO_FIGURES & "figure7.png", sep=""), width=800, height=800)

#quartz()
par(mfrow = c(ceiling(length(altmetricsColumns)/4), 4), oma=c(2,2,4,2), mar=c(3, 2, 1.5, 2))
for (col in cols) {
	i=0
	allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
	plot(xrange, allrange, type="n", main=prettyAltmetricsColumns[col])
	
	for (journal in journals) {
		i = i+1
		inJournal = which(dat$journal==journal)
		journal.background = dat.backgrounds[[journal]]
		#quartz()		
		lines(dat[inJournal, "pubDateVal"], journal.background[,col], col=cbgRaw[i], lwd=3)
	}
}
#plot(1)
legend("center", journals, col = cbgRaw[1:7], lty=1, bty="n", fill=cbgRaw[1:7])
title(paste("Trends over time per journal"), outer=TRUE)
dev.off()


###### FIGURE 8


####### FIGURE 9: Correlations using all data, normalized

#source("events_correlations_plot.R")
library(ggplot2)
library(reshape2)
library(ggdendro)


mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")
colnames(mycor) = prettyAltmetricsColumns[colnames(mycor)]
rownames(mycor) = prettyAltmetricsColumns[rownames(mycor)]



dd.col <- as.dendrogram(hclust(dist(mycor)))
col.ord <- rev(order.dendrogram(dd.col))

dd.row <- as.dendrogram(hclust(dist(t(mycor))))
row.ord <- rev(order.dendrogram(dd.row))

#xx <- scale(mycor)[col.ord, row.ord]
xx <- mycor[col.ord, row.ord]
xx_names <- attr(xx, "dimnames")
df <- as.data.frame(xx)
colnames(df) <- xx_names[[2]]
df$col <- xx_names[[1]]
df$col <- with(df, factor(col, levels=col, ordered=TRUE))

mycor_melted <- melt(df, id.vars="col")

png(paste(PATH_TO_FIGURES & "figure9.png", sep=""), width=600, height=600)

ggplot(mycor_melted, aes(col, variable)) + geom_tile(aes(fill = value), colour = "white") + 
     scale_fill_gradient(low = "white", high = "black", space="Lab") + 
     theme_grey(base_size = 9) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 270, hjust = 0, colour = "black"), 
     axis.text.y = theme_text(size = 12, colour = "black", hjust=1))


dev.off()    


############# TABLE 3:  Factor results

library(psych)

mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")
colnames(mycor) = prettyAltmetricsColumns[colnames(mycor)]
rownames(mycor) = prettyAltmetricsColumns[rownames(mycor)]

fa.results = fa(mycor, 6, fm="minres", rotate="promax", 
                 residuals=TRUE, n.obs=max(dim(dat.research.norm.transform)))

colnames(fa.results$loadings) = c("citations", "pageviews and shares", "facebook-hosted discussion", "plos-hosted comments", "social ref saves", "pdf downloads")

fa.results$loadings
print(fa.results$loadings, digits=2, cutoff=0)

loadings_raw = matrix(fa.results$loadings, dim(fa.results$loadings)[1], dim(fa.results$loadings)[2])
rownames(loadings_raw) = rownames(mycor)
colnames(loadings_raw) = colnames(fa.results$loadings)

print(xtable(loadings_raw, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table3.html")
write.table(loadings_raw, PATH_TO_TABLES & "table3.csv", sep=",", col.names=FALSE)


#########  Supplementary table 1:  Factor results weights


fa.results$weights
colnames(fa.results$weights) = colnames(fa.results$loadings)

print(xtable(fa.results$weights, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptable1.html")
write.table(fa.results$weights, PATH_TO_TABLES & "supptable1.csv", sep=",", col.names=FALSE)


#########  FIGURE 10:  Factor correlations

fa.cor = fa.results$r.scores
rownames(fa.cor) = colnames(fa.results$loadings)
colnames(fa.cor) = colnames(fa.results$loadings)

dd.col <- as.dendrogram(hclust(dist(fa.cor)))
col.ord <- rev(order.dendrogram(dd.col))

dd.row <- as.dendrogram(hclust(dist(t(fa.cor))))
row.ord <- rev(order.dendrogram(dd.row))

#xx <- scale(mycor)[col.ord, row.ord]
xx <- fa.cor[col.ord, row.ord]
xx_names <- attr(xx, "dimnames")
df <- as.data.frame(xx)
colnames(df) <- xx_names[[2]]
df$col <- xx_names[[1]]
df$col <- with(df, factor(col, levels=col, ordered=TRUE))

fa.cor.melted <- melt(df, id.vars="col")

png(PATH_TO_FIGURES & "figure10.png", width=800, height=800)

ggplot(fa.cor.melted, aes(col, variable)) + geom_tile(aes(fill = value+1),
     colour = "white") + scale_fill_gradient2(low = "grey50", mid="white", high = "steelblue", trans="log", midpoint=0) + 
     theme_grey(base_size = 12) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 340, hjust = 0.5, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) + 
     geom_text(aes(x=col,y=variable, label=sprintf("%.1f", value)), data=fa.cor.melted, size=5, colour="black")

dev.off()

#fa.diagram(fa.results, simple=T, cex=.5)
    
    
############ Table 4: TABLE WITH CORRELATIONS TO CITATIONS

library(plyr)
source("utils.R")

dat = c()
for (myjournal in c("pbio", "ppat", "pone")) {
    cat("\n", myjournal)
    dat_journal2010 = subset(dat.research, journal==myjournal)
    dat_journal2010 = subset(dat_journal2010, year=="2010")
    mycor = calc.correlations(dat_journal2010[, altmetricsColumns], "pairwise.complete.obs", "spearman")
    dat = cbind(mycor[,"wosCountThru2011"], dat)
    colnames(dat)[1] = paste(myjournal, " 2010 (n=", dim(dat_journal2010)[1], ")", sep="")
}

print(round(dat[sort.list(dat[,1], dec=T),], 2))

# Based on example at http://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
rownames(dat) = prettyAltmetricsColumns[rownames(dat)]

dat_melted = melt(dat)
dat_melted$X2 <- factor(dat_melted$X2, levels(dat_melted$X2)[c(2, 3, 1)], ordered=T)
dat_melted$X1 <- factor(dat_melted$X1, levels(dat_melted$X1)[sort.list(dat[levels(dat_melted$X1),1], dec=F)], ordered=T)

png(PATH_TO_TABLES & "table4.png", width=600, height=500)
ggplot(dat_melted, aes(X2, X1)) + 
    geom_tile(aes(fill = value), colour = "white") + 
    scale_fill_gradient(low = "white", high = "steelblue") + 
     theme_grey(base_size = 12) + labs(x = "", y = "") + 
     scale_x_discrete(expand = c(0, 0)) + 
     scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 340, hjust = 0.5, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) + 
     geom_text(aes(x=X2,y=X1, label=sprintf("%.1f", value)), data=dat_melted, size=4, colour="black")
dev.off()

print(xtable(dat, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table4.html")
write.table(dat, PATH_TO_TABLES & "table4.csv", sep=",", col.names=FALSE)


########## Figure 11:  PLOT CLUSTER CENTERS

clusterColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor")
clusterColumns = append(clusterColumns, "shareCombo")

prettyClusterNames = c("HTML page views", "Mendeley bookmarks", "Web of Science cites", "F1000 rating", "Sharing combo")
names(prettyClusterNames) = clusterColumns

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
#scree_plot_for_number_clusters(dat.for.cluster[,clusterColumns])
#title("PLoS ONE (2007-2009)")

NUMBER.CLUSTERS = 5
set.seed(43)
### DID TRY THIS MORE TIMES TO VERIFY THIS SEED GETS THE LOWEST WSS RESULT AT THE END
for (i in seq(1:1)){
    cluster_fit = cluster_assignments(dat.for.cluster[,clusterColumns], NUMBER.CLUSTERS)
    dat_with_cluster_assignments <- data.frame(dat.for.cluster, cluster=cluster_fit$cluster)
    plot_cluster_centers(cluster_fit, prettyClusterNames)
    
    cluster_labels_simple = paste("cluster ", LETTERS[1:length(cluster_fit$size)], sep="")
    cluster_labels = paste("cluster ", LETTERS[1:length(cluster_fit$size)], " (", round(100*cluster_fit$size/sum(cluster_fit$size), 0), "%)", sep="")
    metric_labels = prettyClusterNames[colnames(cluster_fit$centers)]

    sorted_size = sort.list(cluster_fit$size, decr=T)
    print(cluster_labels[sorted_size])
    print(rbind(t(round((cluster_fit$centers)[sorted_size,clusterColumns], 1)), percent=round((cluster_fit$size)[sorted_size]/sum(cluster_fit$size), 2)))
    print(cluster_fit$tot.withinss)
}


a = t(round((cluster_fit$centers)[sorted_size,clusterColumns], 1))
colnames(a) = cluster_labels[sorted_size]

#round(t(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2)), 2)
#round(t(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(0, 2, 5, 10, 200))), 2)), 2)

png(PATH_TO_FIGURES & "figure11.png", width=800, height=200)

ggplot(melt(a), aes(X2, X1, width=rep(1.5*(cluster_fit$size[sorted_size])/(max(cluster_fit$size)),each=5))) + geom_tile(aes(fill = value+1), colour = "white") + 
    scale_fill_gradient2(low = "grey50", mid="white", high = "steelblue", trans="log", midpoint=0) + 
     theme_grey(base_size = 12) + labs(x = "", y = "") + 
     scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 340, hjust = 0.5, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) +
     geom_text(aes(x=X2,y=X1, label=sprintf("%.1f", value)),data=melt(a), size=3, colour="black")
dev.off()     

########## supp info TABLES OF CORRELATES WITH CLUSTERS

print(xtable(a, digits=1), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptable2.html")
write.table(a, PATH_TO_TABLES & "supptable2.csv", sep=",", col.names=FALSE)


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
    center_exemplars = ddply(dat, .(cluster), function(x) x[(sort.list(x$cluster_wss, decr=FALSE))[1:3],])    
    return(center_exemplars)
}
    
get_random_exemplars = function(dat) {
    random_exemplars = by(dat, list(dat$cluster), FUN=function(x) x[sample(1:nrow(x), 3), c("doi", "cluster", "year", "title", "plosSubjectTags", clusterColumns)])
    return(center_exemplars)
}

center_exemplars = get_center_exemplars(dat_with_cluster_wss)
set.seed(42)
random_exemplars = get_random_exemplars(dat_with_cluster_wss)
#random_exemplars

field="Ecology"
in_field = by(dat_with_cluster_wss, 1:nrow(dat_with_cluster_wss), function(x) { grepl(field,x$plosSubjectTags) })
specific_dat_with_cluster_assignments = dat_with_cluster_wss[in_field,]
print(round(table(specific_dat_with_cluster_assignments$cluster)/(dim(specific_dat_with_cluster_assignments)[1]), 2))
print(round(table(specific_dat_with_cluster_assignments$cluster)))

specific_center_exemplars = get_center_exemplars(specific_dat_with_cluster_assignments)
set.seed(42)
specific_random_exemplars = get_random_exemplars(specific_dat_with_cluster_assignments)
specific_center_exemplars
    
print(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster")))

print(xtable(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster"))), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptable3.html")
write.table(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster")), PATH_TO_TABLES & "supptable3.csv", sep=",", col.names=FALSE)

write("<ul>", file=PATH_TO_TABLES & "table5.html", append=F)
for (i in seq(specific_center_exemplars[,1])) {
    write(sprintf("<li>%s, <a href='http://dx.doi.org/%s'>%s</a> (<a href='http://www.plosone.org/article/metrics/info:doi/%s'>metrics</a>)", cluster_labels_simple[specific_center_exemplars[i,]$cluster], specific_center_exemplars[i,]$doi, specific_center_exemplars[i,]$title, specific_center_exemplars[i,]$doi), file=PATH_TO_TABLES & "table5.html", append=T)
}
write("</ul>", file=PATH_TO_TABLES & "table5.html", append=T)

######### supp table 3

print(xtable(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster"))), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptable3.html")
write.table(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster")), PATH_TO_TABLES & "supptable3.csv", sep=",", col.names=FALSE)


########## table 6 CLUSTER RULES

library(RWeka)

dat = subset(dat.research.norm.transform, (journal=="pone") & (as.numeric(year) >= 2010))
dat.for.tree.scaled = combo_and_scale(dat)
dat_for_tree_with_clusters = predict_centers(dat.for.tree.scaled, cluster_fit)
dat_for_tree_with_clusters$cluster = dat_for_tree_with_clusters$cluster_guess
table(dat_for_tree_with_clusters$cluster)

dat.for.tree.unnormalized = merge(dat.research, dat_for_tree_with_clusters[,c("doi", "cluster")], by="doi")
predictionColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011", "f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")

fit <- JRip(factor(cluster) ~ ., data=dat.for.tree.unnormalized[,append(predictionColumns, "cluster")], control = Weka_control(R = TRUE, N=50))
fit$levels = cluster_labels_simple
fit

########### table 7

evaluate_Weka_classifier(fit)
evaluate_Weka_classifier(fit)$confusionMatrix

print(xtable(evaluate_Weka_classifier(fit)$confusionMatrix, digits=0), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table7.html")
write.table(round(evaluate_Weka_classifier(fit)$confusionMatrix, 0), PATH_TO_TABLES & "table7.csv", sep=",", col.names=FALSE)


