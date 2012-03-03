#setup.eps.figure(PATH_TO_TABLES & "table4")
#close.eps.figure(PATH_TO_TABLES & "table4")


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
source("preprocessing_eventcounts_norm.R")

savePng = FALSE

### preprocess the data, if haven't done that yet

#source("create_df_research.R")
#source("create_df_research_norm_transform.R") #takes a long time


### read in all the data

raw_crawler_eventcounts = read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_eventcounts.txt.gz", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
df_research = read.csv(paste(PATH_TO_DERIVED_DATA, "df_research.txt.gz", sep=""), sep="\t")
df_research_norm_transform = read.csv(paste(PATH_TO_DERIVED_DATA, "df_research_norm_transform.txt.gz", sep=""), sep="\t")
load(paste(PATH_TO_DERIVED_DATA, "list_df_backgrounds.RData", sep=""))

raw_crawler_events = read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_events.txt.gz", sep=""), sep="\t")


########## How many papers total

source("preprocessing_eventcounts_clean.R")
df_altmetrics_cleaned = clean_crawler_counts(raw_crawler_eventcounts)
cat("Total papers with altmetrics data:", dim(df_altmetrics_cleaned)[1])

##### TABLE 1: assembled manually
#nothing

##### FIGURE 1:  Hamming window

library(signal)
window = hamming(WINDOW_WIDTH_IN_DAYS)
if (savePng) {
    png(PATH_TO_FIGURES & "figure1.png")
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure1")
}

days = seq(window)- (length(window)/2)
plot(days, window)
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure1")
}


##### TABLE 2:  Table on papers per year, per journal

library(xtable)

tablePaperDist = addmargins(table(df_research$year, df_research$journal))
print(xtable(rbind(plos_journals[colnames(tablePaperDist)], tablePaperDist), digits=0), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table2.html")
write.table(rbind(plos_journals[colnames(tablePaperDist)], round(tablePaperDist, 0)), PATH_TO_TABLES & "table2.csv", sep=",", col.names=FALSE)


######## FIGURE 2: proportion of papers with prettyAltmetricsColumns metrics, by metric

df_nonzero_indicator = df_research
df_nonzero_indicator[,altmetricsColumns][df_nonzero_indicator[,altmetricsColumns] > 1] = 1
summary(df_nonzero_indicator[,altmetricsColumns])

vector_nonzero_freq = apply(df_nonzero_indicator[,altmetricsColumns], 2, mean, na.rm=T)
vector_nonzero_freq <- vector_nonzero_freq[sort.list(vector_nonzero_freq, decreasing = T)]

prettyNames = prettyAltmetricsColumns[names(vector_nonzero_freq)]

df_nonzero_freq = data.frame(prettyNames=prettyNames, col=names(vector_nonzero_freq), freq=vector_nonzero_freq)
df_nonzero_freq$names <- factor(df_nonzero_freq$prettyNames, levels=df_nonzero_freq$prettyNames, ordered=T)

if (savePng) {
    png(PATH_TO_FIGURES & "figure2.png", width=500, height=500)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure2", width=6, height=6)
}

ggplot(df_nonzero_freq) + geom_bar(aes(names, freq)) + 
    scale_y_continuous("", formatter="percent", breaks=c(0, .2, .4, .6, .8, 1)) + 
    labs(x="") + coord_flip() + theme_bw() + opts(title = "") + cbgFillPalette
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure2")
}

print(nonzero.freq)

vector_nonzero_freq_pone = apply(subset(df_nonzero_indicator, journal=="pone", altmetricsColumns), 2, mean, na.rm=T)
vector_nonzero_freq_pone <- vector_nonzero_freq_pone[sort.list(vector_nonzero_freq_pone, decreasing = T)]

print(vector_nonzero_freq_pone)


######## FIGURE 3: NUMBER OF NONZERO METRICS PER PAPER

df_nonzero_indicator_engaged = df_nonzero_indicator
df_nonzero_indicator_engaged$num_nonzero = apply(subset(df_nonzero_indicator[,altmetricsColumns], select= -c(wosCountThru2011, almScopusCount, almPubMedCentralCount, almCrossRefCount)), 1, sum, na.rm=T)

print(summary(df_nonzero_indicator_engaged$num_nonzero))

print(cumsum(table(df_nonzero_indicator_engaged$num_nonzero)))/length(df_nonzero_indicator_engaged$num_nonzero)

if (savePng) {
    png(PATH_TO_FIGURES & "figure3.png", width=500, height=500)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure3", width=6, height=6)
}
ggplot(subset(df_nonzero_indicator_engaged, num_nonzero>=2), aes(x=num_nonzero)) + 
    geom_histogram(aes(y=..density..), binwidth=1, position="identity", breaks=1:14) + 
    labs(x="Number of engaged sources", y="Proportion of papers") + theme_bw() + 
    opts(title = "") + cbgFillPalette + cbgColourPalette
#ggplot(subset(dat.nonzero.indicator.engaged, num_nonzero>0), aes(x=num_nonzero, fill=year)) + geom_density(aes(y=..density..), alpha=0.5, adjust=4) + labs(x="Number of engaged sources", y="Density") + theme_bw() + opts(title = "") + cbgFillPalette
#ggplot(subset(dat.nonzero.indicator.engaged, num_nonzero>0), aes(x=num_nonzero)) + geom_freqpoly(aes(y=..density.., color=year), alpha=0.5, binwidth=1, position="identity") + labs(x="Number of engaged sources", y="Number of papers") + theme_bw() + opts(title = "") + cbgFillPalette + cbgColourPalette
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure3")
}

#### Figure 4:
#created elsewhere

########## FIGURE 5: EVENT CREATION BY CREATOR


raw_crawler_events$pubDate = strptime(raw_crawler_events$date, "%Y-%m-%dT")

# restrict events to those on research articles
raw_crawler_eventcounts_research <- raw_crawler_eventcounts[raw_crawler_eventcounts$articleType=="Research Article", ]
raw_crawler_events <- raw_crawler_events[raw_crawler_events$doi %in%  raw_crawler_eventcounts_research$doi,]

# column for which journal
raw_crawler_events$journal <- substr(raw_crawler_events$doi, 17, 20)

# get the indicators with event dates
df_events = subset(raw_crawler_events, eventType %in% c("citeulike","delicious", "backtweets"))

# frame for just Plos ONE, where we have the most data
df_events_pone <- df_events[df_events$journal == "pone",]
df_events_pone$doi <- factor(df_events_pone$doi[1:nrow(df_events_pone)]) #drop unused levels
df_events_pone$creator <- factor(df_events_pone$creator[1:nrow(df_events_pone)])

# remove events with negative latency
nrow(df_events_pone[df_events_pone$latency <= 0,]) / nrow(df_events_pone) # 2%
df_events_pone = df_events_pone[df_events_pone$latency > 0,]

# check out the distributions (as expected, mostly power-law)
#creators <- ddply(bt.pone, .(creator, eventType), summarize, length(unique(creator)))

df_event_creators = data.frame(creator=NULL, num_creations=NULL, eventType=NULL)
for (eType in c("citeulike","delicious", "backtweets")) {
    df_events_pone_eType = subset(df_events_pone, eventType==eType)
    creators <- rev(sort(table(df_events_pone_eType$creator)))
    df_event_creators = rbind(df_event_creators, data.frame(creator=names(creators), num_creations=creators, eventType=eType))
#plot(creators, log="xy")
}

if (savePng) {
    png(PATH_TO_FIGURES & "figure5.png", width=600, height=300)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure5", width=6, height=3)
}
ggplot(data=df_event_creators, aes(x=num_creations)) + 
    geom_histogram(binwidth=0.35, position="identity") + 
    scale_x_log10(formatter="comma", breaks=c(0, 1, 10, 100, 1000)) + 
    scale_y_log10(formatter="comma", breaks=c(0, 1, 10, 100, 1000)) + 
    labs(x="number of events", y="number of distinct event creators") + 
    theme_bw() + cbgFillPalette + facet_grid(~ eventType)
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure5")
}

############ FIGURE 6:   EVENTS OVER TIME, BY METRIC

df_events = raw_crawler_events

# remove tweets with negative latency or that are more than five years old (a few weird outliers)
nrow(df_events[df_events$latency <= 0,]) / nrow(df_events) 
df_events = df_events[df_events$latency > 0,]
df_events = df_events[df_events$latency < (60*60*24*365*5),]

df_events_by_week = data.frame(week=NULL, num_events=NULL, eventType=NULL)
for (eType in c("html views", "pdf views", "native comments", "citeulike", "backtweets", "delicious")) {
    df_events_by_week_eType = subset(df_events, eventType==eType)
    df_events_by_week_eType$latency_round = round(df_events_by_week_eType$latency/(60*60*24*7), 0)
    df_events_by_week_eType = ddply(df_events_by_week_eType, .(latency_round), summarise, total=sum(count))
    df_events_by_week = rbind(df_events_by_week, data.frame(week=df_events_by_week_eType$latency_round, num_events=df_events_by_week_eType$total, eventType=prettyETypeColumns[eType]))
}


if (savePng) {
    png(PATH_TO_FIGURES & "figure6.png", width=600, height=500)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure6", width=6, height=5)
}
ggplot(data=df_events_by_week, aes(x=week, y=num_events, color=eventType)) + 
#    geom_point(aes(alpha=0.5)) + 
    geom_point(size=1) + 
    scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels=c(1, 10, 100, 1000, 10000, 100000)) + 
    scale_x_continuous(breaks=c((1:5)*52)) + 
    labs(x="weeks since publication", y="total events each week") + 
    theme_bw() + cbgColourPalette + cbgFillPalette + 
    geom_smooth(span=0.3) 
#    + scale_alpha(legend = FALSE)
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure6")
}



############  FIGURE 7:  average metric values vs publication date, by journal

journals = names(list_df_backgrounds)
yrange = get_ranges(df_research, altmetricsColumns)

cols = altmetricsColumns
df_research$pubDateVal = strptime(df_research$pubDate, "%Y-%m-%d")
df_research$pubDateVal = as.POSIXct(df_research$pubDateVal)
xrange = range(df_research$pubDateVal)

if (savePng) {
    png(PATH_TO_FIGURES & "figure7.png", width=800, height=800)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure7", width=MAX.FIGURE.WIDTH, height=MAX.FIGURE.WIDTH)
}

#quartz()
par(mfrow = c(ceiling(length(altmetricsColumns)/4), 4), oma=c(2,2,4,2), mar=c(3, 2, 1.5, 2))
for (col in cols) {
	i=0
	allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
	plot(xrange, allrange, type="n", main=prettyAltmetricsColumns[col], cex.main=0.75)
	
	for (journal in journals) {
		i = i+1
		inJournal = which(df_research$journal==journal)
		journal.background = list_df_backgrounds[[journal]]
        journal.background$pubDateVal = strptime(journal.background$pubDate, "%Y-%m-%d")
        journal.background$pubDateVal = as.POSIXct(journal.background$pubDateVal)
		#quartz()		
		lines(journal.background[,"pubDateVal"], journal.background[,col], col=cbgRaw[i], lwd=2)
	}
}
#plot(1)
legend("left", journals, col = cbgRaw[1:7], lty=1, bty="n", fill=cbgRaw[1:7], cex=0.65)
#title(paste("Trends over time per journal"), outer=TRUE)
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure7")
}


###### FIGURE 8
# figure created previously, code not avail

####### FIGURE 9: Correlations using all data, normalized

#source("events_correlations_plot.R")
library(ggplot2)
library(reshape2)
library(ggdendro)


mycor = calc.correlations(df_research_norm_transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")
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

if (savePng) {
    png(PATH_TO_FIGURES & "figure9.png", width=600, height=600)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure9", width=6, height=6)
}

ggplot(mycor_melted, aes(col, variable)) + geom_tile(aes(fill = value)) + 
     scale_fill_gradient(low = "white", high = "black", space="Lab") + 
     theme_grey(base_size = 9) + labs(x = "", y = "") + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 10, angle = 270, hjust = 0, colour = "black"), 
     axis.text.y = theme_text(size = 10, colour = "black", hjust=1)) + 
     scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))

if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure9")
}


############# TABLE 3:  Factor results

library(psych)

mycor = calc.correlations(df_research_norm_transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")
colnames(mycor) = prettyAltmetricsColumns[colnames(mycor)]
rownames(mycor) = prettyAltmetricsColumns[rownames(mycor)]

fa.results = fa(mycor, 6, fm="minres", rotate="promax", 
                 residuals=TRUE, n.obs=max(dim(df_research_norm_transform)))

colnames(fa.results$loadings) = c("citations", "pageviews and shares", "facebook-hosted discussion", "plos-hosted comments", "social ref saves", "pdf downloads")

fa.results$loadings
print(fa.results$loadings, digits=2, cutoff=0)

loadings_raw = matrix(fa.results$loadings, dim(fa.results$loadings)[1], dim(fa.results$loadings)[2])
rownames(loadings_raw) = rownames(mycor)
colnames(loadings_raw) = colnames(fa.results$loadings)

print(xtable(loadings_raw, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table3.html")
write.table(loadings_raw, PATH_TO_TABLES & "table3.csv", sep=",", col.names=FALSE)


#   Factor results weights
fa.results$weights
colnames(fa.results$weights) = colnames(fa.results$loadings)

print(xtable(fa.results$weights, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptable2.html")
write.table(fa.results$weights, PATH_TO_TABLES & "supptable2.csv", sep=",", col.names=TRUE)


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

if (savePng) {
    png(PATH_TO_FIGURES & "figure10.png", width=600, height=600)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure10", width=6, height=6)
}

ggplot(fa.cor.melted, aes(col, variable)) + geom_tile(aes(fill = value+1),
     colour = "white") + scale_fill_gradient2(low = "grey50", mid="white", high = "steelblue", trans="log", midpoint=0) + 
     theme_grey(base_size = 12) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 270, hjust = 0, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) + 
     geom_text(aes(x=col,y=variable, label=sprintf("%.1f", value)), data=fa.cor.melted, size=3, colour="black")

if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure10")
}

#fa.diagram(fa.results, simple=T, cex=.5)
    
    
############ Table 4: TABLE WITH CORRELATIONS TO CITATIONS

library(plyr)

create_journal_correlation_table = function(spearman_2010=TRUE, tablename) {
    
    dat = c()
    for (myjournal in c("pbio", "ppat", "pone")) {
        cat("\n", myjournal)
    
        if (spearman_2010) {
            dat_journal2010 = subset(df_research, journal==myjournal)
            dat_journal2010 = subset(dat_journal2010, year=="2010")
            mycor = calc.correlations(dat_journal2010[, altmetricsColumns], "pairwise.complete.obs", "spearman")
            dat = cbind(mycor[,"wosCountThru2011"], dat)
            colnames(dat)[1] = paste(myjournal, " 2010 (n=", dim(dat_journal2010)[1], ")", sep="")
        } else {
            dat_journal = subset(df_research_norm_transform, journal==myjournal)
            mycor = calc.correlations(dat_journal[, altmetricsColumns], "pairwise.complete.obs", "pearson")
            dat = cbind(mycor[,"wosCountThru2011"], dat)
            colnames(dat)[1] = paste(myjournal, " (n=", dim(dat_journal)[1], ")", sep="")
        }
    
    }

    print(round(dat[sort.list(dat[,1], dec=T),], 2))

    # Based on example at http://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
    rownames(dat) = prettyAltmetricsColumns[rownames(dat)]

    dat_melted = melt(dat)
    dat_melted$X2 <- factor(dat_melted$X2, levels(dat_melted$X2)[c(2, 3, 1)], ordered=T)
    dat_melted$X1 <- factor(dat_melted$X1, levels(dat_melted$X1)[sort.list(dat[levels(dat_melted$X1),1], dec=F)], ordered=T)

    if (savePng) {
        png(PATH_TO_FIGURES & tablename & ".png", width=600, height=500)
    } else {
        setup.eps.figure(PATH_TO_FIGURES & tablename, width=6, height=5)
    }
    
    p = ggplot(dat_melted, aes(X2, X1)) + 
        geom_tile(aes(fill = value), colour = "white") + 
        scale_fill_gradient(low = "white", high = "steelblue") + 
         theme_grey(base_size = 12) + labs(x = "", y = "") + 
         scale_x_discrete(expand = c(0, 0)) + 
         scale_y_discrete(expand = c(0, 0)) + 
         opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 340, hjust = 0.5, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) + 
         geom_text(aes(x=X2,y=X1, label=sprintf("%.1f", value)), data=dat_melted, size=4, colour="black")
    print(p)
    if (savePng) {
        dev.off()
    } else {
        close.eps.figure(PATH_TO_FIGURES & tablename)
    }

    print(xtable(dat, digits=2), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & tablename & ".html")
    write.table(dat, PATH_TO_FIGURES & tablename & ".csv", sep=",", col.names=FALSE)
}

create_journal_correlation_table(FALSE, "figure11")
create_journal_correlation_table(TRUE, "figure12")

########## Figure 11:  PLOT CLUSTER CENTERS

clusterColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011","f1000Factor")
clusterColumns = append(clusterColumns, "shareCombo")

prettyClusterNames = c("HTML pageviews", "Mendeley saves", "Web of Science cites", "F1000 rating", "Sharing combo")
names(prettyClusterNames) = clusterColumns

combo_and_scale = function(dat) {
    #dat = subset(df_research_norm_transform, (journal=="pone") & (year==2010))
    dat.for.cluster.unscaled = dat

    dat.for.cluster.unscaled$shareCombo = with(dat.for.cluster.unscaled, facebookShareCount + deliciousCount + almBlogsCount + backtweetsCount)

    dat.for.cluster.unscaled = dat.for.cluster.unscaled[complete.cases(dat.for.cluster.unscaled[,clusterColumns]),]
    dat.for.cluster = dat.for.cluster.unscaled
    dat.for.cluster[,clusterColumns] = scale(dat.for.cluster.unscaled[,clusterColumns])
    dim(dat.for.cluster)
    summary(dat.for.cluster[,clusterColumns])
    return(dat.for.cluster)
}

dat = subset(df_research_norm_transform, (journal=="pone") & (as.numeric(year) < 2010))
dat.for.cluster = combo_and_scale(dat)
#scree_plot_for_number_clusters(dat.for.cluster[,clusterColumns])
#title("PLoS ONE (2007-2009)")

NUMBER.CLUSTERS = 5
### DID TRY THIS MORE TIMES TO VERIFY THIS SEED GETS THE LOWEST WSS RESULT AT THE END
for (i in seq(0:1)){
    set.seed(44)
    cluster_fit = kmeans(dat.for.cluster[,clusterColumns], NUMBER.CLUSTERS, iter.max=100)
    dat_with_cluster_assignments <- data.frame(dat.for.cluster, cluster=cluster_fit$cluster)
    #plot_cluster_centers(cluster_fit, prettyClusterNames)
    
    cluster_labels_simple = paste("cluster ", LETTERS[1:length(cluster_fit$size)], sep="")
    cluster_labels = paste("cluster ", LETTERS[1:length(cluster_fit$size)], " (", round(100*cluster_fit$size/sum(cluster_fit$size), 0), "%)", sep="")
    metric_labels = prettyClusterNames[colnames(cluster_fit$centers)]

    sorted_size = sort.list(cluster_fit$size, decr=T)
    print(cluster_labels[sorted_size])
    print(rbind(t(round((cluster_fit$centers)[sorted_size,clusterColumns], 1)), percent=round((cluster_fit$size)[sorted_size]/sum(cluster_fit$size), 2)))
    print(cluster_fit$tot.withinss)
}

center_names = paste("cluster ", c("C", "D", "E", "B", "A"), sep="")
rownames(cluster_fit$centers) = paste("cluster ", c("C", "D", "E", "B", "A"), sep="")
a = t(round((cluster_fit$centers)[sorted_size,clusterColumns], 1))

# To match the labels I gave them initially
#colnames(a) = paste("cluster ", c("E", "B", "A", "D", "C"), sep="")


#round(t(prop.table(table(dat_with_cluster_assignments$cluster, dat_with_cluster_assignments$year), 2)), 2)
#round(t(prop.table(table(dat_with_cluster_assignments$cluster, cut(dat_with_cluster_assignments$authorsCount, c(0, 2, 5, 10, 200))), 2)), 2)


if (savePng) {
    png(PATH_TO_FIGURES & "figure13.png", width=800, height=200)
} else {
    setup.eps.figure(PATH_TO_FIGURES & "figure13", width=MAX.FIGURE.WIDTH, height=2)
}

ggplot(melt(a), aes(X2, X1, width=rep(1.5*(cluster_fit$size[sorted_size])/(max(cluster_fit$size)),each=5))) + geom_tile(aes(fill = value+1), colour = "white") + 
    scale_fill_gradient2(low = "grey50", mid="white", high = "steelblue", trans="log", midpoint=0) + 
     theme_grey(base_size = 12) + labs(x = "", y = "") + 
     scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + 
     opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = 12, angle = 340, hjust = 0.5, colour = "black"), axis.text.y = theme_text(size = 12, hjust = 1, colour = "black")) +
     geom_text(aes(x=X2,y=X1, label=sprintf("%.1f", value)),data=melt(a), size=3, colour="black")
if (savePng) {
    dev.off()
} else {
    close.eps.figure(PATH_TO_FIGURES & "figure13")
}

########## supp info TABLES OF CORRELATES WITH CLUSTERS

print(xtable(a, digits=1), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptableA.html")
write.table(a, PATH_TO_TABLES & "supptableA.csv", sep=",", col.names=FALSE)


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

print(xtable(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster"))), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptableB.html")
write.table(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster")), PATH_TO_TABLES & "supptableB.csv", sep=",", col.names=FALSE)

write("<ul>", file=PATH_TO_TABLES & "table5.html", append=F)
for (i in seq(specific_center_exemplars[,1])) {
    write(sprintf("<li>%s, <a href='http://dx.doi.org/%s'>%s</a> (<a href='http://www.plosone.org/article/metrics/info:doi/%s'>metrics</a>)", center_names[specific_center_exemplars[i,]$cluster], specific_center_exemplars[i,]$doi, specific_center_exemplars[i,]$title, specific_center_exemplars[i,]$doi), file=PATH_TO_TABLES & "table5.html", append=T)
}
write("</ul>", file=PATH_TO_TABLES & "table5.html", append=T)

######### supp table 3

print(xtable(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster"))), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "supptableC.html")
write.table(subset(specific_center_exemplars, TRUE, c("title", "doi", "cluster")), PATH_TO_TABLES & "supptableC.csv", sep=",", col.names=FALSE)


########## table 6 CLUSTER RULES

library(RWeka)

dat = subset(df_research_norm_transform, (journal=="pone"))
dat = subset(dat, (as.numeric(year) >= 2010))
dat.for.tree.scaled = combo_and_scale(dat)
dat_for_tree_with_clusters = predict_centers(dat.for.tree.scaled, cluster_fit)
dat_for_tree_with_clusters$cluster = dat_for_tree_with_clusters$cluster_guess
table(dat_for_tree_with_clusters$cluster)

dat.for.tree.unnormalized = merge(df_research, dat_for_tree_with_clusters[,c("doi", "cluster")], by="doi")
predictionColumns = c("htmlDownloadsCount","mendeleyReadersCount","wosCountThru2011", "f1000Factor","wikipediaCites", "facebookShareCount", "deliciousCount", "almBlogsCount", "backtweetsCount")

fit <- JRip(factor(cluster) ~ ., data=dat.for.tree.unnormalized[,append(predictionColumns, "cluster")], control = Weka_control(R = TRUE, N=50))
fit$levels = center_names
fit

########### table 7

evaluate_Weka_classifier(fit)
evaluate_Weka_classifier(fit)$confusionMatrix

print(xtable(evaluate_Weka_classifier(fit)$confusionMatrix, digits=0), type="html", html.table.attributes = "border = '0'", file=PATH_TO_TABLES & "table7.html")
write.table(round(evaluate_Weka_classifier(fit)$confusionMatrix, 0), PATH_TO_TABLES & "table7.csv", sep=",", col.names=FALSE)


