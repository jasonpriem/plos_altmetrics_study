library(Hmisc)

# Placeholder content.  Will make more browseable soon.

metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal", "articleType", "authorsCount")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

dat.metricIsUsed = dat.eventcounts

dat.metricIsUsed[,altmetricsColumns][dat.metricIsUsed[,altmetricsColumns] > 1] = 1
summary(dat.metricIsUsed[,altmetricsColumns])

options(scipen=100)
options(digits=2)
options(width=100)

# Metrics by Average number of articles with at least one event
apply(dat.metricIsUsed[,altmetricsColumns], 2, mean, na.rm=T)

# Number of articles by how many metrics they have with at least one event
hist.has.events = table(apply(dat.metricIsUsed[,altmetricsColumns], 1, sum, na.rm=T))
cbind(hist.has.events)
pdf("results/num_articles_nonzero_event_counts.pdf")
plot(hist.has.events/sum(hist.has.events), main="number of articles by count of nonzero altmetric types")
dev.off()

# Look at the distributions
for (col in altmetricsColumns) {
	#pdf(paste("results/hist_has_events/", col, ".pdf", sep=""))
	quartz()
	par(mfrow = c(2, 1))
	titletext = paste(col, "\nnot normalized by pubdate", sep="")
	hist(dat.eventcounts[,col], breaks=50, main=titletext)
	hist(tr(dat.eventcounts[,col]), breaks=50, main=paste("sqrt(1+", col, ")", "\nnot normalized by pubdate", sep=""))
	#dev.off()
}


### Write out to examine in other programs
write.csv(dat.metricIsUsed, "data/derived/all_metrics_used.csv")



