library(Hmisc)

dat.eventcounts = read.csv("../data/derived/event_counts_research.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)

metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal.x", "articleType", "authorsCount", "journal.y", "articleNumber", "year", "pubDateVal", "title", "pmid", "plosSubjectTags", "plosSubSubjectTags")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

dat.nonzero = dat.eventcounts
dat.nonzero[,altmetricsColumns][dat.nonzero[,altmetricsColumns] > 1] = 1
summary(dat.nonzero[,altmetricsColumns])

options(scipen=100)
options(digits=2)
options(width=50)

### @export "research articles with at least one event"

apply(dat.nonzero[,altmetricsColumns], 2, mean, na.rm=T)

### @export "metrics with at least one event"

hist.nonzero = table(apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))
cbind(hist.nonzero)

### @export "plot number of metrics with at least one event"

png("{{ a.create_input_file('hist_research_nonzero_event_counts', 'png') }}", width=800, height=800)
plot(hist.nonzero/sum(hist.nonzero), main="number of articles by count of nonzero altmetric types")
dev.off()


