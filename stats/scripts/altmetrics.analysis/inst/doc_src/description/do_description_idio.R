library(altmetrics.analysis)
data(dat_research)

dat.nonzero = dat.research
dat.nonzero[,altmetricsColumns][dat.nonzero[,altmetricsColumns] > 1] = 1
summary(dat.nonzero[,altmetricsColumns])

options(scipen=100)
options(digits=2)
options(width=50)

### @export "altmetricColumns"

altmetricsColumns

### @export "articles by journal"

table(dat.research$journal.x)
round(table(dat.research$journal.x)/length(dat.research$journal.x), 2)

### @export "articles by year"

table(dat.research$journal.x, dat.research$year)

table(dat.research$year)

### @export "days since published plot"

png("hist_days_since_published.png", width=500, height=500)
hist(dat.research$daysSincePublished)
dev.off()

### @export "research articles with at least one event"

apply(dat.nonzero[,altmetricsColumns], 2, mean, na.rm=T)

### @export "metrics with at least one event"

hist.nonzero = table(apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))
cbind(hist.nonzero)

### @export "plot number of metrics with at least one event"

png("hist_research_nonzero_event_counts.png", width=500, height=500)
plot(hist.nonzero/sum(hist.nonzero), main="number of articles by count of nonzero altmetric types")
dev.off()


