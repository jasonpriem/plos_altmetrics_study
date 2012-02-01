#PATH_TO_DERIVED_DATA = "./datasets/"
PATH_TO_DERIVED_DATA = "../data/derived/"
load(paste(PATH_TO_DERIVED_DATA, "dat_research.RData", sep=""))

options(scipen=100)
options(digits=2)
options(width=50)

### @export "altmetricColumns"

altmetricsColumns

### @export "articles by journal"

table(dat.research$journal)
round(table(dat.research$journal)/length(dat.research$journal), 2)

### @export "articles by year"

addmargins(table(dat.research$year, dat.research$journal))

table(dat.research$year)
table(dat.research$journal)

#colourblind friendly palettes from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)
cbgFillPalette <- scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
cbgColourPalette <- scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

png("img/stackedjournalhist.png", width=500, height=500)
ggplot(dat.research, aes(x=year)) + stat_bin(aes(y=..count.., fill=journal)) + cbgFillPalette
#ggplot(dat.research, aes(x=year)) + stat_bin(aes(y=..count.., fill=journal)) + scale_fill_brewer(palette="Set1")
dev.off()



### @export "days since published plot"

png("img/hist_days_since_published.png", width=500, height=500)
hist(dat.research$daysSincePublished)
dev.off()

### @export "research articles with at least one event"

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

### @export "metrics with at least one event"

hist.nonzero = table(apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))
cbind(hist.nonzero)

### @export "plot number of metrics with at least one event"

png("img/hist_research_nonzero_event_counts.png", width=500, height=500)
plot(hist.nonzero/sum(hist.nonzero), main="how many different non-zero metrics do papers receive?")
dev.off()

hist.nonzero.by.year = table(dat.nonzero$year, apply(dat.nonzero[,altmetricsColumns], 1, sum, na.rm=T))
nonzero.by.year = melt(hist.nonzero.by.year)
names(nonzero.by.year) = c("year", "num_metrics", "histcount")
png("img/hist_research_nonzero_event_counts_by_year.png", width=500, height=500)
qplot(num_metrics, histcount, data=nonzero.by.year, geom="bar", stat="identity") + facet_grid(year ~ .) + cbgFillPalette
dev.off()