library(altmetrics.analysis)

### @export "altmetricsColumns"

altmetricsColumns

### @export "get_research_articles"

data(dat_raw_event_counts)
data(dat_raw_wos)
dat.research = get_research_articles(dat_raw_event_counts, dat.raw.wos)
dim(dat.research)
names(dat.research)
summary(dat.research)

### @export "WINDOW_WIDTH_IN_DAYS"
WINDOW_WIDTH_IN_DAYS

### @export "hamming"

library(signal)
window = hamming(WINDOW_WIDTH_IN_DAYS)
png("hamming.png")
days = seq(window)- length(window)/2
plot(days, window)
dev.off()

### @export "transformation"

transformation_function