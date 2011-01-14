library(Hmisc)

### @export "read data"

dat.eventcounts.nowos = read.csv("../data/derived/event_counts_altmetrics_cleaned.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)
#load("../data/derived/eventcounts_preprocessed.RData")

# include ISI WoS data 
dat.wos = read.csv("../data/derived/event_counts_wos_extracted.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)
colnames(dat.wos) = c("doi","wosCount","journal","articleNumber","year")
summary(dat.wos)

# Merge with eventcounts
dat.eventcounts = merge(dat.eventcounts.nowos, dat.wos, by.x="doi", by.y="doi", all.x=T)

# Write out the merged data
write.csv(dat.eventcounts, "../data/derived/event_counts_merged_cleaned.txt", row.names=F)
system("gzip ../data/derived/event_counts_merged_cleaned.txt")