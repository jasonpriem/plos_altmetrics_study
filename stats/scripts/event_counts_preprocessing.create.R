#library(Rserve)
#Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA

#workarounds while zipped to get into gz
system("unzip ../data/derived/event_counts_altmetrics_cleaned.txt.zip")
system("gzip ../data/derived/event_counts_altmetrics_cleaned.txt")
dat.raw.eventcounts = read.csv("../data/raw/raw_event_counts.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE)

source("event_counts_altmetrics_cleaned.create.R")
a = event_counts_altmetrics_cleaned(raw_event_counts)
#R --no-save < event_counts_altmetrics_cleaned.create.R > log.out 2>&1

write.csv(dat.eventcounts, "../data/derived/event_counts_altmetrics_cleaned.txt", row.names=F)
system("gzip ../data/derived/event_counts_altmetrics_cleaned.txt")


#python -u events_raw_wos_2008.create.py >> log.out
#cp ../data/derived/events_raw_wos_2008.txt.gz ../data/raw/events_raw_wos_2008.txt.gz
#R --no-save < event_counts_wos_extracted.create.R >> log.out 2>&1
#R --no-save < event_counts_merged_cleaned.create.R >> log.out 2>&1
#R --no-save < event_counts_research.create.R >> log.out 2>&1
#R --no-save < event_counts_normalized.create.R >> log.out 2>&1
#R --no-save < corr_pearson_normalized.create.R >> log.out 2>&1
#R --no-save < event_counts_factor_scores.create.R >> log.out 2>&1

