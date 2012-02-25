##### create dat.research
    
source("counts_clean_merge_filter.R")
    
dat_raw_wos_2010 = read.csv("../data/raw/raw_wos_2010.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
dat_raw_wos_2011 = read.csv("../data/raw/raw_wos_2011.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="", row.names=NULL)
dat_raw_event_counts = read.csv("../data/raw/raw_event_counts.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

dat_altmetrics_cleaned = clean_crawler_counts(dat_raw_event_counts)
dat.extracted.wos.2010 = clean_wos_counts(dat_raw_wos_2010)  
dat.extracted.wos.2011 = clean_wos_counts(dat_raw_wos_2011)  

dat.merged.2010 = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos.2010)
names(dat.merged.2010)[names(dat.merged.2010)=="wosCount"] = "wosCountThru2010"
dat.merged.2011 = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos.2011)
names(dat.merged.2011)[names(dat.merged.2011)=="wosCount"] = "wosCountThru2011"
dat.merged.2010.2011 = merge_crawler_and_wos_counts(dat.merged.2010, subset(dat.merged.2011, select=c(doi, wosCountThru2011)))
dat.merged = dat.merged.2010.2011

dat.research = research_articles_only(dat.merged)
dim(dat.research)
save(dat.research, file = "../data/derived/dat_research.RData", compress="gzip")
