library(altmetrics.analysis)

### @export "dat_raw_event_counts"

data(dat_raw_event_counts)
dim(dat_raw_event_counts)
names(dat_raw_event_counts)
summary(dat_raw_event_counts)

### @export "clean_crawler_counts"

dat_altmetrics_cleaned = clean_crawler_counts(dat_raw_event_counts)
dim(dat_altmetrics_cleaned)
names(dat_altmetrics_cleaned)
summary(dat_altmetrics_cleaned)

### @export "dat_raw_wos"

data(dat_raw_wos)
dim(dat.raw.wos)
names(dat.raw.wos)

### @export "clean_wos_counts"

dat.extracted.wos = clean_wos_counts(dat.raw.wos)  
dim(dat.extracted.wos)
names(dat.extracted.wos)
summary(dat.extracted.wos)

### @export "merge_crawler_and_wos_counts"

dat.merged = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos)
dim(dat.merged)
names(dat.merged)
summary(dat.merged)

### @export "research_articles_only"

dat.research = research_articles_only(dat.merged)
dim(dat.research)
names(dat.research)
summary(dat.research)
