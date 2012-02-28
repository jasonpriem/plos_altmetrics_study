##### create df_all and df_research

# set up
PATH_TO_RAW_DATA = "../data/raw/"
PATH_TO_DERIVED_DATA = "../data/derived/"

source("utils.R")
source("preprocessing_eventcounts_clean.R")

# read raw data    
raw_wos2010_eventcounts = read.csv(paste(PATH_TO_RAW_DATA, "raw_wos2010_eventcounts.txt.gz", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
raw_wos2011_eventcounts = read.csv(paste(PATH_TO_RAW_DATA, "raw_wos2011_eventcounts.txt.gz", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="", row.names=NULL)
raw_crawler_eventcounts = read.csv(paste(PATH_TO_RAW_DATA, "raw_crawler_eventcounts.txt.gz", sep=""), header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

# clean up data
df_wos2010_clean = clean_wos_counts(raw_wos2010_eventcounts)  
df_wos2011_clean = clean_wos_counts(raw_wos2011_eventcounts)  
df_eventcounts_clean = clean_crawler_counts(raw_crawler_eventcounts)

# do merging of Web of Science data into other indicators
# 2010
df_wos2010_merged = merge_crawler_and_wos_counts(df_eventcounts_clean, df_wos2010_clean)
names(df_wos2010_merged)[names(df_wos2010_merged)=="wosCount"] = "wosCountThru2010"
# 2011
df_wos2011_merged = merge_crawler_and_wos_counts(df_eventcounts_clean, df_wos2011_clean)
names(df_wos2011_merged)[names(df_wos2011_merged)=="wosCount"] = "wosCountThru2011"
# merge 2010 and 2011
df_merged = merge_crawler_and_wos_counts(df_wos2010_merged, subset(df_wos2011_merged, select=c(doi, wosCountThru2011)))
# save it
write.table.gzip(df_merged, PATH_TO_DERIVED_DATA, "df_all.txt")

# select research articles only
df_research = research_articles_only(df_merged)
dim(df_research)
# save it
write.table.gzip(df_research, PATH_TO_DERIVED_DATA, "df_research.txt")
