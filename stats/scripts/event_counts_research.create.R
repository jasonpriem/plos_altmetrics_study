dat.eventcounts = read.csv("../data/derived/event_counts_merged_cleaned.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)


# strip to research only 	
dat = dat.eventcounts
isResearch = which(as.character(dat.eventcounts$articleType) == "Research Article")
dat = dat.eventcounts[isResearch,]

# Write out the research data only
write.csv(dat, "../data/derived/event_counts_research.txt", row.names=F)
system("gzip ../data/derived/event_counts_research.txt")