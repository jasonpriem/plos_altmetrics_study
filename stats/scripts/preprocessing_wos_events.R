
### @export "ISI WoS 2008 PLoS cited by raw read"
library(sqldf)

### READ DATA
dat.raw.wosevents = read.csv("../data/derived/wos_plos_2008_events_raw.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
# get dat.events from preprocessing_events.R

## Look at it
dim(dat.raw.wosevents)
names(dat.raw.wosevents)

## A bit of data cleaning
dat.wosevents = dat.raw.wosevents
summary(dat.wosevents)

# set NA dates to 1
# but if no months, letting the date go to NA
dat.wosevents$dateOfCiter[which(is.na(dat.wosevents$dateOfCiter))] = 1
dat.wosevents$wosCiteDateStrings = paste(dat.wosevents$yearOfCiter, substr(dat.wosevents$monthOfCiter, 1, 3), dat.wosevents$dateOfCiter, sep="-")
dat.wosevents$wosCiteDate = strptime(dat.wosevents$wosCiteDateString, "%Y-%b-%d")
dat.wosevents$yearOfCitedTo = as.numeric(dat.wosevents$yearOfCitation)
table(dat.wosevents$yearOfCitedTo)
# The export only extracted Cited By articles of PLoS published in 2008.  
# Some other years may be included incidentally,
#   if they were cited by papers that also cited a PLoS 2008 articles.  
#   Remove cites to these Non-2008 articles because they are incomplete and therefore misleading
dat.wosevents = dat.wosevents[which(dat.wosevents$yearOfCitedTo == 2008),]
table(dat.wosevents$yearOfCitedTo)

# map to journal nicknames used in the other events
lookupJournalText = c(	"pbio", "PLOS BIO",
					"pcbi", "PLOS COM", 
					"pgen", "PLOS GEN", 
					"pmed", "PLOS MED",
					"pntd", "PLOS NEG",
					"pone", "PLOS ONE",
					"ppat", "PLOS PAT")
lookupJournal = data.frame(matrix(lookupJournalText, nrow=7, ncol=2, byrow=T, 
	dimnames=list(c(),c("journal_nickname", "journal_short"))), stringsAsFactors=F)

dat.wosevents$journal_short = substr(dat.wosevents$journalRaw, 1, 8)
dat.wosevents = merge(dat.wosevents, lookupJournal, by.X = "journalRaw", by.Y = "journal_short")
summary(dat.wosevents)
dat.wosevents$journal_nickname = factor(dat.wosevents$journal_nickname)
table(dat.wosevents$journal_nickname)


## Look again
dim(dat.wosevents)
summary(dat.wosevents)

# Need to get DOIs.  
## The ISI WoS export of all PLoS articles included the DOIs and the year and article number, so use this as a lookup
dat.doi.lookup = read.csv("../data/derived/isi_wos_counts.txt", header=TRUE, sep="\t")
names(dat.doi.lookup) = c("doi", "wosCount", "journal", "articleNumber", "year")
# Throw away all the non-2008 lookups to make it faster
table(dat.doi.lookup$year)
dat.doi.lookup = dat.doi.lookup[which(dat.doi.lookup$year==2008),]
dat.doi.lookup$journal_short = substr(dat.doi.lookup$journal, 1, 8)
dim(dat.doi.lookup)

# match to DOIs
dat.wosevents$articleNumber = paste("e", substring(dat.wosevents$id, 2), sep="")
dat.wosevents$wosCiteDate = as.POSIXct(dat.wosevents$wosCiteDate)

id.like.doi = which("10.1" == substr(dat.wosevents$id, 1, 4))
dat.wosevents$doi = NA
dat.wosevents$doi[id.like.doi] = tolower(substr(dat.wosevents$id[id.like.doi], 0, 28))
dat.merge.articleNumber = merge(dat.doi.lookup, dat.wosevents, by=c("journal_short", "articleNumber"))
dat.merge.articleNumber$doi = dat.merge.articleNumber$doi.x
dat.merge.doi = merge(dat.doi.lookup, dat.wosevents, by=c("doi"))

keep.names = c("doi", "journal_nickname", "yearOfCiter", "titleOfCiter", "wosCiteDate")
dat.wosevents.2008 = rbind(dat.merge.articleNumber[,keep.names], dat.merge.doi[,keep.names])
dat.wosevents.2008$doi = factor(tolower(substr(dat.merge$doi, 0, 28)))

write.table(dat.wosevents.2008, "../data/derived/wos_plos_2008_events_raw_doi.txt", sep="\t", row.names=F)

# Now look up the dates of all the PLoS articles to estimate legacy
dat.wosevents.2008 = read.csv("../data/derived/wos_plos_2008_events_raw_doi.txt", header=T, sep="\t", stringsAsFactors=F)
dat.eventcounts = load("../data/derived/eventcounts_preprocessed.RData")

dateLookup = data.frame(doi=dat.eventcounts$doi, pubDate=dat.eventcounts$pubDate)	
dat.wosevents.merge = merge(dat.wosevents.2008, dateLookup, by="doi")
dim(dat.wosevents.merge)
dat.wosevents.merge$latency = as.numeric(difftime(dat.wosevents.merge$wosCiteDate, dat.wosevents.merge$pubDate, units="secs"))

# Make a list of WoS events, in same format as the other alt-metrics events
dat.events.wos = data.frame(eventType="wos", doi=dat.wosevents.merge$doi, creator=NA, 
	date=dat.wosevents.merge$wosCiteDate, latency=dat.wosevents.merge$latency, value=dat.wosevents.merge$titleOfCiter, 
	stringsAsFactors=F)
dim(dat.events.wos)
summary(dat.events.wos)
str(dat.events.wos)

write.table(dat.events.wos, "../data/derived/wos_plos_2008_events.txt", sep="\t", row.names=F)



dat.events.before.wos = read.csv("../data/raw/raw_events.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE)
dat.events.wos = read.csv("../data/derived/wos_plos_2008_events.txt", header=T, sep="\t", stringsAsFactors=F)

## now consolidate all events into a single table of dois, eventsType, and total count
dat.events.wos$count = 1
#dat.wosevents.table = colwise(as.character)(dat.events.wos)
dat.events.wos$date = as.character(dat.events.wos$date)

dat.events.before.wos$latency = as.numeric(dat.events.before.wos$latency)

#dat.events.all = rbind(dat.events.wos, dat.events.where)
events.names = names(dat.events.before.wos)
dat.events.all = rbind(dat.events.wos[,events.names], dat.events.before.wos[,events.names], stringsAsFactors=F)
					
dim(dat.events.all)
summary(dat.events.all)

yearLookup = data.frame(doi=dat.eventcounts$doi, pubDate=dat.eventcounts$pubDate, stringsAsFactors=F)	
dat.events.with.date = merge(dat.events.all, yearLookup, by="doi")
dat.events.from.pubs.in.2008 = dat.events.with.date[which(substr(dat.events.with.date$pubDate, 1, 4) == "2008"),]

write.csv(dat.events.from.pubs.in.2008, "../data/derived/plos_2008_events_with_wos.txt", row.names=F)
system("gzip -f ../data/derived/plos_2008_events_with_wos.txt")

# how many dois have events
dim(table(as.character(dat.events.from.pubs.in.2008$doi)))

# what is the breakdown of events
table(dat.events.from.pubs.in.2008$eventType)





