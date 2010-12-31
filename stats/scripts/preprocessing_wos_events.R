#library(Rserve)
#Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.wosevents = read.csv("../data/derived/wos_2008_events_raw.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")
# get dat.events from preprocessing_events.R

## Look at it
dim(dat.raw.wosevents)
names(dat.raw.wosevents)
summary(dat.raw.wosevents)

## A bit of data cleaning
dat.wosevents = dat.raw.wosevents
summary(dat.wosevents)

lookupJournalText = c(	"pbio", "PLOS BIO",
					"pcbi", "PLOS COM", 
					"pgen", "PLOS GEN", 
					"pmed", "PLOS MED",
					"pntd", "PLOS NEG",
					"pone", "PLOS ONE",
					"ppat", "PLOS PAT")
lookupJournal = data.frame(matrix(lookupJournalText, nrow=7, ncol=2, byrow=T, 
	dimnames=list(c(),c("journal_abbrev", "journal_short"))), stringsAsFactors=F)

dat.wosevents$journal_short = substr(dat.wosevents$journalRaw, 1, 8)
dat.wosevents = merge(dat.wosevents, lookupJournal, by.X = "journalRaw", by.Y = "journal_short")
summary(dat.wosevents)
dat.wosevents$journal_abbrev = factor(dat.wosevents$journal_abbrev)
table(dat.wosevents$journal_abbrev)

# set NA dates to 1
# but if no months, letting the date go to NA
dat.wosevents$dateOfCiter[which(is.na(dat.wosevents$dateOfCiter))] = 1
dat.wosevents$wosCiteDateStrings = paste(dat.wosevents$yearOfCiter, substr(dat.wosevents$monthOfCiter, 1, 3), dat.wosevents$dateOfCiter, sep="-")
dat.wosevents$wosCiteDate = strptime(dat.wosevents$wosCiteDateString, "%Y-%b-%d")

dat.wosevents$yearOfCitation = as.numeric(dat.wosevents$yearOfCitation)

## Look again
summary(dat.wosevents)

# Get PLoS articles to look up dois
dat.wos = read.csv("../data/raw/isi_wos_counts.txt", header=TRUE, sep="\t")
names(dat.wos) = c("doi", "wosCount", "journal", "articleNumber", "year")

dat.wosevents.doi = dat.wosevents[which(dat.wosevents$yearOfCitation == 2008),]
dat.wosevents.doi$doi = NA
# match to DOIs
for (i in 1:length(dat.wosevents.doi$id)) {
	if ("10.1" == substr(dat.wosevents.doi$id[i], 1, 4)) {
		dat.wosevents.doi$doi[i] = dat.wosevents.doi$id[i]
#	} else if ("e" != substr(dat.wosevents.2008.doi$id[i], 1, 1)) {
	} else {
		hits = which(dat.wos$articleNumber == paste("e", substring(dat.wosevents$id[i], 2), sep="") & 
			dat.wos$year == dat.wosevents.doi$yearOfCitation[i] &
			substr(dat.wos$journal, 1, 8) == dat.wosevents.doi$journal_short[i])
		if (length(hits > 0)) {
			dat.wosevents.doi$doi[i] = as.character(dat.wos$doi[hits[1]])
		} else {
			dat.wosevents.doi$doi[i] = NA
		}
	}
	#print(paste(i, dat.wosevents.doi$doi[i]))
	#print(dat.wosevents.doi$doi[i])
}

# make lowercase and strip off stuff at the end
dat.wosevents.doi$doi = tolower(substr(dat.wosevents.doi$doi, 0, 28))

write.table(dat.wosevents.doi, "../data/derived/wos_2008_events_doi.txt", sep="\t", row.names=F)


dat.wosevents.2008 = read.csv("../data/derived/wos_2008_events_doi.txt", header=T, sep="\t", stringsAsFactors=F)


dateLookup = data.frame(doi=dat.eventcounts$doi, pubDate=dat.eventcounts$pubDate)	
dat.wosevents.merge = merge(dat.wosevents.2008, dateLookup, by="doi")
dim(dat.wosevents.merge)
dat.wosevents.merge$latency = as.numeric(difftime(dat.wosevents.merge$wosCiteDate, dat.wosevents.merge$pubDate, units="secs"))
dat.wosevents.table = data.frame(eventType="wos", doi=dat.wosevents.merge$doi, creator=NA, 
	date=dat.wosevents.merge$wosCiteDate, latency=dat.wosevents.merge$latency, value=dat.wosevents.merge$titleOfCiter, 
	stringsAsFactors=F)
dim(dat.wosevents.table)
summary(dat.wosevents.table)
str(dat.wosevents.table)

write.table(dat.wosevents.table, "../data/derived/wos_2008_events.txt", sep="\t", row.names=F)



dat.events.no.wos = read.csv("../data/derived/dat_events_numevents.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
dat.wosevents.table = read.csv("../data/derived/wos_2008_events.txt", header=T, sep="\t", stringsAsFactors=F)

## now consolidate these events into a single table of dois, eventsType, and total count
dat.wosevents.table$number.events = 1
#dat.wosevents.table = colwise(as.character)(dat.wosevents.table)
dat.wosevents.table$date = as.character(dat.wosevents.table$date)

dat.events.no.wos$latency = as.numeric(dat.events.no.wos$latency)

#dat.events.all = rbind(dat.wosevents.table, dat.events.where)
dat.events.all = data.frame(doi=c(dat.wosevents.table$doi, dat.events.no.wos$doi), 
	eventType=c(dat.wosevents.table$eventType, dat.events.no.wos$eventType),
	number.events=c(dat.wosevents.table$number.events, dat.events.no.wos$number.events), 
	latency=c(dat.wosevents.table$latency, dat.events.no.wos$latency), stringsAsFactors=F)
						
dim(dat.events.all)
summary(dat.events.all)

yearLookup = data.frame(doi=dat.eventcounts$doi, year=dat.eventcounts$year, stringsAsFactors=F)	
dat.events.with.date = merge(dat.events.all, yearLookup, by="doi")
dat.events.from.pubs.in.2008 = dat.events.with.date[which(dat.events.with.date$year == 2008),]

write.table(dat.events.from.pubs.in.2008, "../data/derived/all_2008_events.txt", sep="\t", row.names=F)








