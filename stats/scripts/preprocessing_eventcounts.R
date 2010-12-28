#library(Rserve)
#Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.eventcounts = read.csv("data/raw/event_counts.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.eventcounts)
names(dat.raw.eventcounts)
summary(dat.raw.eventcounts)

## A bit of data cleaning
dat.eventcounts = dat.raw.eventcounts

## Make sure all data has good DOIs, detects rogue line breaks etc.
hasGoodDoi = "10." == substr(dat.raw.eventcounts$doi, 1, 3)
summary(hasGoodDoi)
dat.eventcounts[!hasGoodDoi,]

# Now create a date type variable
dat.eventcounts$pubDate  = strptime(dat.eventcounts$pubDate, "%Y-%m-%dT")
summary(dat.eventcounts$pubDate)

# Create a column that has days since published
dat.eventcounts$daysSincePublished = as.integer(difftime(max(dat.eventcounts$pubDate), dat.eventcounts$pubDate, units="days"))
hist(dat.eventcounts$daysSincePublished)

## Adjust some fields to they are the right datatype.  

# Change journal strings to factors
dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)

# Change f1000Factor strings to integer counts.  "false" means count of 0.
dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0

# Change wikipediaCites NAs to 0s
dat.eventcounts$wikipediaCites[is.na(dat.eventcounts$wikipediaCites)] = 0

# Change mendeleyReadersCount NAs to 0s
dat.eventcounts$mendeleyReadersCount[is.na(dat.eventcounts$mendeleyReadersCount)] = 0

# Change facebookClickCount NAs to 0s
dat.eventcounts$facebookClickCount[is.na(dat.eventcounts$facebookClickCount)] = 0

# delicious count looks strange for now
dat.eventcounts$deliciousCount = as.integer(dat.raw.eventcounts$deliciousCount)
dat.eventcounts$deliciousCount[is.na(dat.eventcounts$deliciousCount)] = 0
dat.eventcounts$deliciousCount[dat.eventcounts$deliciousCount > 1000000] = 1

# rename PMC column
dat.eventcounts$almPubMedCentralCount = dat.eventcounts$almPubMedCount
dat.eventcounts$almPubMedCount = NULL

# There are a few Facebook results from Facebook API with negative numbers
# Not clear what this means (not in Facebook API docs), so setting to NA
facebookColumns = c("facebookShareCount", "facebookLikeCount", "facebookCommentCount", "facebookClickCount")
for (col in facebookColumns) {
	dat.eventcounts[which(dat.eventcounts[, col] < 0), col] = NA	
}

## article Type, set NAs to "Research Article" 
# and store as a factor
dat.eventcounts$articleType[is.na(dat.raw.eventcounts$articleType)] = "Research Article" 
dat.eventcounts$articleType = factor(dat.eventcounts$articleType)
summary(dat.eventcounts$articleType)

## authorsCount
dat.eventcounts$authorsCount = as.numeric(dat.raw.eventcounts$authorsCount)

## eliminate columns not in use right now
dat.eventcounts$pmid = NULL
dat.eventcounts$plosSubjectTags = NULL
dat.eventcounts$plosSubSubjectTags = NULL


## Look again
summary(dat.eventcounts)


