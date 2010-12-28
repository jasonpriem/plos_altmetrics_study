#library(Rserve)
#Rserve(args="--no-save")

###### events.txt
# file has one row for each event.
# in some cases, an event contains a count of occurances that day in the "values" column

## READ DATA
dat.raw.events = read.csv("data/raw/events.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

## Look at it
dim(dat.raw.events)
names(dat.raw.events)
summary(dat.raw.events)

## Now adjust because some events include multiple occurances
## add a new column called number.events that is ususally 1
## but is set to the number of occurances in the "value" column 
### for datatypes with more than one occurance per row
dat.events = dat.raw.events
number.rows = table(dat.events$eventType)
eventTypes = names(number.rows)
# Assign the contents of value as the number of occurances
dat.events$number.events = as.integer(dat.events$value)		
# But for many datatypes the value content is actually a text string.  
# for these, overwrite with the number of occurances with 1.
events.with.individual.rows = names(number.rows[number.rows < max(number.rows)])
for (myEventType in events.with.individual.rows) {
	dat.events$number.events[dat.events$eventType == myEventType] = 1
}

## now consolidate these events into a single table of dois, eventsType, and total count
dat.events.perDoi = as.data.frame(tapply(dat.events$number.events, list(dat.events$doi, dat.events$eventType), sum))
dat.events.perDoi$doi = rownames(dat.events.perDoi)
dat.events.perDoi[is.na(dat.events.perDoi)] = 0

# look at it
colnames(dat.events.perDoi)
dat.events.perDoi["10.1371/journal.pone.0008280",]
summary(dat.events.perDoi)

