# Makes a new event_trends.txt dataset.
# event_trends.txt shows counts of those events that have <90 days latency.
# Latency is defined as the time between an event and the publication
# of the article it points at.
# Event counts are given by event type, journal, and year/quarter.
#
# Requires: 
#  raw_events.txt
#  raw_event_counts.txt

# housekeeping
options(width=250)
setwd("/home/jason/projects/Plos altmetrics study")
library(zoo)

# load data
eve <-read.csv("./datasets/raw_events.txt", sep="\t")
art <-read.csv("./datasets/raw_event_counts.txt", sep="\t")
art <- art[order(art$doi),] #sort by doi

# replace latencies < 0 with 0
eve$latency[eve$latency < 0] <- 0

# add all DOIs as levels of events$doi factor
eve$doi<-factor(eve$doi, levels=levels(art$doi))

# create a table of number of articles with events of a given type and under a given latency
# This is sorted by both journal and quarter of article's publication
#
# @param events dataframe of all events
# @param articles  dataframe of all articles
# @param eventType   the type of event you want to count; must match one of the levels in eventsFrame$eventType
# @param maxLatency  only count events that have occured within maxLatency seconds of their target article's pubDate
# @return   dataframe with columns quarter, journal, count of events, count of articles with >=1 event.

get.quarterly.counts.restricted.by.latency <- function(events, articles, eventType, maxLatency) {

   #events<-eve
   #articles<-art
   #eventType<-"native comments"
   #maxLatency<-7776000

   # make frame with only relevent eventType (saves resources)
   events.myType <- events[events$eventType == eventType,]

   # count of in-window events for each article
   events.myType.inWindow <- events.myType[events.myType$latency < maxLatency,]
   events.myType.tab <- data.frame(as.table(tapply(events.myType.inWindow$count, events.myType.inWindow$doi, sum)))
   events.myType.tab[is.na(events.myType.tab)] <- 0
   
   names(events.myType.tab) <- c("doi","eventsInWindow")
   articles <- (merge(articles, events.myType.tab, all.x=TRUE))
   articles$eventsInWindow[is.na(articles$eventsInWindow)] <- 0 # there are a few missing DOIs in the events that get coerced to NA

   # add yr+qtr of publication to each article
   articles$qtr <- as.yearqtr(as.Date(articles$pubDate,"%Y-%m-%d"))

   # make a table showing data for articles by journal and by quarter
   articles.byqtr.totals      <-  as.data.frame(           table(articles$qtr, articles$journal ))
   articles.byqtr.has.event    <- as.data.frame(           table(factor(articles$qtr)[articles$eventsInWindow > 0], factor(articles$journal)[articles$eventsInWindow > 0]))
   articles.byqtr.event.counts <- as.data.frame( as.table (tapply(articles$eventsInWindow, list(articles$qtr, articles$journal), sum)))
   articles.byqtr.event.counts[is.na(articles.byqtr.event.counts)] <- 0

   names(articles.byqtr.totals) <- c("qtr", "journal", "articles.published")
   eventType.nospace<-gsub(" ", ".", eventType)
   names(articles.byqtr.has.event) <- c("qtr", "journal", paste("articles.with.", eventType.nospace, sep=""))
   names(articles.byqtr.event.counts) <- c("qtr", "journal", paste("total.", eventType.nospace, sep=""))
   
   articles.byqtr <- merge(merge(articles.byqtr.totals, articles.byqtr.has.event), articles.byqtr.event.counts)

    
   # quarter/journal cells in which where a journal wasn't being published yet should be NA, not 0
   articles.byqtr.totals <- articles.byqtr.totals[order(articles.byqtr.totals$qtr),]
   articles.byqtr <- articles.byqtr[order(articles.byqtr$qtr),]
   articles.byqtr[articles.byqtr.totals$articles.published==0,3:ncol(articles.byqtr)] <- NA

   # we've got quarters in there for which we can't have data given the window length; get rid of 'em:
   qtrs.to.remove.num <- floor(maxLatency / (90 * 24 * 3600)) + 1
   qtrs <- levels(articles.byqtr$qtr)
   qtrs.to.keep <- qtrs[1:(length(qtrs) - qtrs.to.remove.num)]
   articles.byqtr <- articles.byqtr[articles.byqtr$qtr %in% qtrs.to.keep,]

   return(articles.byqtr)

}

# set params
window.latency = 7776000 # 90 days
events.byqtr<-NULL # for use in testing

# Add each event type to the big return table
eventTypes <- levels(eve$eventType)
events.byqtr<-get.quarterly.counts.restricted.by.latency(eve, art, eventTypes[1], window.latency)

for (eventType in eventTypes[2:length(eventTypes)]) {
   events.byqtr.new <- get.quarterly.counts.restricted.by.latency(eve, art, eventType, window.latency)
   events.byqtr <- merge(events.byqtr, events.byqtr.new)
}

# express quarters as decimals
events.byqtr$qtr<-as.numeric(as.yearqtr(events.byqtr$qtr))

# save output
write.table(events.byqtr, "./datasets/event_trends.txt", sep="\t", row.names=FALSE, quote=FALSE)



