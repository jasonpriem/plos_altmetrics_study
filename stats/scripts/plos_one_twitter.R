options(width=250)
#setwd("/home/jason/projects/Plos altmetrics study")

#PATH_TO_RAW_DATA = "./datasets/"
PATH_TO_RAW_DATA = "../data/raw/"
#PATH_TO_DERIVED_DATA = "./datasets/"
PATH_TO_DERIVED_DATA = "../data/derived/"

# load raw_events.txt
d <-read.csv(paste(PATH_TO_RAW_DATA, "raw_events.txt.gz", sep=""), sep="\t")

d$pubDate  = strptime(d$date, "%Y-%m-%dT")

# get the tweets
#bt <- d[d$eventType=="backtweets",]
bt = subset(d, eventType %in% c("citeulike","delicious", "backtweets"))

# column for which journal
bt$journal <- substr(bt$doi, 17, 20)

# frame for just Plos ONE, where we have much more data
bt.pone <- bt[bt$journal == "pone",]
bt.pone$doi <- factor(bt.pone$doi[1:nrow(bt.pone)]) #drop unused levels
bt.pone$creator <- factor(bt.pone$creator[1:nrow(bt.pone)])

# remove tweets with negative latency
nrow(bt.pone[bt.pone$latency <= 0,]) / nrow(bt.pone) # 3.5%
bt.pone = bt.pone[bt.pone$latency > 0,]

# look for evidence of a decay function
## should use nlm() to actually fit curve...
latency.days = bt.pone$latency / 86400
latency.hist <- hist(latency.days, breaks=max(latency.days), plot=FALSE)
plot(latency.hist$counts + 1, log="xy", xlab="latency in days", ylab="number of tweets")

library(plyr)
library(ggplot2)
ddply(bt.pone, .(format(pubDate, "%Y")), length(table(creator)))
with(bt.pone, table(format(pubDate, "%Y"), creator))
plot(latency.date, , log="xy")

dois_per_year = ddply(d, .(year=format(pubDate, "%Y")), summarise, numdois = length(unique(doi)))
creatorevents = subset(d, eventType %in% c("citeulike","delicious", "backtweets"))
unique_creators_per_year = ddply(creatorevents, .(eventType, year=format(pubDate, "%Y")), summarise, numcreators = length(unique(creator)))
proportion_unique_creators_per_year = merge(unique_creators_per_year, dois_per_year, by="year")
proportion_unique_creators_per_year$fraction = with(proportion_unique_creators_per_year, numcreators/numdois)

#colourblind friendly palettes from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)
cbgFillPalette <- scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
cbgColourPalette <- scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

png("img/unique_creators_per_year.png", width=500, height=500)
ggplot(proportion_unique_creators_per_year, aes(x=year, y=fraction*1000, fill=factor(eventType))) + geom_bar() + opts(title="unique creators in each year, per 1000 papers") + cbgFillPalette
dev.off()

# check out the distributions (as expected, mostly power-law)
creators <- rev(sort(table(bt.pone$creator)))
plot(creators, log="xy")
dois <- rev(sort(table(bt.pone$doi)))
plot(dois, log="xy")

# save the dataset
write.table(bt.pone, file=paste(PATH_TO_DERIVED_DATA, "plos_one_tweets.txt", sep=""), row.names=FALSE, sep="\t")
