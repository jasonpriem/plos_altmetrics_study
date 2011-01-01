options(width=250)
setwd("/home/jason/projects/Plos altmetrics study")

# load raw_events.txt
d<-read.csv("./datasets/raw_events.txt", sep="\t")

# get the tweets
bt <- d[d$eventType=="backtweets",]

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

# check out the distributions (as expected, mostly power-law)
creators <- rev(sort(table(bt.pone$creator)))
plot(creators, log="xy")
dois <- rev(sort(table(bt.pone$doi)))
plot(dois, log="xy")

# save the dataset
write.table(bt.pone, file="./datasets/plos_one_tweets.txt", row.names=FALSE, sep="\t")
