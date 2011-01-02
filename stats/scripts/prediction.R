dat = read.csv("../data/derived/dat_with_factor_scores.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(rms)
library(plyr)
library(gplots)

plot.correlations = function (mycor, month, num.days, starting.day, n) {
	colorRange = round(range(mycor, na.rm=T) * 15) + 16
	colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
	
	#quartz()
	png(paste("../artifacts/heatmap_cor_with_wos", month, "_", starting.day, ".png", sep=""), width=800, height=800)

	heatmap.2(mycor, col=colorChoices, cexRow=1.5, cexCol = 1.5, symm = T, dend = "none", trace = "none", 
		Colv=F, Rowv=F,
		lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
		#distfun = function(x) as.dist(1 - abs(cor(x))),
		margins=c(10,10), key=FALSE, keysize=0.1, 
		main=paste("corr from 2year WoS cites to alt-metrics after", month, "months\n", num.days, "days of pubs in 2008\n(n=", n, ")"))
	dev.off()	
}

get.correlations = function(dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days) {
	events.month.merge = get.events.month.merge(dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days)
	
	event.labels = c("wosShort", "citeulike", "delicious", "backtweets", "native comments", "html views", "pdf views", "xml views", "blogs", "wos2yr")		
	mysubset = events.month.merge[,which(names(events.month.merge) %in% event.labels)]
	mycor = cor(log(1+mysubset), method="pearson", use="pairwise.complete.obs")
	return(mycor)
}

get_event_counts_month = function(dat.events.merge, month, minDaysSincePublished=0, maxDaysSincePublished=100000) {
	# restrict to research articles
	
	dat.events = dat.events.merge[dat.events.merge$articleType == "Research Article", ]
	dat.events$pubDate.y = as.POSIXct(dat.events$pubDate.y)
	dim(table(dat.events$doi))
	
	# restrict to those published between supplied dates
	dat.events = dat.events[which(dat.events$daysSincePublished > minDaysSincePublished
		& dat.events$daysSincePublished <= maxDaysSincePublished),]
	dim(table(dat.events$doi))

	whereForAlts = which(dat.events$eventType!="wos" & dat.events$latency < month*30*24*60*60)
	whereForWos = which(dat.events$eventType=="wos" & dat.events$latency < 24*30*24*60*60)
	dat.use = dat.events[c(whereForAlts, whereForWos),]
	dat.use$eventType[which(dat.use$eventType=="wos")] = "wos2yr"

	whereForWosShort = which(dat.events$eventType=="wos" & dat.events$latency < month*30*24*60*60)
	dat.wos.short = dat.events[whereForWosShort,]
	dat.wos.short$eventType = "wosShort"
	dat.use = rbind(dat.use, dat.wos.short)
	
	dat.events.perDoi = as.data.frame(tapply(dat.use$count, list(dat.use$doi, dat.use$eventType), sum))
	dat.events.perDoi$doi = rownames(dat.events.perDoi)
	dim(table(dat.events.perDoi$doi))
	dim(dat.events.perDoi)
	
	# look at it
	colnames(dat.events.perDoi)
	dat.events.perDoi[0:5,]
	summary(dat.events.perDoi)
	return(dat.events.perDoi)	
}

get.events.month.merge = function (dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days) {
	event_counts_month = get_event_counts_month(dat.events.merge, month, starting.day, starting.day+num.days)
	events.month.merge = merge(event_counts_month, dat.eventcounts.2008, by="doi", all=T)
	print(dim(events.month.merge))
	print(dim(table(events.month.merge$doi)))
	summary(events.month.merge)

	# Set all the NAs to 0s  
	event.names = c("blogs", "citeulike", "delicious", "html views", "native comments", "pdf views", "wos2yr", "wosShort", "xml views", "backtweets")
	for (col in event.names) {
		if (col %in% names(events.month.merge)) {
			events.month.merge[is.na(events.month.merge[,col]),col] = 0
		}
	}
	summary(events.month.merge)	
	return(events.month.merge)
}


get.dat.events.2008 = function() {
	dat.events.2008 = read.csv("../data/derived/plos_2008_events_with_wos.txt.gz", header=T, stringsAsFactors=F)
	length(table(dat.events.2008$doi))

	blog.labels = c("Nature via Plos", "Research Blogging via Plos", "Postgenomic via Plos")
	dat.events.2008$eventType[which(dat.events.2008$eventType %in% blog.labels)] = "blogs"

	#names(table(dat.events.2008$eventType))[table(dat.events.2008$eventType) > 6000]

	dat.events.2008$latency = as.numeric(dat.events.2008$latency)
	dat.events.2008$count = as.numeric(dat.events.2008$count)

	# negative latencies.  
	# mostly occur on month boundaries
	table(dat.events.2008[dat.events.2008$latency < 0,]$date)
	# so assume they are off by a month... add a month to them.  will make estimates conservative
	#hist(dat.events.2008[dat.events.2008$latency < 0,]$latency/(60*60*24), 20)
	# update to add thirty days
	dat.events.2008$latency[which(dat.events.2008$latency < 0)] = 30*24*60*60 + dat.events.2008$latency[which(dat.events.2008$latency < 0)]
	# what to do with the ones that are still negative?  Leave them negative I guess.
	# I think we'd better delete them to make things conservative
	dat.events.2008$latency[which(dat.events.2008$latency < 0)] = NA
	return(dat.events.2008)
}

get.dat.eventcounts.2008 = function() {
	## load dat.eventcounts
	load("../data/derived/eventcounts_preprocessed.RData")
	length(table(dat.eventcounts$doi))
	dat.eventcounts.2008 = dat.eventcounts[which(substr(dat.eventcounts$pubDate, 1, 4) == "2008"),]
	length(table(dat.eventcounts.2008$doi))
	rm(dat.eventcounts)
	summary(dat.eventcounts.2008)
	return(dat.eventcounts.2008)
}

get.dat.events.merge = function() {
	dat.events.2008 = get.dat.events.2008()
	dat.eventcounts.2008 = get.dat.eventcounts.2008()
	
	dat.events.merge = merge(dat.events.2008, dat.eventcounts.2008, by="doi", all=T)
	dim(dat.events.2008)
	dim(dat.events.merge)
	length(table(dat.events.merge$doi))
	summary(dat.events.merge)

	dat.events.merge$pubDate = dat.wosevents.merge$pubDate.x
	merged.names = c("doi", "eventType", "creator", "date", "latency", "value", "count", "pubDate", "journal", "authorsCount", "pmid", "plosSubjectTags", "plosSubSubjectTags")
	dim(dat.events.merge[,merged.names])

	table(dat.eventcounts.2008$articleType)
	return(dat.events.merge)
}

# to check how many have events
#cols = c("wos2yr", "journal", "authorsCount", "daysSincePublished", "citeulike", "delicious", "pdf views", "html views", "xml views", "blogs", "wosShort")
#events.cols = c("citeulike", "delicious", "pdf views", "html views", "xml views", "blogs", "wosShort")
#events.month.merge.num = events.month.merge[,events.cols]
#events.month.merge.num[events.month.merge.num>0] = 1
#apply(events.month.merge.num, 2, sum)

prediction.guts = function (dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days = 365) {
	events.month.merge = get.events.month.merge(dat.events.merge, dat.eventcounts.2008, month, starting.day, num.days)
	fit = ols(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + log(1+wosShort) +log(1+`xml views`)+ log(1+citeulike) ), 
		data=events.month.merge[,cols])
	print(fit)

	fit.full = lm(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + log(1+wosShort) +log(1+`xml views`)+ log(1+citeulike) ), 
		data=events.month.merge[,cols])
	fit.nowos = lm(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) +log(1+`xml views`)+ log(1+citeulike) ), 
		data=events.month.merge[,cols])
	fit.wosonly = lm(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal) + log(1+wosShort),
		data=events.month.merge[,cols])
	fit.base = lm(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal),
		data=events.month.merge[,cols])

	return(data.frame(month=month,
										full=summary(fit.full)$adj.r.squared, 
									  	nowos=summary(fit.nowos)$adj.r.squared, 
										wosonly=summary(fit.wosonly)$adj.r.squared,
										base=summary(fit.base)$adj.r.squared))
}

do.pred.loop = function(days=c(680), num.days=365, monthChoices=c(0.1, .25, .5, 1:5, seq(6,24, by=3))) {
	dat.events.merge = get.dat.events.merge()
	dat.eventcounts.2008 = get.dat.eventcounts.2008()
	r.squared.result = data.frame(cbind("month"=NULL, "full"=NULL, "nowos"=NULL, "wosonly"=NULL, "base"=NULL), stringsAsFactors=F)
	for (month in monthChoices) {
		print(month)
		for (starting.day in days) {
			print(starting.day)
			adj.r.squared = prediction.guts(dat.events.merge, dat.eventcounts.2008, month, starting.day, num.days)
			print(adj.r.squared)
			r.squared.result = rbind(r.squared.result, adj.r.squared)
		}
	}
	return(r.squared.result)
}

do.corr.loop = function(days=c(680), num.days=365, monthChoices=c(0.1, .25, .5, 1:5, seq(6,24, by=6))) {
	dat.events.merge = get.dat.events.merge()
	dat.eventcounts.2008 = get.dat.eventcounts.2008()
	corr.result = data.frame(cbind("eventType"=NULL, "month"=NULL, "starting.day"=NULL, "cor.with.wos"=NULL), stringsAsFactors=F)
	for (month in monthChoices) {
		print(month)
		for (starting.day in days) {
			print(starting.day)
			mycor = get.correlations(dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days)
			plot.correlations(mycor, month, num.days, starting.day, dim(event_counts_month)[1])
			row = data.frame(eventType=names(mycor["wos2yr",]), month=month, starting.day=starting.day, cor.with.wos=mycor["wos2yr",], stringsAsFactors=F)
			corr.result = rbind(corr.result, row)
		}
	}
	return(corr.result=corr.result)
}


### Correlation
corr.result = do.corr.loop()

# see how many of each type
wos2yr.corr.over.time = reshape(corr.result, v.names=c("cor.with.wos"), timevar="eventType", idvar=c("starting.day", "month"), direction="wide")
corr.names = c("month", substr(colnames(wos2yr.corr.over.time[3:12]), 14, 100))
wos2yr.corr.aggregate = aggregate(wos2yr.corr.over.time[,3:12], by=list(sh$month), mean, na.rm=T)
names(wos2yr.corr.aggregate) = corr.names
wos2yr.corr.aggregate

## graph the correlations
quartz()
png(paste("../artifacts/correlations_over_time.png", sep=""), width=800, height=800)
matplot(wos2yr.corr.aggregate$month, wos2yr.corr.over.time[3:12], pch="x", type="b", col=3:12, main="alt-metrics correlations with ISI WoS citations at 2years")
legend(15, .9, corr.names[2:length(corr.names)], pch="x", col=3:12)
dev.off()


# look at prediction over time
r.squared.result = do.pred.loop()

# graph the predictions
quartz()
png(paste("../artifacts/r_squared_over_time_diff_models.png", sep=""), width=800, height=800)
matplot(r.squared.result$month, r.squared.result[2:5], pch="x", type="b", main="Adjusted R-squared of predictions for different models")
legend(1, .8, names(r.squared.result[2:5]), pch="x", col=1:4)
dev.off()

## verify alt-metrics are an improvement
events.month.merge.2 = get.events.month.merge(get.dat.events.merge(), get.dat.eventcounts.2008(), month=2, 680, 365)

fit.full = lm(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + log(1+wosShort) +log(1+`xml views`)+ log(1+citeulike) ), 
	data=events.month.merge.2[,cols])
fit.wosonly = lm(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal) + log(1+wosShort),
	data=events.month.merge[,cols])
	
anova(fit.full, fit.wosonly)
library(lmtest)
waldtest(fit.full, fit.wosonly, test="Chisq")


# look at pairs
events.month.merge.24 = get.events.month.merge(get.dat.events.merge(), get.dat.eventcounts.2008(), month=24, 680, 365)
quartz()
png(paste("../artifacts/pairs_altmetrics_24months.png", sep=""), width=800, height=800)
pairs(events.month.merge.24[,cols])
dev.off()

# note the outlier
events.month.merge.24[events.month.merge$wos2yr==168,]  #10.1371/journal.pmed.0050045
table(events.month.merge.24$wos2yr)


### Look at prediction accuracy
month=2
starting.day = 680
dat.events.merge = get.dat.events.merge()
dat.eventcounts.2008 = get.dat.eventcounts.2008()
events.month.merge.firsthalf = get.events.month.merge(dat.events.merge, dat.eventcounts.2008, month, starting.day, 180)
events.month.merge.secondhalf = get.events.month.merge(dat.events.merge, dat.eventcounts.2008, month, starting.day+180, 180)
fit.full.firsthalf = lm(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + log(1+wosShort) +log(1+`xml views`)+ log(1+citeulike) ), 
	data=events.month.merge.firsthalf)

prediction.secondhalf = predict(fit.full.firsthalf, events.month.merge.secondhalf, interval="prediction")
predicted = exp(prediction.secondhalf[,"fit"])-1
actual = events.month.merge.secondhalf$wos2yr
quartz()
png(paste("../artifacts/actual_vs_predicted_2months.png", sep=""), width=800, height=800)
plot(predicted, actual, xlim=c(0,400), ylim=c(0,400), main="actual vs predicted")
dev.off()

quartz()
png(paste("../artifacts/actual_vs_predicted_log_2months.png", sep=""), width=800, height=800)
plot(log(1+predicted), log(1+actual), xlim=c(-6, 6), ylim=c(-6, 6), main="actual vs predicted, log scale")
dev.off()

quantile(actual, .50, na.rm=T)
quantile(predicted, .95, na.rm=T)

table(predicted>2.55, actual>0)
chisq.test(table(predicted>2, actual>0))

