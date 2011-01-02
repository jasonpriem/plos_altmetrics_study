dat = read.csv("../data/derived/dat_with_factor_scores.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(rms)
library(plyr)
library(gplots)


#collapse into counts



print.correlations = function(event_counts_month) {
	event.labels = c("wosShort", "citeulike", "delicious", "backtweets", "native comments", "html views", "pdf views", "xml views", "blogs", "wos2yr")		
	mysubset = event_counts_month[,which(names(event_counts_month) %in% event.labels)]
	#subset.gt0 = subset[,colwise(max)(subset) > 0]

	#mycor = cor(subset.gt0, method="spearman")
	mycor = cor(log(1+mysubset), method="pearson", use="pairwise.complete.obs")
#	names(mycor) = names(mysubset)
	#mycor = cor(subset.gt0)

	colorRange = round(range(mycor, na.rm=T) * 15) + 16
	colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
	#quartz()
	png(paste("../artifacts/heatmap_cor_with_wos", month, "_", offset.into.2008, ".png", sep=""), width=800, height=800)

	heatmap.2(mycor, col=colorChoices, cexRow=1.5, cexCol = 1.5, symm = T, dend = "none", trace = "none", 
		Colv=F, Rowv=F,
		lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
		#distfun = function(x) as.dist(1 - abs(cor(x))),
		margins=c(10,10), key=FALSE, keysize=0.1, 
		main=paste("corr from 2year WoS cites to alt-metrics at", month, "months\n", num.days, "days of pubs starting ", offset.into.2008, "days into 2008\n(n=", dim(event_counts_month)[1], ")"))

	dev.off()
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

do.stuff = function (dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days = 365) {
		offset.into.2008 = (365*2+10*30) - starting.day
		events.month.merge = get.events.month.merge(dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days)
		
		dd = datadist(events.month.merge[,names(events.month.merge) %nin% c("pubDate", "articleType")])
		options(datadist="dd")
		
		cols = c("wos2yr", "journal", "authorsCount", "daysSincePublished", "citeulike", "delicious", "pdf views", "html views", "xml views", "blogs", "wosShort")
		events.cols = c("citeulike", "delicious", "pdf views", "html views", "xml views", "blogs", "wosShort")
		events.month.merge.num = events.month.merge[,events.cols]
		events.month.merge.num[events.month.merge.num>0] = 1
		apply(events.month.merge.num, 2, sum)
		
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

		anova(fit.full, fit.base)
	

		#print(summary(fit.full)$adj.r.squared)
		#print(summary(fit.nowos)$adj.r.squared)
		#print(summary(fit.wosonly)$adj.r.squared)
		#print(summary(fit.base)$adj.r.squared)
		
		#print(summary(fit))
		#plot(anova(fit))
		#print(anova(fit))
		#print(summary(fit)$r.squared)
		#print(summary(fit)$adj.r.squared)
		
		mycor = print.correlations(event_counts_month) 
		
		return(list(cor.with.wos=mycor[,"wos2yr"], adj.r.squared=data.frame(month=month,
																full=summary(fit.full)$adj.r.squared, 
															  	nowos=summary(fit.nowos)$adj.r.squared, 
																wosonly=summary(fit.wosonly)$adj.r.squared,
																base=summary(fit.base)$adj.r.squared)))
}

# Note the outlier



dat.events.all = read.csv("../data/derived/plos_2008_events_with_wos.txt.gz", header=T, stringsAsFactors=F)
length(table(dat.events.all$doi))

blog.labels = c("Nature via Plos", "Research Blogging via Plos", "Postgenomic via Plos")
dat.events.all$eventType[which(dat.events.all$eventType %in% blog.labels)] = "blogs"

#names(table(dat.events.all$eventType))[table(dat.events.all$eventType) > 6000]

dat.events.all$latency = as.numeric(dat.events.all$latency)
dat.events.all$count = as.numeric(dat.events.all$count)

# negative latencies.  
# mostly occur on month boundaries
table(dat.events.all[dat.events.all$latency < 0,]$date)
# so assume they are off by a month... add a month to them.  will make estimates conservative
hist(dat.events.all[dat.events.all$latency < 0,]$latency/(60*60*24), 20)
# update to add thirty days
dat.events.all$latency[which(dat.events.all$latency < 0)] = 30*24*60*60 + dat.events.all$latency[which(dat.events.all$latency < 0)]
# what to do with the ones that are still negative?  Leave them negative I guess.
# I think we'd better delete them to make things conservative
dat.events.all$latency[which(dat.events.all$latency < 0)] = NA

## load  dat.eventcounts
load("../data/derived/eventcounts_preprocessed.RData")
length(table(dat.eventcounts$doi))
dat.eventcounts.2008 = dat.eventcounts[which(substr(dat.eventcounts$pubDate, 1, 4) == "2008"),]
length(table(dat.eventcounts.2008$doi))
rm(dat.eventcounts)
summary(dat.eventcounts.2008)

dat.events.merge = merge(dat.events.all, dat.eventcounts.2008, by="doi", all=T)
dim(dat.events.all)
dim(dat.events.merge)
length(table(dat.events.merge$doi))
summary(dat.events.merge)

dat.events.merge$pubDate = dat.wosevents.merge$pubDate.x
merged.names = c("doi", "eventType", "creator", "date", "latency", "value", "count", "pubDate", "journal", "authorsCount", "pmid", "plosSubjectTags", "plosSubSubjectTags")
dim(dat.events.merge[,merged.names])

table(dat.eventcounts.2008$articleType)

days = c(680)
num.days=365
i = 0
monthChoices = c(0.1, .25, .5, 1:5, seq(6,24, by=3))
corr.result = data.frame(cbind("eventType"=NULL, "month"=NULL, "starting.day"=NULL, "cor.with.wos"=NULL), stringsAsFactors=F)
r.squared.result = data.frame(cbind("month"=NULL, "full"=NULL, "nowos"=NULL, "wosonly"=NULL, "base"=NULL), stringsAsFactors=F)
for (month in monthChoices) {
	print(month)
	for (starting.day in days) {
		print(starting.day)
		i = i+1
		result.list = do.stuff(dat.events.merge,  dat.eventcounts.2008, month, starting.day, num.days)
		print(result.list["adj.r.squared"])
		r.squared.result = rbind(r.squared.result, result.list$adj.r.squared)
		row = data.frame(eventType=names(result.list["cor.with.wos"]), month, starting.day, result.list["cor.with.wos"], stringsAsFactors=F)
		corr.result = rbind(corr.result, row)
	}
}
#result

quartz()
matplot(r.squared.result$month, r.squared.result[2:5], pch="x", type="b")
legend(1, .8, names(r.squared.result[2:5]), pch="x", col=1:4)

quartz()
#png(paste("../artifacts/correlations_over_time.png", sep=""), width=800, height=800)
matplot(sh$month, sh[3:12], pch="x", type="b", col=3:12)
legend(15, .9, substr(colnames(sh[3:12]), 14, 100), pch="x", col=3:12)
#dev.off()

quartz()
events.month.merge.24 = get.events.month.merge(dat.events.merge,  dat.eventcounts.2008, month=24, starting.day, num.days)
pairs(events.month.merge.24[,cols])


events.month.merge[events.month.merge$wos2yr==168,]  #10.1371/journal.pmed.0050045
table(events.month.merge$wos2yr)


table(corr.result$eventType)
sh = reshape(corr.result, v.names=c("cor.with.wos"), timevar="eventType", idvar=c("starting.day", "month"), direction="wide")
aggregate(sh[,3:12], by=list(sh$month), mean, na.rm=T)
aggregate(sh[,3:12], by=list(sh$starting.day), mean, na.rm=T)


month=2
events.month.merge.firsthalf = get.events.month.merge(dat.events.merge, dat.eventcounts.2008, month, starting.day, 180)
events.month.merge.secondhalf = get.events.month.merge(dat.events.merge, dat.eventcounts.2008, month, starting.day+180, 180)
fit.full.firsthalf = lm(log(1+wos2yr) ~ log(1+authorsCount) +  (factor(journal) * daysSincePublished * log(1+`xml views`) + log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + log(1+wosShort) +log(1+`xml views`)+ log(1+citeulike) ), 
	data=events.month.merge.firsthalf)


prediction.secondhalf = predict(fit.full.firsthalf, events.month.merge.secondhalf, interval="prediction")
predicted = exp(prediction.secondhalf[,"fit"])-1
actual = events.month.merge.secondhalf$wos2yr
quartz()
plot(predicted, actual, xlim=c(0,400), ylim=c(0,400))
plot(log(1+predicted), log(1+actual), xlim=c(-6, 6), ylim=c(-6, 6))

quantile(actual, .95, na.rm=T)

table(predicted>2, actual>0)
chisq.test(table(predicted>2, actual>0))

library(lmtest)
waldtest(fit.full, fit.base, test="Chisq")
anova(fit.full, fit.base)