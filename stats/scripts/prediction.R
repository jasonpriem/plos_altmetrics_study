dat = read.csv("../data/derived/dat_with_factor_scores.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(rms)
library(plyr)
library(gplots)


#collapse into counts
get_event_counts_month = function(dat.events, month, minDaysSincePublished=0, maxDaysSincePublished=100000) {
	# restrict to research articles
	dat.events = dat.events[dat.events$articleType == "Research Article", ]
	dat.events$pubDate.y = as.POSIXct(dat.events$pubDate.y)

	# restrict to those published between supplied dates
	dat.events = dat.events[which(dat.events$daysSincePublished > minDaysSincePublished
		& dat.events$daysSincePublished < maxDaysSincePublished),]

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

	# Set all the NAs to 0s  
	dat.events.perDoi[is.na(dat.events.perDoi)] = 0

	# look at it
	colnames(dat.events.perDoi)
	dat.events.perDoi[0:5,]
	summary(dat.events.perDoi)
	#write.table(dat.eventcounts.merge.2008, "data/derived/merge_2008_eventcounts.txt", sep="\t", row.names=F)
	return(dat.events.perDoi)	
}


do.stuff = function (dat.events.merge, month, starting.day) {
		num.days = 365
		offset.into.2008 = (365*2+10*30) - starting.day
		event_counts_month = get_event_counts_month(dat.events.merge, month, starting.day, starting.day+num.days)
		
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
		
		# remove html views because too correlated
		#fit = lm(wos ~ ., data=event_counts_month[,c("daysSincePublished", "citeulike", "delicious", "backtweets", "native comments", "pdf views", "xml views", "Postgenomic via Plos", "Research Blogging via Plos", "wos")])
		
		events.month.merge = merge(event_counts_month, dat.eventcounts, by="doi")
		
		dd = datadist(events.month.merge[,names(events.month.merge) %nin% c("pubDate", "articleType")])
		options(datadist="dd")
		
		cols = c("wos2yr", "journal", "authorsCount", "daysSincePublished", "citeulike", "delicious", "pdf views", "html views", "xml views", "blogs")
		
		#fit = lm(log(1+wos2yr) ~ factor(journal) + log(1+authorsCount) + daysSincePublished * (log(1+citeulike) + log(1+delicious) + log(1+backtweets) + log(1+`pdf views`) + log(1+`html views`) + log(1+`xml views`)), data=events.month.merge)
		#fit = lm(log(1+wos2yr) ~ factor(journal) + log(1+authorsCount) + daysSincePublished * (log(1+citeulike) + log(1+delicious) + log(1+`pdf views`) + log(1+`html views`) + log(1+`xml views`)), data=events.month.merge)
		fit = ols(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal)* log(1+`xml views`) +  (log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + daysSincePublished * (log(1+`xml views`)+ log(1+citeulike) )), 
			data=events.month.merge[,cols])
		print(fit)

		fit.full = lm(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal)* log(1+`xml views`) +  (log(1+blogs) + log(1+delicious) +   log(1+`pdf views`) + log(1+`html views`) + daysSincePublished * (log(1+`xml views`)+ log(1+citeulike) )), 
			data=events.month.merge[,cols])

		fit.base = lm(log(1+wos2yr) ~ log(1+authorsCount) +  factor(journal),
			data=events.month.merge[,cols])
		anova(fit.full, fit.base)

		#print(summary(fit))
		#plot(anova(fit))
		#print(anova(fit))
		#print(summary(fit)$r.squared)
		#print(summary(fit)$adj.r.squared)
		return(mycor[,"wos2yr"])
}

# Note the outlier



dat.events.all = read.csv("../data/derived/plos_2008_events_with_wos.txt.gz", header=T, stringsAsFactors=F)

blog.labels = c("Nature via Plos", "Research Blogging via Plos", "Postgenomic via Plos")
dat.events.all$eventType[which(dat.events.all$eventType %in% blog.labels)] = "blogs"

#names(table(dat.events.all$eventType))[table(dat.events.all$eventType) > 6000]

dat.events.all$latency = as.numeric(dat.events.all$latency)
dat.events.all$count = as.numeric(dat.events.all$count)

load("../data/derived/eventcounts_preprocessed.RData")

dat.events.merge = merge(dat.events.all, dat.eventcounts, by="doi")
dat.events.merge$pubDate = dat.wosevents.merge$pubDate.x
merged.names = c("doi", "eventType", "creator", "date", "latency", "value", "count", "pubDate", "journal", "authorsCount", "pmid", "plosSubjectTags", "plosSubSubjectTags")
dim(dat.events.merge[,merged.names])


#summary(dat.eventcounts[which(dat.eventcounts$year=="2008"),]$daysSincePublished)
days = seq(730, 1030, by=365)
i = 0
monthChoices = c(1:5, seq(6,24, by=6))
result = data.frame(cbind("eventType"=NULL, "month"=NULL, "starting.day"=NULL, "cor.with.wos"=NULL), stringsAsFactors=F)
for (month in monthChoices) {
	print(month)
	for (starting.day in days) {
		print(starting.day)
		i = i+1
		cor.with.wos = do.stuff(dat.events.merge, month, starting.day)
		row = data.frame(eventType=names(cor.with.wos), month, starting.day, cor.with.wos, stringsAsFactors=F)
		result = rbind(result, row)
	}
}
#result


pairs(events.month.merge[,cols])
events.month.merge[events.month.merge$wos2yr==168,]  #10.1371/journal.pmed.0050045
table(events.month.merge$wos2yr)


table(result$eventType)
sh = reshape(result, v.names=c("cor.with.wos"), timevar="eventType", idvar=c("starting.day", "month"), direction="wide")
png(paste("../artifacts/correlations_over_time.png", sep=""), width=800, height=800)
matplot(sh$month, sh[3:12])
dev.off()
aggregate(sh[,3:12], by=list(sh$month), mean, na.rm=T)
aggregate(sh[,3:12], by=list(sh$starting.day), mean, na.rm=T)

y = predict(fit.full1, events.month.merge, interval="prediction")
predicted = exp(y[,"fit"])-1
actual = events.month.merge$wos2yr
plot(predicted, actual)

table(predicted>3, actual>6)
chisq.test(table(predicted>3, actual>6))

library(lmtest)
waldtest(fit.full, fit.base, test="Chisq")
anova(fit.full, fit.base)