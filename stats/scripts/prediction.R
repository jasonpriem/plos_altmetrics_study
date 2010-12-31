dat = read.csv("../data/derived/dat_with_factor_scores.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat.regress = dat

library(rms)
library(plyr)
dd.regress = datadist(dat.regress)
options(datadist='dd.regress')
options(digits=2)

plot(summary(wosCount ~ downloads+bookmarks+comments+wikipedia..blogs+facebook, data=dat))

Glm(wosCount ~ downloads+bookmarks+comments+wikipedia..blogs+facebook, data=dat)

quartz()
plot(summary(Glm(wosCount ~ downloads+bookmarks+comments+wikipedia..blogs+facebook, data=dat)))

## Include date

Glm(wosCount ~ rcs(daysSincePublished,3) * (downloads+bookmarks+comments+wikipedia..blogs+facebook), data=dat)
anova(Glm(wosCount ~ rcs(daysSincePublished,3) * (downloads+bookmarks+comments+wikipedia..blogs+facebook), data=dat))

quartz()
plot(summary(Glm(wosCount ~ rcs(daysSincePublished,3) * (downloads+bookmarks+comments+wikipedia..blogs+facebook), data=dat)))

## What about without downloads
quartz()
plot(summary(Glm(wosCount ~ rcs(daysSincePublished,3) * (bookmarks+comments+wikipedia..blogs+facebook), data=dat)))


### All of that is input after two years.  Look at after 6 months.

dat.events.all = read.csv("../data/derived/all_2008_events.txt", header=T, sep="\t", stringsAsFactors=F)

dat.eventcounts = dat

library(gplots)
library(plyr)


#collapse into counts
get_event_counts_month = function(dat.events.all, months, minDaysSincePublished=0, maxDaysSincePublished=100000) {
	whereForAlts = which(dat.events.all$eventType!="wos" & dat.events.all$latency < months*30*24*60*60)
	whereForWos = which(dat.events.all$eventType=="wos" & dat.events.all$latency < 24*30*24*60*60)
	dat.use = dat.events.all[c(whereForAlts, whereForWos),]

	whereForWosShort = which(dat.events.all$eventType=="wos" & dat.events.all$latency < months*30*24*60*60)
	dat.wos.short = dat.events.all[whereForWosShort,]
	dat.wos.short$eventType = "wosShort"
	dat.use = rbind(dat.use, dat.wos.short)
	
	dat.events.perDoi = as.data.frame(tapply(dat.use$number.events, list(dat.use$doi, dat.use$eventType), sum))
	dat.events.perDoi$doi = rownames(dat.events.perDoi)

	# restrict to things published in 2008
	dat.eventcounts.merge = merge(dat.events.perDoi, dat.eventcounts, by="doi")
	dat.eventcounts.merge.2008 = dat.eventcounts.merge["2008" == dat.eventcounts.merge$year, ]
	dim(dat.eventcounts.merge.2008)

	# restrict to PLoS ONE research articles
	dat.eventcounts.merge.2008 = dat.eventcounts.merge.2008[dat.eventcounts.merge.2008$journal.x == "pone", ]
	dat.eventcounts.merge.2008 = dat.eventcounts.merge.2008[dat.eventcounts.merge.2008$articleType == "Research Article", ]
	
	dat.eventcounts.merge.2008 = dat.eventcounts.merge.2008[which(dat.eventcounts.merge.2008$daysSincePublished > minDaysSincePublished
		& dat.eventcounts.merge.2008$daysSincePublished < maxDaysSincePublished),]
	# Set all the NAs to 0s  ### Not sure if this is the right thing to do...
	dat.eventcounts.merge.2008[is.na(dat.eventcounts.merge.2008)] = 0

	# look at it
	colnames(dat.eventcounts.merge.2008)
	dat.eventcounts.merge.2008[0:5,]
	summary(dat.eventcounts.merge.2008)
	#write.table(dat.eventcounts.merge.2008, "data/derived/merge_2008_eventcounts.txt", sep="\t", row.names=F)
	return(dat.eventcounts.merge.2008)	
}


do.stuff = function (month, starting.day) {
		num.days = 365
		offset.into.2008 = (365*2+10*30) - starting.day
		event_counts_month = get_event_counts_month(dat.events.all, month, starting.day, starting.day+num.days)
		#subset = event_counts_month[,2:13]
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
		fit = lm(log(1+wos) ~ factor(journal) + num.authors + rcs(daysSincePublished, 3) * (citeulike + delicious + backtweets + log(1+`pdf views`) + log(1+`html views`) + log(1+`xml views`)), data=event_counts_month)
		print(anova(fit))
		print(summary(fit))
		print(summary(fit)$r.squared)
		print(summary(fit)$adj.r.squared)
		return(mycor[,"wos"])
}


#names(table(dat.events.all$eventType))[table(dat.events.all$eventType) > 6000]
event.labels = c("citeulike", "delicious", "backtweets", "native comments", "html views", "pdf views", "xml views", "Postgenomic via Plos", "Research Blogging via Plos", "wos")

dat.events.all$latency = as.numeric(dat.events.all$latency)

summary(dat.eventcounts[which(dat.eventcounts$year=="2008"),]$daysSincePublished)
days = seq(1030, 730, by=-365)
i = 0
monthChoices = c(1:5, seq(6,24, by=6))
result = data.frame(cbind("eventType"=NULL, "month"=NULL, "starting.day"=NULL, "cor.with.wos"=NULL), stringsAsFactors=F)
for (month in monthChoices) {
	print(month)
	for (starting.day in days) {
		print(starting.day)
		i = i+1
		cor.with.wos = do.stuff(month, starting.day)
		row = data.frame(eventType=names(cor.with.wos), month, starting.day, cor.with.wos, stringsAsFactors=F)
		result = rbind(result, row)
	}
}
#result

table(result$eventType)
sh = reshape(result, v.names=c("cor.with.wos"), timevar="eventType", idvar=c("starting.day", "month"), direction="wide")
png(paste("../artifacts/correlations_over_time.png", sep=""), width=800, height=800)
matplot(sh$month, sh[,3:12])
dev.off()
aggregate(sh[,3:12], by=list(sh$month), mean, na.rm=T)
aggregate(sh[,3:12], by=list(sh$starting.day), mean, na.rm=T)

quartz()
plot(cor.with.wos ~ factor(eventType), dat=result)
quartz()
plot(cor.with.wos ~ starting.day, col=factor(eventType), dat=result)
quartz()
plot(cor.with.wos ~ month, col=factor(eventType), dat=result)

ee = do.stuff(3, 790)
ff = do.stuff(3, 880)



	event_counts_month = get_event_counts_month(6, 700, 790)
	subset = event_counts_month[,2:13]
	subset.gt0 = subset[,colwise(max)(subset) > 0]
	names(subset.gt0)[10] = "wSh"
	names(subset.gt0) = substr(names(subset.gt0), 1, 3)

	print(month)
		
	m12 = glm.nb(wos ~ ., dat=subset.gt0)
	print(summary(m12))
	anova(m12)

	m13 = glm.nb(wos ~ pdf + htm + wSh , dat=subset.gt0)
	print(summary(m13))
	anova(m13)

	dd = datadist(subset.gt0)
	options(datadist = "dd")
	
	m13 = Glm(wos ~ pdf + htm + wSh , dat=subset.gt0, 
		family=negative.binomial(2))
	print(summary(m13))
	anova(m13)

	m14 = glm.nb(wos ~ `pdf views` + `html views` , dat=subset.gt0)
	print(summary(m14))
	anova(m14)
	
	anova(m13, m14)

m15 = lm((wos) ~ (`pdf views`) + (`html views`), dat=subset.gt0); summary(m15)




dat = dat.eventcounts.merge.2008[,c(names(dat.events.perDoi), "daysSincePublished", "authorsCount")]
dat = dat[,names(dat) %nin% c("doi")]
names(dat) = substr(names(dat), 1, 3)
attach(dat)


library(fields)
stats(dat)
var(wos)

stats(tr(dat))
var(tr(wos))


stats(log(1+wos))
var(log(1+wos))

stats(sqrt(1+wos))
var(sqrt(1+wos))

quartz()
hist(wos, n=20)

library(MASS)

summary(glm(citations ~ ., data=as.data.frame(scores.1st)))


m1 = glm.nb(wos ~ day+htm, data=dat)



m1 = glm.nb(wos ~ ., data=dat)

m1 = glm.nb(wos ~ day+htm, data=dat)



m1 = glm(wos ~ ., data=dat, family="poisson")
summary(m1)
anova(m1)

dat.trans = cbind(tr(dat[,names(dat) %nin% c("day")]), dat$day)
m2 = glm(wos ~ ., data=dat.trans, family="poisson")
summary(m2)
anova(m2)

m3 = glm(wos ~ bac+cit+del+htm+nat+pdf+Pos+Res+xml+day, data=dat[which(dat$day < 700),], family="poisson")
summary(m3)

m4 = glm(wos ~ bac+cit+del+htm+nat+Pos+Res+xml+day, data=dat[which(dat$day < 700),], family="poisson")
summary(m4)

small = dat[which(dat$day < 700),]
heatmap(cor(dat[which(dat$day < 700),], method="spearman"))

heatmap(cor(tr(dat[which(dat$day < 700),])))
cor(tr(dat[which(dat$day < 700),]))

m5 = Glm(wos ~ (bac+cit+del+htm+nat+pdf+Pos+Res+xml)*day, data=dat[which(dat$day < 11000),], family="poisson")
summary(m5)

dd = datadist(dat)
options(datadist="dd")

m6 = Glm(wos ~ (bac+cit+del+htm+nat+pdf+Pos+Res+xml)*rcs(day, 6), data=dat[which(dat$day < 1100),], family="poisson")
m6
summary(m6)
anova(m6)
plot(summary(m6))

table(dat.eventcounts.merge.2008$articleType)
dd = datadist(dat.eventcounts.merge.2008[13:35])
options(datadist="dd")
datause = dat.eventcounts.merge.2008[which(dat.eventcounts.merge.2008$articleType == "Research Article"),]
summary(datause)


m9 = Glm(wos ~ rcs(daysSincePublished,3) * (wikipediaCites +
	authorsCount + f1000Factor + backtweetsCount +
	deliciousCount + facebookShareCount  + facebookCommentCount  + 
	mendeleyReadersCount + almBlogsCount + pdfDownloadsCount + xmlDownloadsCount + 
	htmlDownloadsCount + almCiteULikeCount + plosCommentCount),
	data=datause[which(datause$journal == "pone"),], family="poisson")
m9
summary(m9)
anova(m9)
plot(summary(m9))

boxplot(wos ~ journal, dat=datause)

m8 = Glm(wos ~ rcs(daysSincePublished,3) * (wikipediaCites +
	authorsCount + f1000Factor + backtweetsCount +
	deliciousCount + facebookShareCount  + facebookCommentCount  + 
	mendeleyReadersCount + almBlogsCount  + xmlDownloadsCount + 
	htmlDownloadsCount + almCiteULikeCount + plosCommentCount),
	data=datause[which(datause$journal == "pone"),], family="poisson")
m8
summary(m8)
anova(m8)
plot(summary(m8))


m8 = Glm(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweetsCount +
	deliciousCount + 
	almBlogsCount  + xmlDownloadsCount + 
	htmlDownloadsCount + almCiteULikeCount),
	data=datause[which(datause$journal == "pone"),], family="poisson")
m8
summary(m8)
anova(m8)
plot(summary(m8))

library(stringr)
names(datause) = str_replace_all(names(datause), " ", ".")

dd = datadist(datause[,c(2:11,13:15,34)])
options(datadist = "dd")

m8 = Glm(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweets + citeulike + delicious +
	html.views + native.comments + pdf.views + Postgenomic.via.Plos + 
	Research.Blogging.via.Plos + xml.views),
	data=datause[which(datause$journal == "pone"),], family="poisson")
m8
summary(m8)
anova(m8)
plot(summary(m8))

m8 = glm.nb(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweets + citeulike + delicious +
	html.views + native.comments + pdf.views + Postgenomic.via.Plos + 
	Research.Blogging.via.Plos + xml.views),
	data=datause[which(datause$journal == "pone"),]) #, family="poisson")
m8
summary(m8)
anova(m8)
plot(summary(m8))

m8 = glm.nb(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweets + citeulike + delicious +
	html.views + native.comments + pdf.views + Postgenomic.via.Plos + 
	Research.Blogging.via.Plos + xml.views),
	data=datause[which(datause$daysSincePublished < 800 & datause$journal == "pone"),]) #, family="poisson")
m8
summary(m8)
anova(m8)
plot(summary(m8))

m9 = glm.nb(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweets + citeulike + delicious +
	html.views + native.comments + pdf.views + Postgenomic.via.Plos + 
	Research.Blogging.via.Plos + xml.views),
	data=datause[which(datause$daysSincePublished > 800 & datause$daysSincePublished < 900 & datause$journal == "pone"),]) #, family="poisson")
m9
summary(m9)
anova(m9)
plot(summary(m9))

m10 = glm.nb(wos ~ rcs(daysSincePublished,3) * (
	authorsCount + backtweets + citeulike + delicious +
	html.views + native.comments + pdf.views + Postgenomic.via.Plos + 
	Research.Blogging.via.Plos + xml.views),
	data=datause[which(datause$daysSincePublished > 900 & datause$daysSincePublished < 1000 & datause$journal == "pone"),]) #, family="poisson")
m10
summary(m10)
anova(m10)
plot(summary(m10))

m11 = glm.nb(wos ~ 	authorsCount  + citeulike +
	html.views , #+ pdf.views,
	data=datause[which(datause$daysSincePublished > 900 & datause$daysSincePublished < 1000 & datause$journal == "pone"),]) #, family="poisson")
m11
summary(m11)
anova(m11)
plot(summary(m11))

m12 = Glm(wos ~ 	authorsCount  + citeulike +
	html.views + pdf.views,
	data=datause[which(datause$daysSincePublished > 900 & datause$daysSincePublished < 1000 & datause$journal == "pone"),], family=negative.binomial(1))
m12
summary(m12)
anova(m12)
plot(summary(m12))

a = c()
for (n in 6:10) {
	b = cor(datause[which(datause$daysSincePublished > n*100 & datause$daysSincePublished < (n+1)*100 & datause$journal == "pone"),c(2:11,15,34)])[,"wos"]
	a = cbind(a, b)
}
a	
