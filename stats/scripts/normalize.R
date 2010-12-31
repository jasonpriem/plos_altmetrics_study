library(Hmisc)

### @export "transformation function"

# using sqrt with minimum value of 1, as per advice at
# http://www.webcitation.org/query?url=http%3A%2F%2Fpareonline.net%2Fgetvn.asp%3Fv%3D8%26n%3D6&date=2010-02-11
tr = function(x) return(sqrt(1 + x))

### @export "read data"

dat.eventcounts = read.csv("../data/derived/dat_eventcounts.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

# include ISI WoS data 
dat.wos = read.csv("../data/raw/isi_wos_counts.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
colnames(dat.wos) = c("doi","wosCount","journal","articleNumber","year")
summary(dat.wos)

# Merge with eventcounts
dat.eventcounts = merge(dat.eventcounts, dat.wos, by.x="doi", by.y="doi")


# Write out the normalized data
write.csv(dat.eventcounts, "../data/derived/dat_eventcounts_withwos.txt", row.names=F)


metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal.x", "articleType", "authorsCount", "journal.y", "articleNumber", "year", "pubDateVal")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]
dat.eventcounts$pubDateVal = strptime(dat.eventcounts$pubDate, "%Y-%m-%d")
dat.eventcounts.tr = dat.eventcounts
dat.eventcounts.tr[,altmetricsColumns] = tr(dat.eventcounts.tr[,altmetricsColumns])
	
dat = dat.eventcounts.tr
isResearch = which(as.character(dat$articleType) == "Research Article")
dat = dat[isResearch,]

# Write out the normalized data
write.csv(dat, "../data/derived/dat_eventcounts_researchonly.txt", row.names=F)

### @export "normalize functions"

inWindow = function(x, windowSize=60) {
	inWindowVals = list()
	for (ii in seq(along=x)) {
		inWindowVals[ii] = list(which((x > (x[ii] - windowSize/2)) & (x < (x[ii] + windowSize/2))))
	}
	return(inWindowVals)
}

applyToWindow  = function(whichInWindow, y, fun) {
	a = sapply(seq(along=y), function(i, x, y) {mean(y[x[[i]]], na.rm=T)}, whichInWindow, y)
	return(a)
}

meanInWindow = function(whichInWindow, y) applyToWindow(whichInWindow, y, mean)
varInWindow = function(whichInWindow, y) applyToWindow(whichInWindow, y, var)

get_background = function(dat.input, cols, windowSize) {
	whichInWindow = inWindow(dat.input$daysSincePublished, windowSize)
	dat.background = data.frame(pubDate = dat.input$pubDate)
	for (col in cols) {
		print(col)
		dat.background[,col] = meanInWindow(whichInWindow, dat.input[,col])
	}
	return(dat.background)
}

plot_background = function(dat.input, dat.background, cols, title, xrange, yrange) {
	par(mfrow = c(ceiling(length(cols)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
	for (col in cols) {
		print(col)
		allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
		plot(xrange, allrange, type="n", main=col)
		points(dat.input$pubDateVal, dat.input[,col], main=col, col="black", pch=20, cex=.5)	
		points(dat.input$pubDateVal, dat.background[,col], col="red", lwd=3, main=col)
	}
	title(paste("Trends over time ", title), outer=TRUE)	
}

# Get ranges so all journals can be plotted on the same axes
yrange = data.frame(column=altmetricsColumns)
yrange$rangea = NA
yrange$rangeb = NA
for (col in altmetricsColumns) {
	yrange$rangea[which(yrange$column==col)] = range(dat[,col], na.rm=T)[1]
	yrange$rangeb[which(yrange$column==col)] = range(dat[,col], na.rm=T)[2]
}

# Compute the background
journals = names(table(dat$journal.x))
dat.background = vector("list", length(journals))
names(dat.background) = journals
for (journal in journals) {
	print(journal)
	inJournal = which(dat$journal.x==journal)
	dat.background[[journal]] = get_background(dat[inJournal, ], altmetricsColumns, 180)
}

summary(dat.background)

# Plot the background
i = 0
for (journal in journals) {
	print(journal)
	i = i+1
	inJournal = which(dat$journal.x==journal)
	#quartz()
	png(paste("../artifacts/mean_over_time_figure", i, ".png", sep=""), width=800, height=800)
	plot_background(dat[inJournal, ], dat.background[[journal]], altmetricsColumns, title=journal, range(dat$pubDateVal), yrange)
	dev.off()
}

# Now do the normalization
i = 0
dat.norm = dat
for (journal in journals) {
	print(journal)
	i = i+1
	inJournal = which(dat$journal.x==journal)
	dat.norm[inJournal, altmetricsColumns] = dat[inJournal, altmetricsColumns] / dat.background[[journal]][,altmetricsColumns]
}

# Look at the distributions
i = 0
for (col in altmetricsColumns) {
	i = i+1
	#quartz()
	filename = paste("../artifacts/hist_figure", i, ".png", sep="")
	png(filename, width=600, height=600)
	par(mfrow = c(3, 1))
	titletext = paste(col, "\nnot normalized by pubdate", sep="")
	hist(dat.eventcounts[isResearch,col], breaks=50, main=titletext)
	hist(dat[,col], breaks=50, main=paste("sqrt(1+", col, ")", "\nnot normalized by pubdate", sep=""))
	hist(dat.norm[,col], breaks=50, main=paste("sqrt(1+", col, ")", "\nnormalized by mean of 180 day window within journal", sep=""))
	dev.off()
}


# Write out the normalized data
write.csv(dat.norm, "../data/derived/dat_eventcounts_norm.txt", row.names=F, sep="\t")
