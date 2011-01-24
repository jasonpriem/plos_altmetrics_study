
library(rms)
library(signal)

### @export "normalize functions"

inWindow = function(x, windowSize=60) {
	inWindowVals = list()
	for (ii in seq(along=x)) {
		inWindowVals[ii] = list(which((x > (x[ii] - windowSize/2)) & (x < (x[ii] + windowSize/2))))
	}
	return(inWindowVals)
}


#trimmed mean
meanInWindow = function(whichInWindow, y) {
	a = sapply(seq(along=y), function(i, x, y) {mean(y[x[[i]]], na.rm=T, trim=0.10)}, whichInWindow, y)
	return(a)
}

# or instead try a hamming window
applyWindow = function(i, x, y) {
	# compute inner product
	z = y
	z[which(is.na(z))] = 0
	window = hamming(length(x[[i]]))
	inner = (z[x[[i]]] %*% window) / sum(window)
	return(inner)
}

meanInWindow2 = function(whichInWindow, y) {
	a = sapply(seq(along=y), applyWindow, whichInWindow, y)
	return(a)
}

get_background = function(dat.input, cols, windowSize) {
	whichInWindow = inWindow(dat.input$daysSincePublished, windowSize)
	dat.background = data.frame(pubDate = dat.input$pubDate)
	for (col in cols) {
		print(col)
		dat.background[,col] = meanInWindow2(whichInWindow, dat.input[,col])
	}
	return(dat.background)
}

plot_background = function(dat.input, dat.background, cols, title, xrange, yrange, colour) {
	par(mfrow = c(ceiling(length(cols)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
	for (col in cols) {
		print(col)
		allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
		plot(xrange, allrange, type="n", main=col)
		points(dat.input$pubDateVal, dat.input[,col], main=col, col="black", pch=20, cex=.5)	
		points(dat.input$pubDateVal, dat.background[,col], col=colour, lwd=3, main=col)
	}
	title(paste("Trends over time ", title), outer=TRUE)	
}

dat.eventcounts = read.csv("../data/derived/event_counts_research.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)

metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal.x", "articleType", "authorsCount", "journal.y", "articleNumber", "year", "pubDateVal", "pmid", "plosSubjectTags", "plosSubSubjectTags", "title")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

dat.eventcounts$pubDateVal = strptime(dat.eventcounts$pubDate, "%Y-%m-%d")
dat.eventcounts$pubDateVal = as.POSIXct(dat.eventcounts$pubDateVal)

dat = dat.eventcounts

# start by sorting this by date, so that window will be applied properly
dat = dat[order(dat$pubDateVal),]

# Get ranges so all journals can be plotted on the same axes
yrange = data.frame(column=altmetricsColumns)
yrange$rangea = NA
yrange$rangeb = NA
for (col in altmetricsColumns) {
	yrange$rangea[which(yrange$column==col)] = quantile(dat[,col], c(0.01), na.rm=T)
	yrange$rangeb[which(yrange$column==col)] = quantile(dat[,col], c(0.99), na.rm=T)
}

# Compute the background
journals = names(table(dat$journal.x))
dat.background = vector("list", length(journals))
names(dat.background) = journals
for (journal in journals) {
	print(journal)
	inJournal = which(dat$journal.x==journal)
	dat.background[[journal]] = get_background(dat[inJournal, ], altmetricsColumns, 365)
}

summary(dat.background)
summary(dat.background[["pbio"]])

# Plot the background
i = 0
for (journal in journals) {
	print(journal)
	i = i+1
	inJournal = which(dat$journal.x==journal)
	#quartz()
	png(paste("../artifacts/mean_over_time_figure", i, ".png", sep=""), width=800, height=800)
	plot_background(dat[inJournal, ], dat.background[[journal]], altmetricsColumns, title=journal, range(dat$pubDateVal), yrange, colour=rainbow(length(journals))[i])
	dev.off()
}

png(paste("../artifacts/mean_over_time_all.png", sep=""), width=800, height=800)
par(mfrow = c(ceiling(length(altmetricsColumns)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
cols = altmetricsColumns
xrange = range(dat$pubDateVal)
for (col in cols) {
	i=0
	allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
	plot(xrange, allrange, type="n", main=col)
	for (journal in journals) {
		i = i+1
		inJournal = which(dat$journal.x==journal)
		journal.background = dat.background[[journal]]
		#quartz()		
		lines(dat[inJournal, "pubDateVal"], journal.background[,col], col=rainbow(length(journals))[i], lwd=3)
	}
}
title(paste("Trends over time per journal"), outer=TRUE)
dev.off()


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
	hist(dat[,col], breaks=50, main=titletext)
	hist(dat.norm[,col], breaks=50, main=paste("(1", col, ")", "\nnormalized by mean of 180 day window within journal", sep=""))
	hist(log(1+dat.norm[,col]), breaks=50, main=paste("log(0.01+", col, ")", "\nnormalized by pubdate", sep=""))
	dev.off()
}


# Write out the normalized data
write.csv(dat.norm, "../data/derived/event_counts_research_normalized.txt", row.names=F)
system("gzip ../data/derived/event_counts_research_normalized.txt")
