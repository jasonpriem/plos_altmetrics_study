options(width=250)
#setwd("/home/jason/projects/Plos altmetrics study")

#PATH_TO_RAW_DATA = "./datasets/"
PATH_TO_RAW_DATA = "../data/raw/"
#PATH_TO_DERIVED_DATA = "./datasets/"
PATH_TO_DERIVED_DATA = "../data/derived/"

# load raw_events.txt
d.all <-read.csv(paste(PATH_TO_DERIVED_DATA, "event_trends.txt", sep=""), sep="\t")

d <- d.all[,names(d.all) %in% c("qtr","journal", "articles.published", "articles.with.native.comments", "total.native.comments")]

# remove NAs
d.hasna <- d
d[is.na(d)] <- 0


# plot total articles with and without comments over time
d.byqtr.pub <- tapply(d$articles.published, d$qtr, sum)
d.byqtr.with.coms <- tapply(d$articles.with.native.comments, d$qtr, sum)
d.byqtr.without.coms <- d.byqtr.pub - d.byqtr.with.coms

barplot(rbind(d.byqtr.with.coms, d.byqtr.without.coms), space=0, border="#444444", xlab="year and quarter", ylab="articles published", main="PLoS articles: total and commented\nby quarter")


# rows for qtr, cols for journals
d.byqtr.byjrnl.with.comments<-tapply(d$articles.with.native.comments, list(d$qtr, d$journal), sum)
d.byqtr.byjrnl.pub<-tapply(d$articles.published, list(d$qtr, d$journal), sum)
d.byqtr.byjrnl.pub
d.byqtr.byjrnl.freq <-  as.data.frame(d.byqtr.byjrnl.with.comments / d.byqtr.byjrnl.pub)
freq <- d.byqtr.byjrnl.freq # for convenience

# plot percentage of articles with comments, by journal
## get rid of NaN
nans <- apply(freq, 2, is.nan)
freq[nans] <- NA
## get rid of rows that have no positive values
freq <- freq[apply(freq, 1, function(x) any(x[!is.na(x)] > 0) ),]

## set up the plot params
max.y <- max(freq[!is.na(freq)])
cols<-matrix(rainbow(ncol(freq)), nrow=1, dimnames=list(NULL, names(freq)))

## plot
plot(rownames(freq), c(rep(0, nrow(freq) - 1), max.y * 100), type="n", ylab="% articles with comments", xlab="year and quarter", main="% PLoS Articles with comments after 90 days\nby journal and quarter")
for (journal in names(freq)) {
   lines(rownames(freq), freq[,journal] * 100, type="o", lwd=3, pch=NA, col=cols[1,journal])
}
legend("topright", names(freq), cex=0.6, col=cols[1,], pch=NA, lty=1, lwd=8, bty="n")




