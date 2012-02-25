options(width=250)
#setwd("/home/jason/projects/Plos altmetrics study")
#PATH_TO_DERIVED_DATA = "./datasets/"
PATH_TO_DERIVED_DATA = "../data/derived/"

d<-read.table(paste(PATH_TO_DERIVED_DATA, "event_trends.txt", sep=""), header=T, sep="\t")
library(ggplot2)



# remove NAs
d.hasna <- d
d[is.na(d)] <- 0

## usage data
d$html.per.article <- d$total.html.views / d$articles.published

# remove 2 outliers in html downloads
which(d$html.per.article > 3 * mean(d$html.per.article, na.rm=T)) # two values
d$html.per.article[d$html.per.article > 3 * mean(d$html.per.article, na.rm=T)] <- NA 

pdf.ratio.max <- max(d$total.pdf.views / d$total.html.views, na.rm=TRUE)

p <- ggplot(d, aes(x=qtr, y=html.per.article, size=total.pdf.views / total.html.views, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("pdf views / html views", limits=c(.1, pdf.ratio.max)) + opts(title="HTML views within 90 days")

## CiteULike
p <- ggplot(d, aes(x=qtr, y=total.citeulike / total.html.views * 1000, size=total.citeulike / articles.with.citeulike, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("bookmarks per\nbookmarked article") + opts(title="CiteULike bookmarks within 90 days")

p <- ggplot(d, aes(x=qtr, y=total.citeulike / articles.published, size=total.citeulike / articles.with.citeulike, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("bookmarks per\nbookmarked article") + opts(title="CiteULike bookmarks within 90 days")

## comments
## this will have to wait until I can remove comments that are from reviewers. I think I can do that with a simple grep.
# p <- ggplot(d, aes(x=qtr, y=total.native.comments/total.html.views * 1000, size=total.native.comments / articles.with.native.comments, colour=factor(journal)))
# p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("comments per\ncommented-upon article")

## delicious
p <- ggplot(d, aes(x=qtr, y=total.delicious/total.html.views * 1000, size=total.delicious / articles.with.delicious, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("bookmarks per\nbookmarked article") + opts(title="Delicious bookmarks within 90 days")

p <- ggplot(d, aes(x=qtr, y=total.delicious/articles.published, size=total.delicious / articles.with.delicious, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("bookmarks per\nbookmarked article") + opts(title="Delicious bookmarks within 90 days, per article")

