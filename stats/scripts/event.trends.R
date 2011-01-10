options(width=250)
setwd("/home/jason/projects/Plos altmetrics study")
d<-read.table("./datasets/event_trends.txt", header=T, sep="\t")
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
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("pdf views / html views", limits=c(.1, pdf.ratio.max))

## CiteULike
p <- ggplot(d, aes(x=qtr, y=articles.with.citeulike / total.html.views * 10000, size=total.citeulike / articles.published, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("events per article\nwith events")

## comments
p <- ggplot(d, aes(x=qtr, y=articles.with.native.comments/articles.published, size=total.native.comments / articles.with.native.comments, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("pdf views / html views")

## delicious
p <- ggplot(d, aes(x=qtr, y=total.delicious/total.html.views * 1000, size=total.delicious / articles.published, colour=factor(journal)))
p + stat_smooth(alpha=.1, na.rm=TRUE) + geom_point(alpha=.7) + scale_area("pdf views / html views")


