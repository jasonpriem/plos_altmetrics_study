
## Extract the info we need
dat.raw.wos = read.csv("../data/raw/event_counts_wos_raw.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC), journal=dat.raw.wos$SO, articleNumber=dat.raw.wos$AR, year=dat.raw.wos$PY, stringsAsFactors=F) 

dim(dat.wos)
names(dat.wos)
summary(dat.wos$wosCount)

write.csv(dat.wos, "../data/derived/event_counts_wos_extracted.txt", row.names=F)
system("gzip ../data/derived/event_counts_wos_extracted.txt")
