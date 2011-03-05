
### @export "ISI WoS preprocessing"

## Look at it
dim(dat.raw.wos)
names(dat.raw.wos)

## Extract the info we need
dat.raw.wos = read.csv("../data/raw/events_raw_wos_2008.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC), journal=dat.raw.wos$SO, articleNumber=dat.raw.wos$AR, year=dat.raw.wos$PY, stringsAsFactors=F) 

dim(dat.wos)
names(dat.wos)
summary(dat.wos$wosCount)

write.table(dat.wos, "../data/derived/events_extracted_wos_2008.txt", sep="\t", row.names=F, col.names=names(dat.wos))

### @export "Merge ISI WoS extracted event counts"

## This file is available in the downloads directory


# start with the data in the repository
#dat.wos = read.csv("../data/raw/isi_wos_counts.txt", header=TRUE, sep="\t", quote="")
#colnames(dat.wos) = c("doi","wosCount","journal","articleNumber","year")
#summary(dat.wos)

# Merge with eventcounts
#dat.eventcounts = merge(dat.eventcounts, dat.wos, by.x="doi", by.y="doi")

## Look again
#summary(dat.eventcounts)


