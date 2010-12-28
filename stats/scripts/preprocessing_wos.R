#library(Rserve)
#Rserve(args="--no-save")

######## event_counts.txt
## This data contains into on metrics for which we only have aggregate counts

### READ DATA
dat.raw.wos = read.csv("data/raw/isi_wos.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE, quote="")

## Look at it
dim(dat.raw.wos)
names(dat.raw.wos)
summary(dat.raw.wos)

## A bit of data cleaning
dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC)) 
summary(dat.wos)

write.table(dat.wos, "data/raw/isi_wos_counts.txt", sep="\t", row.names=F)

# start with the data in the repository
dat.wos = read.csv("data/raw/isi_wos_counts.txt", header=TRUE, sep="\t", quote="")
summary(dat.wos)

# Merge with eventcounts

## eliminate columns not in use right now
dat.eventcounts = merge(dat.eventcounts, dat.wos, by.x="doi", by.y="doi")


## Look again
summary(dat.eventcounts)


