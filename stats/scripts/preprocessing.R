#write.csv(dat.eventcounts, "../data/derived/dat_eventcounts.txt", row.names=F)
#write.csv(dat.events, "../data/derived/dat_events.txt", row.names=F)

dat.eventcounts = read.csv("../data/derived/dat_eventcounts.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat.events = read.csv("../data/derived/dat_events.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

png("{{ a.create_input_file('figure1', 'png') }}")
plot(c(1, 4, 5, 6, 3, 6), col="green")
dev.off()

png("{{ a.create_input_file('figure2', 'png') }}")
plot(c(9, 4, 2, 6, 3, 6), col="red")
dev.off()

### @export "description eventcounts"
names(dat.eventcounts)

dim(dat.eventcounts)

str(dat.eventcounts)

### @export "description events"

names(dat.events)

dim(dat.events)

table(dat.events$eventType)

str(dat.events)

