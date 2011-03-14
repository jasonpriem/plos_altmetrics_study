write.table.gzip <- function
## Simple function to write out a dataframe and then gzip it
(data, ##< data frame to save
basedir,  ##< directory
filename   ##< filename
) {
    write.table(data, file=file.path(basedir, filename), row.names=FALSE, sep="\t", col.names=names(data), na="NA")
    system(sprintf("gzip -f %s", file.path(basedir, filename)))
    ##<<note To see the first few lines of the file when it is zipped
    ## from the command line, run this:
    ##   cat ../data/raw/event_counts_wos_raw.txt.gz | zcat | head -5
}

