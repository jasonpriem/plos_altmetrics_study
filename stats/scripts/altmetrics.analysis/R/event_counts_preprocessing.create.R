event_counts_preprocessing <- function
### Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study
(
)
{
  ##note<<documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

  ##note<<read.csv works better with .gz than zip, so working around it for now

  #workarounds while zipped to get into gz
  system("unzip ../data/derived/event_counts_altmetrics_cleaned.txt.zip")
  system("gzip ../data/derived/event_counts_altmetrics_cleaned.txt")
  raw_event_counts = read.csv("../data/raw/raw_event_counts.txt.gz", header=TRUE, sep="\t", stringsAsFactors=FALSE)

  dat_altmetrics_cleaned = event_counts_altmetrics_cleaned(dat_raw_event_counts)
  
  save(dat.eventcounts, file="../data/derived/eventcounts_preprocessed.RData")
  write.csv(dat.eventcounts, "../data/derived/event_counts_altmetrics_cleaned.txt", row.names=F)
  system("gzip ../data/derived/event_counts_altmetrics_cleaned.txt")
  
  #python -u events_raw_wos_2008.create.py >> log.out
  dat = events_raw_wos_2008(dat)
  dat = event_counts_wos_extracted(dat)
  dat = event_counts_merged_cleaned(dat)
  dat = event_counts_research(dat)
  dat = event_counts_normalized(dat)
  dat = corr_pearson_normalized(dat)
  dat = event_counts_factor_scores(dat)

  "the end"
  ### Returns the filename of the final somethingorother
}
