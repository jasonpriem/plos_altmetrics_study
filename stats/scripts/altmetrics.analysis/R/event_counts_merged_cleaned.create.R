library(Hmisc)

event_counts_merged_cleaned_create = function
### Merges ISI Web of Science eventcounts into other altmetrics eventcounts by DOI
(
  dat.eventcounts.nowos, ##<< data frame with eventcounts without WoS
  dat.eventcounts.wos    ##<< data frame with WoS eventcounts
  ) 
{
  colnames(dat.eventcounts.wos) = c("doi","wosCount","journal","articleNumber","year")
  
  # Merge with eventcounts
  dat.merged = merge(dat.eventcounts.nowos, dat.eventcounts.wos, by.x="doi", by.y="doi", all.x=TRUE)
  
  return(dat.merged)
  ### Data frame with altmetrics and ISI Web of Science eventcounts
}

