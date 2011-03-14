event_counts_preprocessing_create <- function
### Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study
(
  basedir ##<< Path to directory above the derived and raw data subdirectories
)
{
  ##note<<documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

  ##details<< First read and clean the altmetrics data from scraper
  data(dat_raw_event_counts)
  dat_altmetrics_cleaned = event_counts_altmetrics_cleaned_create(dat_raw_event_counts)
  #write.table.gzip(dat_altmetrics_cleaned, basedir, "derived/event_counts_altmetrics_cleaned.txt")

  ##details<<  Then read the ISI Web of Science data and extract relevant parts
  data(dat_raw_wos)
  dat.extracted.wos = events_raw_wos_2008_create(dat.raw.wos)  
  #write.table.gzip(dat.extracted.wos, basedir, "derived/events_extracted_wos_2008.txt")
  
  ##details<<  Merge together the altmetrics and the WoS data
  dat.merged = event_counts_merged_cleaned_create(dat_altmetrics_cleaned, dat.extracted.wos)
  #write.table.gzip(dat.merged, basedir, "derived/event_counts_merged_cleaned.txt")

  ##details<<  Filter to just research articles
  dat.research = event_counts_research_create(dat.merged)
  
  ##details<<  Do normalization
  dat.research.normalized = event_counts_normalized_create(dat.research)
  
  ##details<<  Calculate Pearson correlation matrix
  ##note<< HEATHER!!!!! SPLIT OUT THE TRANSFORMATION FROM HERE!!!
  mycor = corr_pearson_normalized_create(dat.research.normalized)
  
  ##details<<  Exploratory factor analysis and calculate factor scores
  dat.with.factor.scores = event_counts_factor_scores_create(mycor, dat.research.normalized)

  dat = dat.with.factor.scores

  return(dat)
  ### Returns the filename of the final somethingorother
}

