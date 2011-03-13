#library(inlinedocs)
#package.skeleton.dx("altmetrics.analysis")
# R CMD Rd2txt altmetrics.analysis/man/event_counts_altmetrics_cleaned_create.Rd 
# rm altmetrics.analysis/man/z*.Rd

# then in the directory above altmetrics.analysis, run this to build the package
# R CMD install altmetrics.analysis

# detach(package:altmetrics.analysis, unload=T)
# library(altmetrics.analysis)
# ?event_counts_altmetrics_cleaned_create

# setwd("~/Documents/Projects/PLoSimpact/new/plos_altmetrics_study/stats/scripts")
# a = event_counts_preprocessing_create("../data"); dim(a)


write.table.gzip <- function
## Simple function to call gzip to create a gz
(data, ##< data frame to save
basedir,  ##< directory
filename   ##< filename
) {
    write.table(data, file=file.path(basedir, filename), row.names=FALSE, sep="\t", col.names=names(data), na="NA")
    system(sprintf("gzip -f %s", file.path(basedir, filename)))
    # cat ../data/raw/event_counts_wos_raw.txt.gz | zcat | head -5
}


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

