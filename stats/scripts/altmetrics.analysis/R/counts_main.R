altmetricsColumns = c( "wosCount",
"almScopusCount",
"almPubMedCentralCount",
"almCrossRefCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",
"almBlogsCount",            
"facebookCommentCount",          
"facebookLikeCount",          
"facebookShareCount",          
"facebookClickCount",          
"f1000Factor",              
"wikipediaCites",           
"backtweetsCount")
    
counts_main <- function
### Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study
(
    dat_raw_event_counts, ##<< raw event counts data from crawler SQL
    dat_raw_wos,  ##<< raw ISI Web of Science event counts data from consolidation script
    altmetricsColumns,  ##<< column names of metrics to include in correlation
    number_factors ##<< Number of factors in factor analysis
)
{
  ##note<< documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

  ##details<< First clean the altmetrics data from scraper
  dat_altmetrics_cleaned = clean_crawler_counts(dat_raw_event_counts)
  #write.table.gzip(dat_altmetrics_cleaned, basedir, "derived/event_counts_altmetrics_cleaned.txt")

  ##details<<  Then clean the ISI Web of Science event counts and extract relevant columns
  dat.extracted.wos = clean_wos_counts(dat.raw.wos)  
  #write.table.gzip(dat.extracted.wos, basedir, "derived/events_extracted_wos_2008.txt")
  
  ##details<< Merge together the altmetrics and the WoS data
  dat.merged = merge_crawler_and_wos_counts(dat_altmetrics_cleaned, dat.extracted.wos)
  #write.table.gzip(dat.merged, basedir, "derived/event_counts_merged_cleaned.txt")

  ##details<<  Filter to just research articles
  dat.research = research_articles_only(dat.merged)
  
  ##details<<  Do normalization
  ## on the selected altmetrics columns
  dat.research.norm = normalize_altmetrics(dat.research, altmetricsColumns)
  
  ##details<< Do transformation
  ## using log(1+x)
  ## ; Only retain the altmetrics columns
  transformation.function = function(x) {log(1+x)}  
  dat.research.norm.transform = transformation.function(dat.research.norm[, altmetricsColumns])
  
  ##details<< Calculate correlation matrix
  ## using pairwise-complete observations and Pearson correlations
  mycor = calc.correlations(dat.research.norm.transform, "pairwise.complete.obs", "pearson")
  
  ##details<<  Run exploratory factor analysis and calculate factor scores
  ## Applying factor names that we know only because we've already run this and 
  ## looked at the results
  factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
  dat.with.factor.scores = get_factor_scores(dat.research.norm.transform, mycor, number_factors, factor.labels)

  return(dat.with.factor.scores)
  ### Returns dataframe that contains factor scores
  
  ##examples<<
  data(dat_raw_event_counts)
  data(dat_raw_wos)
  dat.with.factor.scores = counts_main(dat_raw_event_counts, dat.raw.wos, altmetricsColumns, 6)
  dim(dat.with.factor.scores)
}

