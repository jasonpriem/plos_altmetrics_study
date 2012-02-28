clean_crawler_counts <- function
### Cleans data idiocyncracies in raw crawler altmetrics event count data
(
  dat.raw.eventcounts ##<< Raw crawler event counts dataframe
  )
{
    ## Create a dataframe to hold the cleaned data
    dat.eventcounts = dat.raw.eventcounts
    
    ##details<< Make sure all rows have good DOIs.  This also detects rogue line breaks.
    hasGoodDoi = "10." == substr(dat.raw.eventcounts$doi, 1, 3)
    stopifnot(all(hasGoodDoi))
    stopifnot(length(unique(dat.raw.eventcounts$doi)) == length(dat.raw.eventcounts$doi))
    
    # Create a date-type variable for publication date
    dat.eventcounts$pubDate  = strptime(dat.eventcounts$pubDate, "%Y-%m-%dT")
    min(dat.eventcounts$pubDate)
    max(dat.eventcounts$pubDate)
    # Create a convenience variable for year
    dat.eventcounts$year = format(dat.eventcounts$pubDate, "%Y")
    table(dat.eventcounts$year)
    
    ##details<< Create a column to store days since published, relative to most recent paper in the set
    dat.eventcounts$daysSincePublished = as.integer(difftime(max(dat.eventcounts$pubDate), dat.eventcounts$pubDate, units="days"))
    #hist(dat.eventcounts$daysSincePublished)
    
    ##details<< Adjust some fields to they are the right datatype.  In particular: 
    
    ##details<< - change journal names from strings to factors
    dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)
    
    ##details<< - change f1000Factor strings to integer counts. 
    ## a f1000Factor value of "false" means count of 0
    dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
    dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0

    ##details<< Clean up some NAs that should be 0s: 
    
    ##details<< - change wikipediaCites NAs to 0s
    dat.eventcounts$wikipediaCites[is.na(dat.eventcounts$wikipediaCites)] = 0
    
    ##details<< - change mendeleyReadersCount NAs to 0s
    dat.eventcounts$mendeleyReadersCount[is.na(dat.eventcounts$mendeleyReadersCount)] = 0
    
    ##details<< - change facebookClickCount NAs to 0s
    dat.eventcounts$facebookClickCount[is.na(dat.eventcounts$facebookClickCount)] = 0
    
    ##details<< Fix the Facebook results from Facebook API with negative numbers
    ## It is not clear what the negative numbers mean (not in Facebook API docs)
    ## Setting negative Facebook counts to NA
    facebookColumns = c("facebookShareCount", "facebookLikeCount", "facebookCommentCount", "facebookClickCount")
    for (col in facebookColumns) {
    	dat.eventcounts[which(dat.eventcounts[, col] < 0), col] = NA	
    }
    
    ##details<< For article Type, set NAs to "Research Article" 
    ## and store as a factor
    dat.eventcounts$articleType[is.na(dat.raw.eventcounts$articleType)] = "Research Article" 
    dat.eventcounts$articleType = factor(dat.eventcounts$articleType)
    summary(dat.eventcounts$articleType)
    
    ##details<< Add a column for authorsCount
    dat.eventcounts$authorsCount = as.numeric(dat.raw.eventcounts$authorsCount)

    ##details<< Remove lines with NA pdfCount
    dat.eventcounts = subset(dat.eventcounts, !is.na(pdfDownloadsCount))
    
    return(dat.eventcounts)
    ### return the cleaned dataframe
}



clean_wos_counts = function(
### Gets necessary Web of Science columns and does any necessary cleaning
  dat.raw.wos ##<< Extracted WoS event data
) 
{
#  dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC), journal=dat.raw.wos$SO, articleNumber=dat.raw.wos$AR, year=dat.raw.wos$PY, stringsAsFactors=FALSE) 
  dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC))

  ## There might be a few duplicate rows.  remove them (some dup rows might have different citation counts.  oh well.)
  dat.wos = subset(dat.wos, !duplicated(doi))
  ##note<< Removing citations with dates before publication date would happen at an earlier step
  
  return(dat.wos)
  ### Returns data frame with Web of Science citation eventcounts
}



research_articles_only = function
### Filters article data to include only those with articleType == "Research Article"
(  
    dat ##<< data frame with eventcounts
  ) 
{
  isResearch = which(as.character(dat$articleType) == "Research Article")
  dat.research = dat[isResearch,]
  return(dat.research)
  ### Return data frame with just research articles
}


merge_crawler_and_wos_counts = function
### Merges ISI Web of Science eventcounts into the crawler altmetrics eventcounts by DOI
(
  dat.eventcounts.crawler, ##<< data frame with crawler eventcounts, no WoS
  dat.eventcounts.wos    ##<< data frame with just WoS eventcounts
  ) 
{  
  # Merge with eventcounts by DOI
  
  dat.merged = merge(dat.eventcounts.crawler, dat.eventcounts.wos, by.x="doi", by.y="doi", all.x=TRUE, all.y=FALSE)
  if ("wosCount" %in% names(dat.merged)) {
      dat.merged$wosCount[which(is.na(dat.merged$wosCount))] = 0
  }
  
  return(dat.merged)
  ### Data frame with metrics for both altmetrics crawler event counts and ISI Web of Science event counts
}

