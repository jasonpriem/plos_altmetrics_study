event_counts_altmetrics_cleaned_create <- function
### Addresses data idiocyncracies in raw eventcounts altmetrics event count data
(
  ##title
  dat.raw.eventcounts ##<< Raw eventcounts dataframe
  )
{
    ## Create a dataframe to hold the cleaned data
    dat.eventcounts = dat.raw.eventcounts
    
    ##details<< Make sure all rows have good DOIs.  This also detects rogue line breaks.
    hasGoodDoi = "10." == substr(dat.raw.eventcounts$doi, 1, 3)
    stopifnot(len(hasGoodDoi)==0)
    
    # Create a date-type variable for publication date
    dat.eventcounts$pubDate  = strptime(dat.eventcounts$pubDate, "%Y-%m-%dT")
    summary(dat.eventcounts$pubDate)
    
    ##details<< Create a column to store days since published
    dat.eventcounts$daysSincePublished = as.integer(difftime(max(dat.eventcounts$pubDate), dat.eventcounts$pubDate, units="days"))
    hist(dat.eventcounts$daysSincePublished)
    
    ## Adjust some fields to they are the right datatype.  
    
    ##details<< Change journal names from strings to factors
    dat.eventcounts$journal = factor(dat.raw.eventcounts$journal)
    
    ##details<< Change f1000Factor strings to integer counts. 
    ## a value of "false" means count of 0
    dat.eventcounts$f1000Factor = as.integer(dat.raw.eventcounts$f1000Factor)
    dat.eventcounts$f1000Factor[is.na(dat.eventcounts$f1000Factor)] = 0
    
    ##details<< Change wikipediaCites NAs to 0s
    dat.eventcounts$wikipediaCites[is.na(dat.eventcounts$wikipediaCites)] = 0
    
    ##details<< Change mendeleyReadersCount NAs to 0s
    dat.eventcounts$mendeleyReadersCount[is.na(dat.eventcounts$mendeleyReadersCount)] = 0
    
    ##details<< Change facebookClickCount NAs to 0s
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
    
    ## return the dataframe
    dat.eventcounts

}
