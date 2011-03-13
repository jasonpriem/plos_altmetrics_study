event_counts_research_create = function
### Filters to just research articles
(  dat.eventcounts ##<< data frame with eventcounts
  ) 
{
  # strip to research only 	
  dat = dat.eventcounts
  isResearch = which(as.character(dat.eventcounts$articleType) == "Research Article")
  dat.research = dat.eventcounts[isResearch,]
  return(dat.research)
  ### Return data frame with just research articles
}

