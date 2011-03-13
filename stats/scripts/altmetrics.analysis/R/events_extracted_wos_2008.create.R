events_raw_wos_2008_create = function(
### Merges ISI Web of Science eventcounts into other altmetrics eventcounts by DOI
  dat.raw.wos ##<< Exracted WoS event data
) 
{
  dat.wos = data.frame(doi=dat.raw.wos$DI, wosCount=as.numeric(dat.raw.wos$TC), journal=dat.raw.wos$SO, articleNumber=dat.raw.wos$AR, year=dat.raw.wos$PY, stringsAsFactors=FALSE) 

  return(dat.wos)
}
