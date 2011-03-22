\name{get_research_articles}
\alias{get_research_articles}
\title{get research articles}
\description{Do processing to get the research articles}
\usage{
get_research_articles(dat_raw_event_counts, dat.raw.wos)
}
\arguments{
  \item{dat_raw_event_counts}{raw event counts data from crawler SQL}
  \item{dat.raw.wos}{raw ISI Web of Science event counts data from consolidation script}
}
\details{First clean the altmetrics data from scraper

Then clean the ISI Web of Science event counts and extract relevant columns

Merge together the altmetrics and the WoS data

Filter to just research articles}


\author{Jason Priem and Heather Piwowar}
\note{documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/}



\examples{
data(dat_raw_event_counts)
data(dat_raw_wos)
get_research_articles = get_research_articles(dat_raw_event_counts, dat.raw.wos)
}
