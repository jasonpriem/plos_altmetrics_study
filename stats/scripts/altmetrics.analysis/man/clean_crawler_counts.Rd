\name{clean_crawler_counts}
\alias{clean_crawler_counts}
\title{clean crawler counts}
\description{Cleans data idiocyncracies in raw crawler altmetrics event count data}
\usage{
clean_crawler_counts(dat.raw.eventcounts)
}
\arguments{
  \item{dat.raw.eventcounts}{Raw crawler event counts dataframe}
}
\details{Make sure all rows have good DOIs.  This also detects rogue line breaks.

Create a column to store days since published

Adjust some fields to they are the right datatype.  In particular: 

- change journal names from strings to factors

- change f1000Factor strings to integer counts. 
a f1000Factor value of "false" means count of 0

Clean up some NAs that should be 0s: 

- change wikipediaCites NAs to 0s

- change mendeleyReadersCount NAs to 0s

- change facebookClickCount NAs to 0s

Fix the Facebook results from Facebook API with negative numbers
It is not clear what the negative numbers mean (not in Facebook API docs)
Setting negative Facebook counts to NA

For article Type, set NAs to "Research Article" 
and store as a factor

Add a column for authorsCount}
\value{return the cleaned dataframe}

\author{Jason Priem and Heather Piwowar}





