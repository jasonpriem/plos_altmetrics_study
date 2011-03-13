\name{event_counts_altmetrics_cleaned_create}
\alias{event_counts_altmetrics_cleaned_create}
\title{event counts altmetrics cleaned create}
\description{Addresses data idiocyncracies in raw eventcounts altmetrics event count data}
\usage{
event_counts_altmetrics_cleaned_create(dat.raw.eventcounts)
}
\arguments{
  \item{dat.raw.eventcounts}{Raw eventcounts dataframe}
}
\details{Make sure all rows have good DOIs.  This also detects rogue line breaks.

Create a column to store days since published

Change journal names from strings to factors

Change f1000Factor strings to integer counts. 
a value of "false" means count of 0

Change wikipediaCites NAs to 0s

Change mendeleyReadersCount NAs to 0s

Change facebookClickCount NAs to 0s

Fix the Facebook results from Facebook API with negative numbers
It is not clear what the negative numbers mean (not in Facebook API docs)
Setting negative Facebook counts to NA

For article Type, set NAs to "Research Article" 
and store as a factor

Add a column for authorsCount}
\value{return the cleaned dataframe}

\author{Jason Priem and Heather Piwowar}





