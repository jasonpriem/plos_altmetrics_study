\name{normalize_altmetrics}
\alias{normalize_altmetrics}
\title{normalize altmetrics}
\description{Normalize the altmetrics}
\usage{
normalize_altmetrics(dat, column.names.to.normalize, journals = NULL)
}
\arguments{
  \item{dat}{data frame with eventcounts to normalize}
  \item{column.names.to.normalize}{columns with data to normalize}
  \item{journals}{list of journals.  NULL means do all journals in journal.x column}
}
\details{Calculate background levels for each journal }
\value{Return a data frame with normalized data}

\author{Jason Priem and Heather Piwowar}





