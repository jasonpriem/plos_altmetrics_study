\name{backgrounds_for_each_journal}
\alias{backgrounds_for_each_journal}
\title{backgrounds for each journal}
\description{Calculate the background vectors for each journal, for each altmetric column}
\usage{
backgrounds_for_each_journal(dat, column.names.to.normalize, window_width_in_days = 365, journals = NULL)
}
\arguments{
  \item{dat}{data frame that contains the journal, dates of publication, and altmetrics}
  \item{column.names.to.normalize}{
}
  \item{window_width_in_days}{Window width in days}
  \item{journals}{list of journals.  NULL means do all journals in journal.x column}
}
\details{Get all the names of the journal

Initialize the list that will hold the background vectors for each journal

For each journal, get the background level}
\value{Return a list that contains the background vectors for each journal for each altmetric column}

\author{Jason Priem and Heather Piwowar}





