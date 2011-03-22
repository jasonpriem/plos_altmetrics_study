\name{clean_wos_counts}
\alias{clean_wos_counts}
\title{clean wos counts}
\description{Gets necessary Web of Science columns and does any necessary cleaning}
\usage{
clean_wos_counts(dat.raw.wos)
}
\arguments{
  \item{dat.raw.wos}{Exracted WoS event data}
}

\value{Returns data frame with Web of Science citation eventcounts}

\author{Jason Priem and Heather Piwowar}
\note{Removing citations with dates before publication date would happen at an earlier step}




