\name{merge_crawler_and_wos_counts}
\alias{merge_crawler_and_wos_counts}
\title{merge crawler and wos counts}
\description{Merges ISI Web of Science eventcounts into the crawler altmetrics eventcounts by DOI}
\usage{
merge_crawler_and_wos_counts(dat.eventcounts.crawler, dat.eventcounts.wos)
}
\arguments{
  \item{dat.eventcounts.crawler}{data frame with crawler eventcounts, no WoS}
  \item{dat.eventcounts.wos}{data frame with just WoS eventcounts}
}

\value{Data frame with metrics for both altmetrics crawler event counts and ISI Web of Science event counts}

\author{Jason Priem and Heather Piwowar}





