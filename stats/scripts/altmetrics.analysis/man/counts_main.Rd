\name{counts_main}
\alias{counts_main}
\title{counts main}
\description{Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study}
\usage{
counts_main(dat_raw_event_counts, dat.raw.wos, altmetricsColumns, number_factors)
}
\arguments{
  \item{dat_raw_event_counts}{raw event counts data from crawler SQL}
  \item{dat.raw.wos}{raw ISI Web of Science event counts data from consolidation script}
  \item{altmetricsColumns}{column names of metrics to include in correlation}
  \item{number_factors}{Number of factors in factor analysis}
}
\details{Filter to just research articles

Do normalization
on the selected altmetrics columns

Do transformation

Calculate correlation matrix
using pairwise-complete observations and Pearson correlations

Run exploratory factor analysis and calculate factor scores
Applying factor names that we know only because we've already run this and 
looked at the results}


\author{Jason Priem and Heather Piwowar}
\note{documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/}



\examples{
data(dat_raw_event_counts)
data(dat_raw_wos)
dat.with.factor.scores = counts_main(dat_raw_event_counts, dat.raw.wos, altmetricsColumns, 6)
dim(dat.with.factor.scores)
}
