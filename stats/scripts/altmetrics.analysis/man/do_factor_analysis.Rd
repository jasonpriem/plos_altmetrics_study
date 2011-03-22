\name{do_factor_analysis}
\alias{do_factor_analysis}
\title{do factor analysis}

\usage{
do_factor_analysis(dat, mycor, num.factors = 6, factor.labels = FALSE)
}
\arguments{
  \item{dat}{original data}
  \item{mycor}{Correlation}
  \item{num.factors}{Number of factors to use in factor analysis}
  \item{factor.labels}{Factor labels if already known from previous run}
}
\details{Factor analysis using fa() from the psych package  
also "minres" and "oblimin"

If factor labels are supplied (because this was run 
previously and the results have been reviewed and named), then 
name the columns using the supplied labels}
\value{Returns the fit}

\author{Jason Priem and Heather Piwowar}





