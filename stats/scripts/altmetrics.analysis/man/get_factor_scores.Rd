\name{get_factor_scores}
\alias{get_factor_scores}
\title{get factor scores}
\description{Run first-order exploratory factor analysis, calculate factor scores, 
and return the factor scores as columns appended to the original data frame}
\usage{
get_factor_scores(dat, mycor, num.factors, factor.labels = FALSE)
}
\arguments{
  \item{dat}{original data}
  \item{mycor}{Correlation}
  \item{num.factors}{Number of factors to use in factor analysis}
  \item{factor.labels}{
}
}
\details{Do the factor analysis

Calculate scores for each of the factors for each article

Merge the factor scores with the original variables in data frame}


\author{Jason Priem and Heather Piwowar}





