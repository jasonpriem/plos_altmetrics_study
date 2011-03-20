\name{get_background}
\alias{get_background}
\title{get background}
\description{Calculate the background for columns}
\usage{
get_background(dat, cols, windowSize)
}
\arguments{
  \item{dat}{
}
  \item{cols}{Columns to normalize}
  \item{windowSize}{window Size in Days}
}
\details{Calculate which points fall within the window sizes once, 
outside the loop, for efficiency

Get the Date of publication in a computable format

Start by sorting by date, so that window will be applied properly

For each column to be normalized, calculate the weighted mean for each point}
\value{Return a data frame containing the background for each point 
of each normalized column}

\author{Jason Priem and Heather Piwowar}





