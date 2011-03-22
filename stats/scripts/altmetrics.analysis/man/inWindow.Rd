\name{inWindow}
\alias{inWindow}
\title{inWindow}
\description{Returns a list of points in the window}
\usage{
inWindow(x, windowSize = 365)
}
\arguments{
  \item{x}{Timing information}
  \item{windowSize}{Size of window, in the timing units}
}
\details{For each of the points in the timing vector, specify a list
<< The list contains the indicies of all points in the timing vector that are 
greater than the giving timing location minus half the window size AND
smaller than the given timing location plus half the window size}
\value{Returns the indexes of the timepoints that fall within the specified window size }

\author{Jason Priem and Heather Piwowar}





