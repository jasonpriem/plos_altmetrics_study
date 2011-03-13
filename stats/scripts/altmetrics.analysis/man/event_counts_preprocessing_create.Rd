\name{event_counts_preprocessing_create}
\alias{event_counts_preprocessing_create}
\title{event counts preprocessing create}
\description{Do all the preprocessing on the altmetrics and ISI WoS data for PLoS study}
\usage{
event_counts_preprocessing_create(basedir)
}
\arguments{
  \item{basedir}{Path to directory above the derived and raw data subdirectories}
}
\details{First read and clean the altmetrics data from scraper

Then read the ISI Web of Science data and extract relevant parts

Merge together the altmetrics and the WoS data

Filter to just research articles

Do normalization

Calculate Pearson correlation matrix

Exploratory factor analysis and calculate factor scores}
\value{Returns the filename of the final somethingorother}

\author{Jason Priem and Heather Piwowar}
\note{documented using inlinedocs at http://inlinedocs.r-forge.r-project.org/

HEATHER!!!!! SPLIT OUT THE TRANSFORMATION FROM HERE!!!}




