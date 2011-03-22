\name{dat_backgrounds}
\alias{dat_backgrounds}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Background level of each altmetric, per journal
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
See documentation on normalization for more details
}
\usage{data(dat_backgrounds)}
\format{
str(dat.backgrounds, max.level=1)
List of 7
 $ pbio:'data.frame':	1333 obs. of  20 variables:
 $ pcbi:'data.frame':	1359 obs. of  20 variables:
 $ pgen:'data.frame':	1629 obs. of  20 variables:
 $ pmed:'data.frame':	648 obs. of  20 variables:
 $ pntd:'data.frame':	623 obs. of  20 variables:
 $ pone:'data.frame':	14102 obs. of  20 variables:
 $ ppat:'data.frame':	1462 obs. of  20 variables:
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
See documentation on normalization for more details
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
ADD LINK TO ALTMETRICS PROJECT
}
\references{
%%  ~~ possibly secondary sources and usages ~~
ADD LINK TO ALTMETRICS PROJECT
}
\examples{
data(dat_backgrounds)
See Normalization vignette in altmetrics.analysis project
## maybe str(dat.raw.wos) ; plot(dat.raw.wos) ...
}
\keyword{datasets}
