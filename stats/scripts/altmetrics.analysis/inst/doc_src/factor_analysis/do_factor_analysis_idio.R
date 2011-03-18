
library(psych)
library(nFactors)
library(GPArotation)
library(gplots)

library(altmetrics.analysis)

data(dat_research_norm)
dat.research.norm.transform = transformation_function(dat.research.norm[, altmetricsColumns])
mycor = calc.correlations(dat.research.norm.transform, "pairwise.complete.obs", "pearson")

### @export "factor analysis results"

factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
fa.results = do_factor_analysis(dat.research.norm.transform, mycor, length(factor.labels), factor.labels)
fa.results





