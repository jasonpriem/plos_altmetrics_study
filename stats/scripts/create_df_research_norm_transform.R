#### create dat.research.norm.transform

source("counts_normalize.R")
source("do_normalization_viz.R")




dat.backgrounds = backgrounds_for_each_journal(dat.research, altmetricsColumns, WINDOW_WIDTH_IN_DAYS)
save(dat.backgrounds, file = "../data/derived/dat_backgrounds.RData", compress="gzip")
plot_all_backgrounds_separately(dat.research, dat.backgrounds, altmetricsColumns)
plot_all_backgrounds_overlay(dat.research, dat.backgrounds, altmetricsColumns)

dat.research.norm = normalize_altmetrics(dat.research, altmetricsColumns, dat.backgrounds)
save(dat.research.norm, file = "../data/derived/dat_research_norm.RData", compress="gzip")


##details<< Do transformation
transformation_function = function(x) {log(1+x)}  
dat.research.norm.transform = dat.research.norm
dat.research.norm.transform[, altmetricsColumns] = transformation_function(dat.research.norm[, altmetricsColumns])
save(dat.research.norm.transform, file = "../data/derived/dat_research_norm_transform.RData", compress="gzip")

