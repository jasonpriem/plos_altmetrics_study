library(altmetrics.analysis)
source("../prediction/do_prediction_viz.R")

run_regression_subset = function(model_name){
  fit = do_regression(models_rawvars_woscount[[model_name]], dat)
    print(model_name)
    print(summary(fit))
    print(anova(fit))
}

rsquares = run_many_regressions(models_rawvars_woscount, dat, F)


### @export "rsquares"

rsquares


### @export "regression base"

run_regression_subset("base")

### @export "regression raw_base_and_downloads"

run_regression_subset("raw_base_and_downloads")

### @export "regression raw_altmetrics_includes_downloads"

run_regression_subset("raw_altmetrics_includes_downloads")

### @export "regression raw_altmetrics_no_downloads"

run_regression_subset("raw_altmetrics_no_downloads")


### @export "compare base and raw_altmetrics_no_downloads"

compare_models(models_rawvars_woscount, c("base", "raw_altmetrics_no_downloads"), dat)

### @export "compare raw_altmetrics_no_downloads and raw_altmetrics_includes_downloads"

compare_models(models_rawvars_woscount, c("raw_altmetrics_no_downloads", "raw_altmetrics_includes_downloads"), dat)

### @export "compare raw_base_and_downloads and raw_altmetrics_includes_downloads"

compare_models(models_rawvars_woscount, c("raw_base_and_downloads", "raw_altmetrics_includes_downloads"), dat)


### @export "trees"

run_many_trees(models_rawvars_woscut, dat, control=rpart.control(maxdepth=3), "maxdepth3")


