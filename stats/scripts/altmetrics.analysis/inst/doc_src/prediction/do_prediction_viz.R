library(rpart)
library(altmetrics.analysis)


rpart_summarize = function(main, rpart_fit) {
  #quartz()
  #plot(rpart_fit, compress=TRUE)
  #text(rpart_fit, use.n=TRUE, xpd = TRUE, all=T)
  filename = paste("predict_tree", main, ".png", sep="_")
  png(filename, width=600, height=600)
  par(xpd = TRUE)
  post(rpart_fit, file = "", title=main, use.n=TRUE, xpd = TRUE)
  dev.off()
  #plotcp(rpart_fit); title(main)
  #print(rpart_fit)
  rpart_print = rpart_fit$frame
  min_xerror = min(rpart_fit$cptable[,4L])
  root_node_error = rpart_fit$frame$dev/rpart_fit$frame$n
  return(list(rpart_print= rpart_print, min_xerror = min_xerror, root_node_error = root_node_error))
}

         
                
run_many_trees = function(models_list, dat, control=rpart.control(), main) {
    for (model_name in names(models_list)) {
        print(model_name)
        fit = rpart(models_list[[model_name]], dat, control=control)
        summarize_results = rpart_summarize(paste(model_name, main, sep="_"), fit)
        print(summarize_results$min_xerror)
    }
}

get_r_squared = function(fit) {
    return(summary(fit)$adj.r.squared)
}

do_regression = function(model, dat) {
    fit = lm(model, dat)    
    return(fit)
}

run_many_regressions = function(models_list, dat, do_print=F) {
    adj.r.sq = list()
    for (model_name in names(models_list)) {
        fit = do_regression(models_list[[model_name]], dat)
        adj.r.sq[model_name] = get_r_squared(fit)
        if (do_print) {
            print(model_name)
            print(summary(fit))
            print(anova(fit))
        }
    }
    return(adj.r.sq)
}

compare_models = function(models_list, model_names, dat) {
    print(model_names)
    fit1.test = lm(models_list[[model_names[1]]], dat)
    fit2.test = lm(models_list[[model_names[2]]], dat)
    names.merge = intersect(names(fit1.test$fitted.values), names(fit2.test$fitted.values))
    fit1 = lm(models_list[[model_names[1]]], dat[names.merge,])
    fit2 = lm(models_list[[model_names[2]]], dat[names.merge,])
    print(anova(fit1, fit2))
}

### Necessary to avoid problems with fancy quotes in p-value reporting!
options(useFancyQuotes = FALSE)

models_rawvars_woscut = list(  WoS_categories_base=wos.cut ~ daysSincePublished + journal.x + authorsCount, 
                WoS_categories_all=wos.cut ~ daysSincePublished + journal.x + authorsCount + pdfDownloadsCount + htmlDownloadsCount + backtweetsCount + f1000Factor + deliciousCount + facebookShareCount + facebookLikeCount + mendeleyReadersCount + almBlogsCount + almCiteULikeCount + plosCommentCount + plosCommentResponsesCount + wikipediaCites, 
                WoS_categories_onlydownloads=wos.cut ~ daysSincePublished + journal.x + authorsCount + pdfDownloadsCount + htmlDownloadsCount, 
                WoS_categories_nodownloads = wos.cut ~ daysSincePublished + journal.x + authorsCount + backtweetsCount + f1000Factor + deliciousCount + facebookShareCount + facebookLikeCount + mendeleyReadersCount + almBlogsCount + almCiteULikeCount + plosCommentCount + plosCommentResponsesCount + wikipediaCites)

models_rawvars_woscount = list(  base=wosCount ~ daysSincePublished + journal.x + authorsCount, 
                raw_base_and_downloads=wosCount ~ daysSincePublished + journal.x + authorsCount + pdfDownloadsCount + htmlDownloadsCount, 
                raw_altmetrics_includes_downloads=wosCount ~ daysSincePublished + journal.x + authorsCount + pdfDownloadsCount + htmlDownloadsCount + f1000Factor + deliciousCount + mendeleyReadersCount + almBlogsCount + almCiteULikeCount + plosCommentCount,
                raw_altmetrics_no_downloads=wosCount ~ daysSincePublished + journal.x + authorsCount + f1000Factor + deliciousCount + mendeleyReadersCount + almBlogsCount + almCiteULikeCount + plosCommentCount)
 
### A serious limitation of using this data for prediction is that don't know cause and effect: 
###  maybe altmetrics go high after citations are high

### @export "prep"

data(dat_with_factor_scores)
dat.with.factor.scores$wos.cut = cut(dat.with.factor.scores$wosCount, quantile(dat.with.factor.scores$wosCount, c(0, .5, .75, 1), na.rm=T), include.lowest=T, labels=c("LOW", "MID", "HIGH"))
dat = dat.with.factor.scores[which(dat.with.factor.scores$year == "2008"),]

### @export "trees"

run_many_trees(models_rawvars_woscut, dat, control=rpart.control(maxdepth=3), "maxdepth3")
#run_many_trees(models_rawvars_woscut, dat, control=rpart.control(minbucket=500), "minbucket500")
#run_many_trees(models_rawvars_woscut, dat, control=rpart.control(cp=0.012), "cp012")


      