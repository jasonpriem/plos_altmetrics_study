
## Could impute missing values

altmetricsColumns = c( "wosCount",
"almScopusCount",
"almPubMedCentralCount",
"almCrossRefCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",
"almBlogsCount",            
"facebookCommentCount",          
"facebookLikeCount",          
"facebookShareCount",          
"facebookClickCount",          
"f1000Factor",              
"wikipediaCites",           
"backtweetsCount")

mycorr = read.csv("../data/derived/corr_pearson_normalized.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat = read.csv("../data/derived/event_counts_research_normalized.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)

library(psych)
library(GPArotation)

num.factors=6
fit.fa.1st = fa(mycorr, num.factors, fm="minres", rotate="oblimin", 
                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat)))
factor.labels = c("citations", "facebook", "downloads", "comments", "wikipedia+\nblogs", "bookmarks")
colnames(fit.fa.1st$loadings) = factor.labels
print(fit.fa.1st, sort=TRUE)

########

####Get scores:
# based on middle of factanal()
factor.scores.bartlett = function(x, fa.fit, na.action=NULL) {
    Lambda <- fa.fit$loadings
    z <- as.matrix(x)
    if (!is.numeric(z)) 
#        stop("factor analysis applies only to numerical variables")
        z = as.matrix(colwise(as.numeric)(x))
    zz <- scale(z, TRUE, TRUE)
    d <- 1/fa.fit$uniquenesses
    tmp <- t(Lambda * d)
    scores <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
    rownames(scores) <- rownames(z)
    colnames(scores) <- colnames(Lambda)
    if (!is.null(na.action)) 
        scores <- napredict(na.act, scores)
    scores
}


###### COMPUTE FIRST ORDER SCORES
dat.for.scores = dat
scores.1st = factor.scores.bartlett(dat.for.scores[,altmetricsColumns], fit.fa.1st)
colnames(scores.1st) = factor.labels
set.seed(42)
subsample = as.matrix(sample(as.data.frame(t(scores.1st)), 500, F))
rownames(subsample) = factor.labels

library(gplots)

#png(paste("../artifacts/factor_scores_heatmap.png", sep=""))

#heatmap.2(subsample, cexRow=0.9, cexCol = .9, symm = F, 
#	dend = "both", Colv=T, Rowv=T,
#	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
#	trace = "none", margins=c(10,10), key=T, keysize=0.1)

#dev.off()

scores = as.data.frame(t(scores.1st))
names(scores) = factor.labels

dat.merge = cbind(dat.for.scores, scores.1st)
write.csv(dat.merge, "../data/derived/event_counts_factor_scores.txt", row.names=F)
system("gzip -f ../data/derived/event_counts_factor_scores.txt")

