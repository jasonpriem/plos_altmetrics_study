library(Hmisc)

dat.not.norm = read.csv("../data/derived/dat_eventcounts_withwos.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat.norm = read.csv("../data/derived/dat_eventcounts_norm.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
dats = list()
dats[["normalized"]] = dat.norm
dats[["notNormalized"]] = dat.not.norm

altmetricsColumns = c( "pdfDownloadsCount",        
"htmlDownloadsCount",       
"xmlDownloadsCount",        
"mendeleyReadersCount",     
"almCiteULikeCount",        
"wosCount",
"almScopusCount",           
"almCrossRefCount",         
"almPubMedCentralCount",    
"plosCommentCount",         
"plosCommentResponsesCount",
"deliciousCount",           
"facebookClickCount",       
"almBlogsCount",            
"wikipediaCites",           
"f1000Factor",              
"facebookCommentCount",     
"facebookLikeCount",        
"facebookShareCount",       
"backtweetsCount")


#########

# Extracted then modified from hetcor.data.frame from polycor
adjust.to.positive.definite = function(inputcor) {
    min.eigen = min(eigen(inputcor, only.values=TRUE)$values)
    if (min.eigen < 0){
        print("will try to make correlation matrix positive-definite")
        cor.corrected <- nearcor(inputcor)  # also could try nearPD
        if (!cor.corrected$converged) {
            stop("attempt to make correlation matrix positive-definite failed")
        }
        print("the correlation matrix has been adjusted to make it positive-definite")
        outputcor = cor.corrected$cor
        rownames(outputcor) <- rownames(inputcor)
        colnames(outputcor) <- colnames(inputcor)
    } else {
        print("Correlation matrix already positive-definite, so no adjustment needed")
        outputcor = inputcor
    }
    outputcor
}

# Based on hetcore.data.frame from polycor
# Modified slightly to call different correlation function for continuous correlations
# and print out updates
"hetcor.modified" <-
function(data, ML=FALSE, std.err=TRUE, use=c("complete.obs", "pairwise.complete.obs"),
  bins=4, pd=TRUE, type=c("pearson", "spearman"), ...){
  se.r <- function(r, n){
    rho <- r*(1 + (1 - r^2)/(2*(n - 3))) # approx. unbiased estimator
    v <- (((1 - rho^2)^2)/(n + 6))*(1 + (14 + 11*rho^2)/(2*(n + 6)))
    sqrt(v)
    }
  use <- match.arg(use)
  if (class(data) != "data.frame") stop("argument must be a data frame.")
  if (use == "complete.obs") data <- na.omit(data)
  p <- length(data)
  if (p < 2) stop("fewer than 2 variables.")
  R <- matrix(1, p, p)
  Type <- matrix("", p, p)
  SE <- matrix(0, p, p)
  N <- matrix(0, p, p)
  Test <- matrix(0, p, p)
  diag(N) <- if (use == "complete.obs") nrow(data)
             else sapply(data, function(x) sum(!is.na(x)))
  for (i in 2:p) {
    print(i)
    for (j in 1:(i-1)){
      x <- data[[i]]
      y <- data[[j]]
      if (inherits(x, c("numeric", "integer")) && inherits(y, c("numeric", "integer"))) {
#         r <- cor(x, y, use="complete.obs")
#         r <- rcorr(x, y, type="pearson")$r[1,2]
         r <- rcorr(x, y, type=type)$r[1,2]
#         Type[i, j] <- Type[j, i] <- "Pearson"
#         Type[i, j] <- Type[j, i] <- "rcorr Pearson"
		 if (type=="spearman")
         	Type[i, j] <- Type[j, i] <- "rcorr Spearman"
		 else
      		Type[i, j] <- Type[j, i] <- "rcorr Pearson"
         R[i, j] <- R[j, i] <- r
         if (std.err) {
           n <- sum(complete.cases(x, y))
           SE[i, j] <- SE[j, i] <- se.r(r, n)
           N[i, j] <- N[j, i] <- n
           Test[i, j] <- pchisq(chisq(x, y, r, bins=bins), bins^2 - 2, lower.tail=FALSE)
           }
         }
      else if (inherits(x, "factor") && inherits(y, "factor")) {
         Type[i, j] <- Type[j, i] <- "Polychoric"
         result <- polychor(x, y, ML=ML, std.err=std.err)
         if (std.err){
           n <- sum(complete.cases(x, y))
           R[i, j] <- R[j, i] <- result$rho
           SE[i, j] <- SE[j, i] <- sqrt(result$var[1,1])
           N[i, j] <- N[j, i] <- n
           Test[i, j] <- if (result$df > 0)
                pchisq(result$chisq, result$df, lower.tail=FALSE)
                else NA
           }
         else R[i, j] <- R[j, i] <- result
         }
       else {
         if (inherits(x, "factor") && inherits(y, c("numeric", "integer")))
           result <- polyserial(y, x, ML=ML, std.err=std.err, bins=bins)
         else if (inherits(x, c("numeric", "integer")) && inherits(y, "factor"))
           result <- polyserial(x, y, ML=ML, std.err=std.err, bins=bins)
         else {
             stop("columns must be numeric or factors.")
             }
         Type[i, j] <- Type[j, i] <- "Polyserial"
         if (std.err){
           n <- sum(complete.cases(x, y))
           R[i, j] <- R[j, i] <- result$rho
           SE[i, j] <- SE[j, i] <- sqrt(result$var[1,1])
           N[i, j] <- N[j, i] <- n
           Test[i, j] <- pchisq(result$chisq, result$df, lower.tail=FALSE)
           }
         else R[i, j] <- R[j, i] <- result
         }
       }
     }
   if (pd) {
       if (min(eigen(R, only.values=TRUE)$values) < 0){
            cor <- nearcor(R)
            if (!cor$converged) stop("attempt to make correlation matrix positive-definite failed")
            warning("the correlation matrix has been adjusted to make it positive-definite")
            R <- cor$cor
        }
    }
   rownames(R) <- colnames(R) <- names(data)
   result <- list(correlations=R, type=Type, NA.method=use, ML=ML)
   if (std.err) {
     rownames(SE) <- colnames(SE) <- names(data)
     rownames(N) <- colnames(N) <- names(N)
     rownames(Test) <- colnames(Test) <- names(data)
     result$std.errors <- SE
     result$n <- if (use == "complete.obs") n else N
     result$tests <- Test
     }
   class(result) <- "hetcor"
   result
   }


############## Get correlation matrix

library(polycor)
library(gplots)
# source("helper_functions.R")

get_correlations = function(mydat, type) {
	myhetcorr = hetcor.modified(mydat, 
								use="pairwise.complete.obs", 
								std.err=FALSE, 
								pd=FALSE, 
								type=type)
	mycor.unadjusted = myhetcorr$correlations
	# Are some correlations NA?
	which(is.na(mycor.unadjusted))
	#mycor.unadjusted[which(is.na(mycor.unadjusted))] = 1

	# Now fix the correlation matrix if it is not positive-definite
	mycor.adjusted = adjust.to.positive.definite(mycor.unadjusted)
	return(mycor.adjusted)
}

plot_heatmap = function (mycor, main, dend="none", Colv=F) {
	if (dend!="none") Colv=T
	colorRange = round(range(mycor) * 15) + 16
	colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
	heatmap.2(mycor, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, 
		#dend = "both", Colv=T, 
		dend = dend, Colv=Colv, Rowv=F,
		lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
		trace = "none", margins=c(10,10), key=FALSE, keysize=0.1, main=main)
}

### @export "correlations of alt-metrics"

# for different normalizations and correlation types
for (norm in names(dats)){
	dat = dats[[norm]]
	for (type in c("spearman", "pearson")) {
		mycor = get_correlations(dat[,altmetricsColumns], type)
		#quartz()
		png(paste("../artifacts/heatmap_altmetrics_", type, "_", norm, ".png", sep=""))
		plot_heatmap(mycor, type)
		title(norm)
		dev.off()
		write.csv(mycor, paste("../data/derived/dat_corr_", type, "_", norm, ".txt", sep=""), row.names=F)
	}
}

# Now with dendrograms
type = "spearman"
norm = "normalized"
dat = dats[[norm]]
mycor = get_correlations(dat[,altmetricsColumns], type)
png(paste("../artifacts/heatmap_altmetrics_dend_", type, "_", norm, ".png", sep=""))
plot_heatmap(mycor, type, dend="both")
title(norm)
dev.off()


# for different journals
type = "spearman"
norm = "normalized"
year = 2008
dat = dats[[norm]]
for (journal in names(table(dat$journal.x))) {
	print(journal)
	if (journal != "pntd") {
		inYear = which(dat$year == year)
		inJournal = which(dat$journal.x == journal)
		mycor = get_correlations(dat[inYear %in% inJournal,altmetricsColumns], type)
		png(paste("../artifacts/heatmap_altmetrics_", journal, year, ".png", sep=""))
		plot_heatmap(mycor, type)
		title(paste("\n", year, journal, norm, type))
		dev.off()
		#write.csv(dat.events, paste("../data/derived/dat_corr", type, ".txt", sep=""), row.names=F)
	}
}


# for different years
type = "spearman"
norm = "normalized"
journal = "pbio"
dat = dats[[norm]]
for (year in 2003:2010) {
	print(year)
	inYear = which(dat$year == year)
	inJournal = which(dat$journal.x == journal)
	mycor = get_correlations(dat[inYear %in% inJournal, altmetricsColumns], type)
	png(paste("../artifacts/heatmap_altmetrics_", journal, year, ".png", sep=""))
	plot_heatmap(mycor, type)
	title(paste("\n", year, journal, norm, type))
	dev.off()
	#write.csv(dat.events, paste("../data/derived/dat_corr", type, ".txt", sep=""), row.names=F)
}

## Would be cool to do it for different article types too

# Now a heatmap with a subsample of articles and the variables
set.seed(42)
type = "spearman"
norm = "normalized"
year = 2008
inYear = which(dat$year == year)
dat.tosample = dats[[norm]][inYear,]
dat.subsample = as.matrix(dat.tosample[sample(1:dim(dat.tosample)[1], 1000, TRUE), altmetricsColumns])
m=200
png(paste("../artifacts/heatmap_articles_vs_altmetrics_", year, ".png", sep=""))
heatmap.2(t(dat.subsample), col=bluered(m*2)[1:(m*2-1)], 
 	cexRow=.6, cexCol=.6, dend = "both", trace="none", 
 	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
 	margins=c(1,10), key=FALSE, keysize=0.1, scale="row", symbreaks=T)
title(paste("\narticles vs altmetrics", year, norm, type))
dev.off()


showpanel <- function(col) {
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )
}
#quartz()
#showpanel(colorChoices)
#showpanel(bluered(m*2)[1:(m*2-1)])

