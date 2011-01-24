library(Hmisc)

tr = function(x) {log(1+x)}

dat.not.norm = read.csv("../data/derived/event_counts_research.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat.norm = read.csv("../data/derived/event_counts_research_normalized.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)
dats = list()
dats[["normalized"]] = tr(dat.norm[,c(altmetricsColumns)])
dats[["notNormalized"]] = tr(dat.not.norm[,c(altmetricsColumns)])

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

corrColumns = c( 
	"wosCount",
"pdfDownloadsCount",        
"htmlDownloadsCount",    
"mendeleyReadersCount",     
"almCiteULikeCount",        
"deliciousCount",
"almBlogsCount",   
"backtweetsCount", 
"wikipediaCites",        
"f1000Factor",              
"plosCommentCount",         
"plosCommentResponsesCount",
"facebookCommentCount"                     
)


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
    #print(i)
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
	#mycor.return = adjust.to.positive.definite(mycor.unadjusted)
	mycor.return = mycor.unadjusted
	return(mycor.return)
}

plot_heatmap = function (mycor, main, dend="none", Colv=F, withlabels=F) {
	Colv = (dend=="both")
	Rowv = (dend=="both")
	if (withlabels==TRUE) {
		textsize=6
		marginsize=40
	} else {
		textsize=.2
		marginsize=.5
	}
	colorRange = round(range(mycor, na.rm=T) * 15) + 16
	colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
	heatmap.2(mycor, col=colorChoices, symm = TRUE, cexRow=textsize, cexCol = textsize, 
		#dend = "both", Colv=T, 
		dend = dend, Colv=Colv, Rowv=Rowv,
		lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(0.1, 2, 0.1), 
		trace = "none", margins=c(marginsize, marginsize), key=FALSE, keysize=0.1, main=main)
}
plot_heatmap(mycor, type, withlabels=T)


### @export "correlations of alt-metrics"

# for different normalizations and correlation types, for all altmetrics
for (norm in names(dats)){
	dat = dats[[norm]]
	for (type in c("spearman", "pearson")) {
		mycor = get_correlations(dat[,altmetricsColumns], type)
		write.csv(mycor, paste("../data/derived/dat_corr_", type, "_", norm, ".txt", sep=""), row.names=F)
	}
}

for (norm in names(dats)){
	dat = dats[[norm]]
	for (type in c("spearman", "pearson")) {
		mycor = get_correlations(dat[,corrColumns], type)
		#quartz()
		png(paste("../artifacts/heatmap_altmetrics_", type, "_", norm, ".png", sep=""), height=2000, width=2000)
		plot_heatmap(mycor, type,withlabels=T)
		title(norm)
		dev.off()
	}
}

# Now with dendrograms
type = "pearson"
norm = "normalized"
dat = dats[[norm]]
mycor = get_correlations(dat[,corrColumns], type)
png(paste("../artifacts/heatmap_altmetrics_dend_", type, "_", norm, ".png", sep=""), height=2000, width=2000)
plot_heatmap(mycor, type, dend="both",withlabels=T)
title(norm)
dev.off()

png(paste("../artifacts/heatmap_altmetrics_allall.png", sep=""), height=2000, width=2000)
plot_heatmap(mycor, type, dend="none",withlabels=T)
title(norm)
dev.off()


# for different journals, different years
type = "pearson"
norm = "normalized"
dat = dats[[norm]]
for (year in 2010:2004) {
	for (journal in names(table(dat$journal.x))) {
		print(year); print(journal)
		inYear = which(dat$year == year)
		inJournal = which(dat$journal.x == journal)
		dat.subset = dat[intersect(inYear,inJournal),corrColumns]
		if (nrow(dat.subset) > 50) {
			print(dim(dat.subset))
			mycor = get_correlations(dat.subset, type)
			png(paste("../artifacts/heatmap_altmetrics_", journal, year, ".png", sep=""), height=2000, width=2000)
			plot_heatmap(mycor, type,withlabels=F)
			title(paste("\n", year, journal, norm, type, "n=", nrow(dat.subset)))
			dev.off()
			#write.csv(dat.events, paste("../data/derived/dat_corr", type, ".txt", sep=""), row.names=F)
		}
	}
}


# for different years
type = "pearson"
norm = "normalized"
journal = "all"
dat = dats[[norm]]
for (year in 2004:2010) {
	print(year)
	inYear = which(dat$year == year)
	mycor = get_correlations(dat[inYear,corrColumns], type)
	png(paste("../artifacts/heatmap_altmetrics_", journal, year, ".png", sep=""), width=2000, height=2000)
	plot_heatmap(mycor, type)
	#title(paste("\n", year, journal, norm, type))
	dev.off()
	#write.csv(dat.events, paste("../data/derived/dat_corr", type, ".txt", sep=""), row.names=F)
}

# for different journals
type = "pearson"
norm = "normalized"
dat = dats[[norm]]
year = "all"
for (journal in names(table(dat$journal.x))) {
	print(year); print(journal)
	inJournal = which(dat$journal.x == journal)
	dat.subset = dat[inJournal,corrColumns]
	print(dim(dat.subset))
	mycor = get_correlations(dat.subset, type)
	png(paste("../artifacts/heatmap_altmetrics_", journal, year, ".png", sep=""), height=2000, width=2000)
	plot_heatmap(mycor, type,withlabels=F)
	title(paste("\n", year, journal, norm, type, "n=", nrow(dat.subset)))
	dev.off()
	#write.csv(dat.events, paste("../data/derived/dat_corr", type, ".txt", sep=""), row.names=F)
}

# Now a heatmap with a subsample of articles and the variables
set.seed(42)
type = "pearson"
norm = "normalized"
year = 2008
inYear = which(dat$year == year)
dat.tosample = dats[[norm]][inYear,]
dat.subsample = as.matrix(dat.tosample[sample(1:dim(dat.tosample)[1], 1000, TRUE), corrColumns])
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

