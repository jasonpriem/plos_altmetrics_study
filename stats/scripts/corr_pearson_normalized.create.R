library(Hmisc)

#########


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

get_correlations = function(mydat, type) {
	myhetcorr = hetcor.modified(mydat, 
								use="pairwise.complete.obs", 
								std.err=FALSE, 
								pd=FALSE, 
								type=type)
	return(myhetcorr)
}


### @export "correlations of alt-metrics"

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

tr = function(x) {log(1+x)}
dat.norm = read.csv("../data/derived/event_counts_research_normalized.txt.gz", header=TRUE, sep=",", stringsAsFactors=FALSE)
dat.norm.tr = tr(dat.norm[,c(altmetricsColumns)])

mycor = get_correlations(dat.norm.tr[,altmetricsColumns], "pearson")
write.csv(mycor, "../data/derived/corr_pearson_normalized.txt", row.names=F)
