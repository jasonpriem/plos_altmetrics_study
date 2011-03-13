library(Hmisc)

#########

# Inspired by hetcore.data.frame from polycor

"get.correlations" <-
function(data, use=c("complete.obs", "pairwise.complete.obs"), type=c("pearson", "spearman")) {
  if (use == "complete.obs") data <- na.omit(data)
  p <- length(data)
  if (p < 2) stop("fewer than 2 variables.")
  R <- matrix(1, p, p)
  for (i in 2:p) {
    for (j in 1:(i-1)){
        x <- data[[i]]
        y <- data[[j]]
        r <- cor(x, y, method=type, use=use)
        R[i, j] <- R[j, i] <- r
     }
  }
  rownames(R) <- colnames(R) <- names(data)
  result <- list(correlations=R, NA.method=use, use=use, type=type)
  result
} 

############## Get correlation matrix


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

corr_pearson_normalized_create = function 
(
    dat.norm ##<< Data frame to for correlation
)
{
    tr = function(x) {log(1+x)}
    dat.norm.tr = tr(dat.norm[,c(altmetricsColumns)])

    mycor.record = get.correlations(dat.norm.tr[,altmetricsColumns], use="pairwise.complete.obs", type="pearson")
    mycor = mycor.record$correlations
    return(mycor)
    ### Returning the correlation
}

