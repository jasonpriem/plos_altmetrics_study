
factor.scores.bartlett = function
### Calculate factor scores using Bartlett method
### Code is based on the middle of the factanal function in the stats package
(   x, ##<< a dataframe
    fa.fit, ##<< the factor analysis results; the output of calling fa()
    na.action=NULL  ##<< what to do about NAs
) {
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
    ### Return the factor scores
}

do_factor_analysis = function
(
    dat, ##<< original data
    mycor, ##<< Correlation
    num.factors=6, ##<< Number of factors to use in factor analysis
    factor.labels=FALSE  ##<< Factor labels if already known from previous run
    )
{
    library(psych)
    library(GPArotation)

    ##details<< Factor analysis using fa() from the psych package  
    ## also "minres" and "oblimin"
    fa.results = fa(mycor, num.factors, fm="minres", rotate="oblimin", 
                    scores=FALSE, residuals=TRUE, n.obs=max(dim(dat)))
                    
    ##details<< If factor labels are supplied (because this was run 
    ## previously and the results have been reviewed and named), then 
    ## name the columns using the supplied labels
    if (length(factor.labels) > 1) {
        colnames(fa.results$loadings) = factor.labels
    }
    
    ## prints the reults
    print(fa.results, sort=TRUE)
    
    return(fa.results)  
    ### Returns the fit
}


get_factor_scores = function
### Run first-order exploratory factor analysis, calculate factor scores, 
### and return the factor scores as columns appended to the original data frame
(
    dat, ##<< original data
    mycor, ##<< Correlation
    num.factors, ##<< Number of factors to use in factor analysis
    factor.labels = FALSE
    )
{
    ##details<< Do the factor analysis
    fa.results = do_factor_analysis(dat, mycor, num.factors, factor.labels)
    
    ##details<< Calculate scores for each of the factors for each article
    columns = rownames(mycor)    
    scores = factor.scores.bartlett(dat[,columns], fa.results)
    colnames(scores) = factor.labels
    
    ##details<< Merge the factor scores with the original variables in data frame
    dat.merge = cbind(dat, scores)
    
    return(dat.merge)
    ### Returning factor scores as columns appended to the original data frame

    #set.seed(42)
    #subsample = as.matrix(sample(as.data.frame(t(scores)), 500, F))
    #rownames(subsample) = factor.labels

    #library(gplots)
    #png(paste("../artifacts/factor_scores_heatmap.png", sep=""))
    #heatmap.2(subsample, cexRow=0.9, cexCol = .9, symm = F, 
    #	dend = "both", Colv=T, Rowv=T,
    #	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
    #	trace = "none", margins=c(10,10), key=T, keysize=0.1)
    #dev.off()

   #scores = as.data.frame(t(scores))
    #names(scores) = factor.labels

}
