
library(rms)
library(gplots)


plot_heatmap = function (mycor, main, dend="none", Colv=F, withlabels=F) {
	Colv = (dend=="both")
	Rowv = (dend=="both")
	if (withlabels==TRUE) {
		textsize=46
		marginsize=400
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


save_correlation_heatmap = function(dat, journal, year, dend="none", withlabels=F, main=""){
		mycor = calc.correlations(dat, "pairwise.complete.obs", "pearson")
	    pdf(paste("img/heatmap_altmetrics_", journal, year, main, ".pdf", sep=""), width=200, height=200)
	    plot_heatmap(mycor, "", dend=dend, withlabels=withlabels)
		title(paste("\n", main, year, journal))
	    dev.off()    
}

# for different journals, different years
lots_of_correlations = function(dat, corrColumns){
	save_correlation_heatmap(dat[,corrColumns], "all", "all")
    years = 2010:2004
    journals = names(table(dat$journal.x))
	#quartz()
    for (year in years) {
		inYear = which(dat$year == year)
		save_correlation_heatmap(dat[inYear,corrColumns], "all", year)
    	for (journal in journals) {
    		inJournal = which(dat$journal.x == journal)
    		
    		# save one for all years; will be overwritten but that is ok
	        save_correlation_heatmap(dat[inJournal,corrColumns], journal, "all")
    		
    		# now save a different one per journal, per year
    		#print(year); print(journal)
    		dat.subset = dat[intersect(inYear,inJournal),corrColumns]
    		if (nrow(dat.subset) > 50) {
    			#print(dim(dat.subset))
		        save_correlation_heatmap(dat.subset, journal, year) # , "n=", nrow(dat.subset)
    		}
    	}
    } 
}


heatmap_of_articles = function(dat, corrColumns, year=2008) {
    # Now a heatmap with a subsample of articles and the variables
    set.seed(42)
    inYear = which(dat$year == year)
    dat.tosample = dat[inYear,]
    dat.subsample = as.matrix(dat.tosample[sample(1:dim(dat.tosample)[1], 1000, TRUE), corrColumns])
    m=200
    pdf(paste("img/heatmap_articles_vs_altmetrics_", year, ".pdf", sep=""))
    heatmap.2(t(dat.subsample), col=bluered(m*2)[1:(m*2-1)], 
     	cexRow=1, cexCol=.1, dend = "both", trace="none", 
     	lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ),
     	margins=c(1,10), key=FALSE, keysize=0.1, scale="row", symbreaks=T)
    title(paste("\narticles vs altmetrics", year))
    dev.off()
}

showpanel <- function(column) {
  image(z=matrix(1:100, ncol=1), col=column, xaxt="n", yaxt="n" )
}
#quartz()
#showpanel(colorChoices)
#showpanel(bluered(m*2)[1:(m*2-1)])


mycor = calc.correlations(dat.research.norm.transform[, altmetricsColumns], "pairwise.complete.obs", "pearson")

# main one, with labels
save_correlation_heatmap(dat.research.norm.transform[, altmetricsColumns], "all", "all", dend="none", withlabels=T, main="labels")

# subdivisions
lots_of_correlations(dat.research.norm.transform, altmetricsColumns)

# now with dendrograms
save_correlation_heatmap(dat.research.norm.transform[, altmetricsColumns], "all", "all", dend="both", withlabels=T, main="dend")

heatmap_of_articles(dat.research.norm.transform, altmetricsColumns)

