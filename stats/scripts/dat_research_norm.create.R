
inWindow = function
### Returns a list of points in the window
(   x, ##<< Timing information
    windowSize=365  ##<< Size of window, in the timing units
) 
{
	inWindowVals = list()
	
	##details<< For each of the points in the timing vector, specify a list
	##<< The list contains the indicies of all points in the timing vector that are 
	## greater than the giving timing location minus half the window size AND
	## smaller than the given timing location plus half the window size
	for (ii in seq(along=x)) {
		inWindowVals[ii] = list(which((x > (x[ii] - windowSize/2)) & (x < (x[ii] + windowSize/2))))
	}
	
	return(inWindowVals)
	### Returns the indexes of the timepoints that fall within the specified window size 
}

trimmed_meanInWindow = function
### Return a trimmed mean
(whichInWindow, 
    y) 
{
	a = sapply(seq(along=y), function(i, x, y) {mean(y[x[[i]]], na.rm=T, trim=0.10)}, whichInWindow, y)
	return(a)
}

weighted_meanInWindow = function
### Return mean of a weighted window
(whichInWindow, 
    y) {
	a = sapply(seq(along=y), applyWeightedWindow, whichInWindow, y)
	return(a)
}

applyWeightedWindow = function
### Weight the values of a signal within a given time window
## by a moving, tapered window shape
(   i, ##<< index along the time and signal vectors
    x, ##<< vector of time values
    y  ##<< vector of signal amplitudes
) 
{
	##<< Get the timing points that fall in this window
	timing_values = x[[i]]

	z = y
		
	##<< Get the signal values that correspond to the timing values in the window
	signal_values = z[timing_values]

    ##details<< Uses the rms and signal libraries
#HAPP    library(rms)
    library(signal)

	##<< Get the weights for a Hamming window of this size
	window = hamming(length(timing_values))
	
	##<< Calculate dot product of the signal and the window, then 
	##<< standardize the height by dividing by the dotproduct by the area of the window
	inner = (signal_values %*% window)  / sum(ifelse(is.na(signal_values), 0, 1) %*% window)
	
	##<< If there aren't at least 25 values>0 in the window, then not enough data to normalize well,
	##<< so consider the background to be NA.  This will result in a normalized value of NA
	##<< for any points normalized with this background point.
	if (sum(signal_values > 0, na.rm=T) < 25) {
	    inner = NA
    }
    	    
	return(inner)
	### Return the convolved signal
}


get_background = function
### Calculate the background for columns
(dat, 
    cols, ##<< Columns to normalize
    windowSize ##<< window Size in Days
    ) 
{    
    ##details<< Start by sorting by date, so that window will be applied properly
    dat = dat[order(dat$pubDate),]

    ##details<< Calculate which points fall within the window sizes once, 
    ## outside the loop, for efficiency
	whichInWindow = inWindow(dat$daysSincePublished, windowSize)
	
	dat.background = data.frame(pubDate = dat$pubDate)
	##details<< For each column to be normalized, calculate the weighted mean for each point
	for (col in cols) {
		print(col)
		dat.background[,col] = weighted_meanInWindow(whichInWindow, dat[,col])
	}
	
	return(dat.background)
	### Return a data frame containing the background for each point 
	### of each normalized column
}


backgrounds_for_each_journal = function
### Calculate the background vectors for each journal, for each altmetric column
(   dat, ##<< data frame that contains the journal, dates of publication, and altmetrics
    column.names.to.normalize, #<< Columns to normalize
    window_width_in_days=365,  ##<< Window width in days
    journals=NULL ##<< list of journals.  NULL means do all journals in journal.x column
)
{
    ##details<< Get all the names of the journal
    if (length(journals) < 1) {
        journals = names(table(dat$journal))
    }
    
    ##details<< Initialize the list that will hold the background vectors for each journal
    dat.backgrounds = vector("list", length(journals))
    names(dat.backgrounds) = journals
    
    ##details<< For each journal, get the background level
    for (journal in journals) {
    	print(journal)
    	inJournal = which(dat$journal==journal)
    	dat.backgrounds[[journal]] = get_background(dat[inJournal, ], column.names.to.normalize, window_width_in_days)
    } 
    
    return(dat.backgrounds)
    ### Return a list that contains the background vectors for each journal for each altmetric column
}

WINDOW_WIDTH_IN_DAYS = 365

normalize_altmetrics = function
### Normalize the altmetrics
(
    dat, ##<< data frame with eventcounts to normalize
    column.names.to.normalize,  ##<< columns with data to normalize
    dat.backgrounds=NULL, #<< backgrounds
    journals=NULL ##<< list of journals.  NULL means do all journals in journal.x column
)
{
    ##details<< Get all the names of the journal
    if (length(journals) < 1) {
        journals = names(table(dat$journal))
    }
    
    ##details<<  Calculate background levels for each journal 
    if (length(dat.backgrounds) < 1) {
        dat.backgrounds = backgrounds_for_each_journal(dat, column.names.to.normalize, WINDOW_WIDTH_IN_DAYS, journals)
    }
    #save(dat.backgrounds, file="altmetrics.analysis/data/dat_backgrounds.RData", compress="gzip")
        
    #summary(dat.backgrounds)
    #summary(dat.backgrounds[["pbio"]])

    # Plot the background
    # plot_the_backgrounds(dat.backgrounds)

    ##details<< Apply the normalizations for each journal in turn
    ## Initialize the normalized dataframe to match the incoming data frame
    dat.norm = dat
    for (journal in journals) {
    	print(journal)
    	inJournal = which(dat$journal==journal)
    	## then overwrite it with the normalized values
    	dat.norm[inJournal, column.names.to.normalize] = (dat[inJournal, column.names.to.normalize]) / (dat.backgrounds[[journal]][,column.names.to.normalize])
    }
    
    return(dat.norm)
    ### Return a data frame with normalized data
}


### @export "visualization helpers"

get_ranges = function(dat, column.names.to.normalize) {
    yrange = data.frame(column=column.names.to.normalize)
    yrange$rangea = NA
    yrange$rangeb = NA
    for (col in column.names.to.normalize) {
    	yrange$rangea[which(yrange$column==col)] = quantile(dat[,col], c(0.01), na.rm=T)
    	yrange$rangeb[which(yrange$column==col)] = quantile(dat[,col], c(0.99), na.rm=T)
    }
    
    return(yrange)
}

plot_background = function(dat.input, dat.background, cols, title, yrange, colour) {
    dat.input$pubDateVal = strptime(dat.input$pubDate, "%Y-%m-%d")
    dat.input$pubDateVal = as.POSIXct(dat.input$pubDateVal)
    xrange = range(dat.input$pubDateVal)
    
	par(mfrow = c(ceiling(length(cols)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
	for (col in cols) {
		#print(col)
		allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
		plot(xrange, allrange, type="n", main=col)
		points(dat.input$pubDateVal, dat.input[,col], main=col, col="black", pch=20, cex=.5)	
		points(dat.input$pubDateVal, dat.background[,col], col=colour, lwd=3, main=col)
	}
	title(paste("Trends over time ", title), outer=TRUE)	
}


plot_all_backgrounds_separately = function(dat, dat.backgrounds, column.names.to.normalize) {
    journals = names(dat.backgrounds)
    
    # Get ranges so all journals can be plotted on the same axes
    yrange = get_ranges(dat, column.names.to.normalize)

    i = 0
    for (journal in journals) {
    	#print(journal)
    	i = i+1
    	inJournal = which(dat$journal==journal)
    	#quartz()
    	png(paste("img/mean_over_time_figure", i, ".png", sep=""), width=800, height=800)
    	plot_background(dat[inJournal, ], dat.backgrounds[[journal]], column.names.to.normalize, title=journal, yrange, colour=rainbow(length(journals))[i])
    	dev.off()
    }
}

plot_all_backgrounds_overlay = function(dat, dat.backgrounds, column.names.to.normalize) {
    # Get ranges so all journals can be plotted on the same axes
    journals = names(dat.backgrounds)
    yrange = get_ranges(dat, column.names.to.normalize)

    png(paste("img/mean_over_time_all.png", sep=""), width=800, height=800)
    #quartz()
    par(mfrow = c(ceiling(length(column.names.to.normalize)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
    cols = column.names.to.normalize
    dat$pubDateVal = strptime(dat$pubDate, "%Y-%m-%d")
    dat$pubDateVal = as.POSIXct(dat$pubDateVal)

    xrange = range(dat$pubDateVal)
    for (col in cols) {
    	i=0
    	allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
    	plot(xrange, allrange, type="n", main=col)
    	
    	for (journal in journals) {
    		i = i+1
    		inJournal = which(dat$journal==journal)
    		journal.background = dat.backgrounds[[journal]]
    		#quartz()		
    		lines(dat[inJournal, "pubDateVal"], journal.background[,col], col=rainbow(length(journals))[i], lwd=3)
    	}
    }
    #plot(1)
    legend("right", journals, col = rainbow(length(journals)), lty=1, bty="n", fill=rainbow(length(journals)))
    title(paste("Trends over time per journal"), outer=TRUE)
    dev.off()
}



view_distributions = function(dat, dat.norm, column.names.to.normalize) {
    ###Look at the distributions
    i = 0
    for (col in column.names.to.normalize) {
    	i = i+1
    	#quartz()
    	filename = paste("img/altmetrics_distribution_", i, ".png", sep="")
    	png(filename, width=600, height=600)
    	par(mfrow = c(3, 1))
    	titletext = paste(col, "\nNOT normalized", sep="")
    	hist(dat[,col], breaks=50, main=titletext)
    	hist(dat.norm[,col], breaks=50, main=paste(col, "\nnormalized by pubdate within journal", sep=""))
    	hist(log(1+dat.norm[,col]), breaks=50, main=paste("log(1+", col, ")", "\nnormalized by pubdate within journal", sep=""))
    	dev.off()
    }
}

view_distributions_overlay = function(dat, dat.norm, column.names.to.normalize) {
    #quartz()
	png("img/altmetrics_distributions.png", width=600, height=600)
    
    par(mfrow = c(ceiling(length(column.names.to.normalize)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
    
    for (col in column.names.to.normalize) {
    	results = hist(log(1+dat.norm[,col]), breaks=50, main=col, plot=F)
    	barplot(results$counts[-1], main=col)
     }
    title("hist of NON-ZERO results\nnormalized by pubdate within journal, then LOG(1+column)", outer=TRUE)
   
    dev.off()
}

### @export "backgrounds_for_each_journal"

do_norm_viz = function() {
    #data(dat_research)

    journals = NULL  # this will include all
    #journals = c("pmed", "pbio")

    # Could recalculate backgrounds, but this is slow
    #dat.backgrounds = backgrounds_for_each_journal(dat.research, altmetricsColumns, WINDOW_WIDTH_IN_DAYS, journals)
    # Instead reload data from library
    #data(dat_backgrounds)

    plot_all_backgrounds_separately(dat.research, dat.backgrounds, altmetricsColumns)
    plot_all_backgrounds_overlay(dat.research, dat.backgrounds, altmetricsColumns)

    # Could recalculate normalization, but this is slow
    #dat.research.norm = normalize_altmetrics(dat.research, altmetricsColumns, journals)
    # Instead reload data from library
    #data(dat_research_norm)

    view_distributions(dat.research, dat.research.norm, altmetricsColumns)
    view_distributions_overlay(dat.research, dat.research.norm, altmetricsColumns)

    #source("altmetrics.analysis/inst/doc_src/normalization/do_normalization.R")

}

get_dat_research_norm = function() {
    altmetricsColumns = c( "wosCountThru2010", "wosCountThru2011",
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

    WINDOW_WIDTH_IN_DAYS = 365

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
    return(list(dat.backgrounds=dat.backgrounds, 
                dat.research.norm=dat.research.norm, 
                dat.research.norm.transform=dat.research.norm.transform))
}

response = get_dat_research_norm()
dat.backgrounds = response$dat.backgrounds
dat.research.norm = response$dat.research.norm
dat.research.norm.transform = response$dat.research.norm.transform
