
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

	##<< set points in the signal that are NA to 0
	z = y
	z[which(is.na(z))] = 0	
		
	##<< Get the signal values that correspond to the timing values in the window
	signal_values = z[timing_values]

    ##details<< Uses the rms and signal libraries
    library(rms)
    library(signal)

	##<< Get the weights for a Hamming window of this size
	window = hamming(length(timing_values))
	
	##<< Calculate dot product of the signal and the window, then 
	##<< standardize the height by dividing by the dotproduct by the area of the window
	inner = (signal_values %*% window) / sum(window)
	
	return(inner)
	### Return the convolved signal
}


get_background = function
### Calculate the background for columns
(dat.input, 
    cols, ##<< Columns to normalize
    windowSize ##<< window Size (IN WHAT UNITS???)
    ) 
{
    ##details<< Calculate which points fall within the window sizes once, 
    ## outside the loop, for efficiency
	whichInWindow = inWindow(dat.input$daysSincePublished, windowSize)
	
	dat.background = data.frame(pubDate = dat.input$pubDate)
	##details<< For each column to be normalized, calculate the weighted mean for each point
	for (col in cols) {
		print(col)
		dat.background[,col] = weighted_meanInWindow(whichInWindow, dat.input[,col])
	}
	
	return(dat.background)
	### Return a data frame containing the background for each point 
	### of each normalized column
}


backgrounds_for_each_journal = function
### Calculate the background vectors for each journal, for each altmetric column
(   dat, ##<< data frame that contains the journal, dates of publication, and altmetrics
    column.names.to.normalize, #<< Columns to normalize
    window_width_in_days=365  ##<< Window width in days
)
{
    ##details<< Get all the names of the journal
    journals = names(table(dat$journal.x))
    
    ##details<< Initialize the list that will hold the background vectors for each journal
    dat.backgrounds = vector("list", length(journals))
    names(dat.backgrounds) = journals
    
    ##details<< For each journal, get the background level
    for (journal in journals) {
    	print(journal)
    	inJournal = which(dat$journal.x==journal)
    	dat.backgrounds[[journal]] = get_background(dat[inJournal, ], column.names.to.normalize, window_width_in_days)
    } 
    
    return(dat.backgrounds)
    ### Return a list that contains the background vectors for each journal for each altmetric column
}

normalize_altmetrics = function
### Normalize the altmetrics
(
    dat.eventcounts, ##<< data frame with eventcounts to normalize
    column.names.to.normalize  ##<< columns with data to normalize
)
{
    #metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal.x", "articleType", "authorsCount", "journal.y", "articleNumber", "year", "pubDateVal", "pmid", "plosSubjectTags", "plosSubSubjectTags", "title")
    #altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

    ##details<< Get the Date of publication in a computable format
    dat.eventcounts$pubDateVal = strptime(dat.eventcounts$pubDate, "%Y-%m-%d")
    dat.eventcounts$pubDateVal = as.POSIXct(dat.eventcounts$pubDateVal)

    dat = dat.eventcounts

    ##details<< Start by sorting by date, so that window will be applied properly
    dat = dat[order(dat$pubDateVal),]

    # Get ranges so all journals can be plotted on the same axes
    ##get_ranges()

    ##details<<  Calculate background levels for each journal 
    dat.backgrounds = backgrounds_for_each_journal(dat, column.names.to.normalize)
    journals = names(dat.backgrounds)
    
    #summary(dat.backgrounds)
    #summary(dat.backgrounds[["pbio"]])

    # Plot the background
    ##plot_the_backgrounds(dat.backgrounds)

    ##<< Apply the normalizations for each journal in turn
    ## Initialize the normalized dataframe to match the incoming data frame
    dat.norm = dat
    for (journal in journals) {
    	print(journal)
    	inJournal = which(dat$journal.x==journal)
    	## then overwrite it with the normalized values
    	dat.norm[inJournal, column.names.to.normalize] = dat[inJournal, column.names.to.normalize] / dat.backgrounds[[journal]][,column.names.to.normalize]
    }
    
    return(dat.norm)
    ### Return a data frame with normalized data
}


####### BELOW this line was experimentation and visualization

get_ranges = function(dat, column.names.to.normalize) {
    yrange = data.frame(column=column.names.to.normalize)
    yrange$rangea = NA
    yrange$rangeb = NA
    for (col in column.names.to.normalize) {
    	yrange$rangea[which(yrange$column==col)] = quantile(dat[,col], c(0.01), na.rm=T)
    	yrange$rangeb[which(yrange$column==col)] = quantile(dat[,col], c(0.99), na.rm=T)
    }
}

plot_the_background2 = function(dat, dat.background, journals, column.names.to.normalize) {
    i = 0
    for (journal in journals) {
    	print(journal)
    	i = i+1
    	inJournal = which(dat$journal.x==journal)
    	#quartz()
    	png(paste("../artifacts/mean_over_time_figure", i, ".png", sep=""), width=800, height=800)
    	plot_background(dat[inJournal, ], dat.background[[journal]], column.names.to.normalize, title=journal, range(dat$pubDateVal), yrange, colour=rainbow(length(journals))[i])
    	dev.off()
    }

    png(paste("../artifacts/mean_over_time_all.png", sep=""), width=800, height=800)
    par(mfrow = c(ceiling(length(column.names.to.normalize)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
    cols = column.names.to.normalize
    xrange = range(dat$pubDateVal)
    for (col in cols) {
    	i=0
    	allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
    	plot(xrange, allrange, type="n", main=col)
    	for (journal in journals) {
    		i = i+1
    		inJournal = which(dat$journal.x==journal)
    		journal.background = dat.background[[journal]]
    		#quartz()		
    		lines(dat[inJournal, "pubDateVal"], journal.background[,col], col=rainbow(length(journals))[i], lwd=3)
    	}
    }
    title(paste("Trends over time per journal"), outer=TRUE)
    dev.off()
}

plot_background = function(dat.input, dat.background, cols, title, xrange, yrange, colour) {
	par(mfrow = c(ceiling(length(cols)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
	for (col in cols) {
		print(col)
		allrange = c(yrange$rangea[which(yrange$column==col)], yrange$rangeb[which(yrange$column==col)])
		plot(xrange, allrange, type="n", main=col)
		points(dat.input$pubDateVal, dat.input[,col], main=col, col="black", pch=20, cex=.5)	
		points(dat.input$pubDateVal, dat.background[,col], col=colour, lwd=3, main=col)
	}
	title(paste("Trends over time ", title), outer=TRUE)	
}

view_distributions = function(dat, dat.norm, column.names.to.normalize) {
    ###Look at the distributions
    i = 0
    for (col in column.names.to.normalize) {
    	i = i+1
    	#quartz()
    	filename = paste("../artifacts/hist_figure", i, ".png", sep="")
    	png(filename, width=600, height=600)
    	par(mfrow = c(3, 1))
    	titletext = paste(col, "\nnot normalized by pubdate", sep="")
    	hist(dat[,col], breaks=50, main=titletext)
    	hist(dat.norm[,col], breaks=50, main=paste("(1", col, ")", "\nnormalized by mean of 180 day window within journal", sep=""))
    	hist(log(1+dat.norm[,col]), breaks=50, main=paste("log(0.01+", col, ")", "\nnormalized by pubdate", sep=""))
    	dev.off()
    }
}

