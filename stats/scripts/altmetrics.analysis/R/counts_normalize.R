
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
	
	##<< If there aren't at least 50 values>0 in the window, then not enough data to normalize well,
	##<< so consider the background to be NA.  This will result in a normalized value of NA
	##<< for any points normalized with this background point.
	if (sum(signal_values > 0) < 50) {
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
    ##details<< Calculate which points fall within the window sizes once, 
    ## outside the loop, for efficiency
	whichInWindow = inWindow(dat$daysSincePublished, windowSize)

    ##details<< Get the Date of publication in a computable format
    dat$pubDateVal = strptime(dat$pubDate, "%Y-%m-%d")
    dat$pubDateVal = as.POSIXct(dat$pubDateVal)

    ##details<< Start by sorting by date, so that window will be applied properly
    dat = dat[order(dat$pubDateVal),]
	
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
        journals = names(table(dat$journal.x))
    }
    
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

WINDOW_WIDTH_IN_DAYS = 365

normalize_altmetrics = function
### Normalize the altmetrics
(
    dat, ##<< data frame with eventcounts to normalize
    column.names.to.normalize,  ##<< columns with data to normalize
    journals=NULL ##<< list of journals.  NULL means do all journals in journal.x column
)
{
    ##details<<  Calculate background levels for each journal 
    dat.backgrounds = backgrounds_for_each_journal(dat, column.names.to.normalize, WINDOW_WIDTH_IN_DAYS, journals)
    #save(dat.backgrounds, file="altmetrics.analysis/data/dat_backgrounds.RData", compress="gzip")
    
    journals_with_backgrounds = names(dat.backgrounds)
    
    #summary(dat.backgrounds)
    #summary(dat.backgrounds[["pbio"]])

    # Plot the background
    # plot_the_backgrounds(dat.backgrounds)

    ##details<< Apply the normalizations for each journal in turn
    ## Initialize the normalized dataframe to match the incoming data frame
    dat.norm = dat
    for (journal in journals_with_backgrounds) {
    	print(journal)
    	inJournal = which(dat$journal.x==journal)
    	## then overwrite it with the normalized values
    	dat.norm[inJournal, column.names.to.normalize] = dat[inJournal, column.names.to.normalize] / dat.backgrounds[[journal]][,column.names.to.normalize]
    }
    
    return(dat.norm)
    ### Return a data frame with normalized data
}



