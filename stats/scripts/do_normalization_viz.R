
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