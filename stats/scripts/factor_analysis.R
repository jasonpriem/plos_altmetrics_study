bSaveImage = T

# to get latex to work, first in the R GUI go to 
# Misc, start X11 server
# And execute the following line
Sys.setenv( PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":") ) 

library(Rserve)
Rserve(args="--no-save")

#library(plyr)
source("scripts/helper_functions.R")
#source("scripts/preprocessing.R")

### Get just the dataset we want for this
## Restrict to Research Articles
## get rid of other columns we won't be using

excludeColumns = c("journal", "articleType", "authorsCount")
dat.indep.stats = dat.eventcounts[,names(dat.eventcounts) %nin% excludeColumns]
dat.indep.stats = dat.indep.stats[which(as.character(dat.eventcounts$articleType) == "Research Article"),]
summary(dat.indep.stats)

metadataColumns = c("doi", "pubDate", "daysSincePublished", "journal", "articleType", "authorsCount")
altmetricsColumns = names(dat.eventcounts)[names(dat.eventcounts) %nin% metadataColumns]

#### transform

dat.indep.stats.tr = dat.indep.stats
dat.indep.stats.tr[,altmetricsColumns] = tr(dat.indep.stats.tr[,altmetricsColumns])
summary(dat.indep.stats.tr)

###### Normalize


inWindow = function(x, windowSize=60) {
	inWindowVals = list()
	for (ii in seq(along=x)) {
		inWindowVals[ii] = list(which((x > (x[ii] - windowSize/2)) & (x < (x[ii] + windowSize/2))))
	}
	return(inWindowVals)
}

meanInWindow = function(whichInWindow, y) {
	a = sapply(seq(along=y), function(i, x, y) {mean(y[x[[i]]], na.rm=T)}, whichInWindow, y)
	return(a)
}

varInWindow = function(whichInWindow, y, ii) {
	a = sapply(seq(along=y), function(i, x, y) {var(y[x[[i]]], na.rm=T)}, whichInWindow, y)
	return(a)
}



dat.indep.stats.tr.norm = dat.indep.stats.tr
dat.indep.stats.tr.meannorm = dat.indep.stats.tr
dat.background = data.frame(pubDate = dat.indep.stats.tr.norm$pubDate)

inWindow180 = inWindow(dat.indep.stats.tr$daysSincePublished, 180)


for (col in altmetricsColumns) {
	print(col)
	background = meanInWindow(inWindow180, dat.indep.stats.tr[,col])
	background.var = varInWindow(inWindow180, dat.indep.stats.tr[,col])
    dat.indep.stats.tr.norm[,col] = (dat.indep.stats.tr[, col] - background) / background.var
    dat.indep.stats.tr.meannorm[,col] = (dat.indep.stats.tr[, col] - background) 
	dat.background[,col] = background
}

quartz()
par(mfrow = c(ceiling(length(altmetricsColumns)/4), 4), oma=c(2,2,4,2), mar=c(2, 1, 1, 1))
for (col in altmetricsColumns) {
	print(col)
	#pdf(paste("results/timing/", col, ".pdf", sep=""))
	plot(dat.indep.stats.tr$pubDate, dat.indep.stats.tr[,col], main=col, col="black", pch=20, cex=.5)	
	points(dat.indep.stats.tr$pubDate, dat.background[,col], col="red", lwd=5, main=col)
}
title("Trends over time", outer=TRUE)

for (col in altmetricsColumns) {
	print(col)
	#pdf(paste("results/timing/", col, ".pdf", sep=""))

	#quartz()
	#plot(dat.indep.stats.tr$pubDate, dat.indep.stats.tr[, col]^2 - 1, main=col)
	#points(dat.indep.stats.tr$pubDate, dat.background[,col]^2 - 1, col="red", lwd=5, main=col)

	quartz()
	plot(dat.indep.stats.tr.norm$pubDate, dat.indep.stats.tr[,col], main=col, col="black", pch=20, cex=.5)
	points(dat.indep.stats.tr.norm$pubDate, dat.background[,col], col="red", lwd=5, main=col, pch=20, cex=.25)
	par(new=T)
	plot(dat.indep.stats.tr.norm$pubDate, dat.indep.stats.tr.meannorm[,col], main=col, col="blue", pch=20, cex=.5, axes=F)
	axis(side=4)
}
summary(dat.indep.stats.tr.norm)
summary(dat.background)




############## Get correlation matrix

# Need to do special sorts of correlations because have binary data, not just this:
# mycor = rcorr(as.matrix(dat.indep.stats))$r

library(polycor)
#myhetcorr = hetcor.modified(dat.indep.stats, use="complete.obs", std.err=FALSE, pd=FALSE)
myhetcorr = hetcor.modified(dat.indep.stats.tr.meannorm[,altmetricsColumns], 
							use="pairwise.complete.obs", 
							std.err=FALSE, 
							pd=FALSE, 
							type="spearman")
#							type="pearson")
mycor.unadjusted = myhetcorr$correlations
#write.table(mycor.unadjusted,"/Users/hpiwowar/stats link/mycor.unadjusted.txt",append=F,quote=F,sep="\t",row.names=T)

# Are some correlations NA?
which(is.na(mycor.unadjusted))
#mycor.unadjusted[which(is.na(mycor.unadjusted))] = 1

# Now fix the correlation matrix if it is not positive-definite
mycor = adjust.to.positive.definite(mycor.unadjusted)

#display
library(gplots)
colorRange = round(range(mycor) * 15) + 16
colorChoices = bluered(32)[colorRange[1]:colorRange[2]]
if (bSaveImage) png("results/heatmap.png")
	heatmap.2(mycor, col=colorChoices, cexRow=0.9, cexCol = .9, symm = TRUE, dend = "row", trace = "none", margins=c(10,10), key=FALSE, keysize=0.1)
if (bSaveImage) dev.off()

# Now a heatmap with a subsample of articles and the variables
set.seed(42)
dat.subsample = as.matrix(dat.indep.stats[sample(1:dim(dat.indep.stats)[1], 1000, TRUE),altmetricsColumns])
m=32
# using canberra distance because tried several and this one had the most interperable dendrogram
heatmap.2(t(dat.subsample), col=bluered(m*2)[m:(m*2-1)], 
	distfun = function(x) dist(x, method="canberra"), 
	cexRow=.6, cexCol=.6, dend = "both", scale="row", trace="none", 
	margins=c(1,10), key=FALSE, keysize=0.1, breaks=c(0:m)/m)


showpanel <- function(col) {
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )
}
quartz()
showpanel(colorChoices)
showpanel(bluered(32))


############## FIRST ORDER ANALYSIS

##############  Determine number of First-Order factors

# Determine Number of Factors to Extract
library(nFactors)
eigenvectors.1st <- eigen(mycor) # get eigenvalues
# this line takes a long time
aparallel.1st <- parallel(subject=nrow(dat.indep.stats.tr.meannorm), var=ncol(dat.indep.stats.tr.meannorm), rep=100, cent=.05)
scree.results.1st <- nScree(eigenvectors.1st$values, aparallel.1st$eigen$qevpea)
summary(scree.results.1st)
plotnScree(scree.results.1st) 

# Pull out the "Optimal Coordinate"
# defined in nScree help page as 
# The optimal coordinates (OC) corresponds to an extrapolation of the preceding eigenvalue by a regression line between the eignvalue coordinates and the last eigenvalue coordinate
#number.factors.1st = scree.results.1st$Components$noc
number.factors.1st = 5


##############  Do First-Order Factor Analysis

# Maximum liklihood 
fit.ml = factanal(dat.indep.stats.tr.meannorm, number.factors.1st, rotation="oblimin", covmat=mycor)
print(fit.ml, sort=TRUE)


# Use princip axis when maximum liklihood fails to converge:
library(psych)

fit.fa.1st = fa(mycor, number.factors.1st, fm="minres", rotate="oblimin", 
                scores=FALSE, residuals=TRUE, n.obs=max(dim(dat.indep.stats.tr.meannorm)))

#to show the loadings sorted by absolute value
print(fit.fa.1st, sort=TRUE)

#fa.diagram(fit.fa.1st)
#cluster.plot(fit.fa.1st)

# look at residuals
#fit.fa.1st.residuals = fit.fa.1st$residual
#heatmap.2(fit.fa.1st.residuals, col=bluered(16), cexRow=0.5, cexCol = .8, symm = TRUE, dend = "row", trace = "none", main = "Thesis Data", margins=c(15,15), key=FALSE, keysize=0.1)


factor.names.1st = c(
"MR1"="MR1",
"MR2"="MR2",
"MR3"="MR3",
"MR4"="MR4",
"MR5"="MR5")

for (afactor in names(factor.names.1st)) {
    print(paste(afactor, ": ", factor.names.1st[afactor], sep=""))
    print.thresh(fit.fa.1st$loadings[, afactor], .3, TRUE)
    print.thresh(fit.fa.1st$loadings[, afactor], -0.3, FALSE)
}



############## SECOND ORDER ANALYSIS

##############  Determine number of Second-Order factors
# Equations as per Factor Analysis, Second Edition, by Gorsuch

# Correlation of my first order results
fit.fa.1st.cor = fit.fa.1st$r.scores
colnames(fit.fa.1st.cor) = factor.names.1st[colnames(fit.fa.1st$loadings)]
rownames(fit.fa.1st.cor) = factor.names.1st[colnames(fit.fa.1st$loadings)]

eigenvectors.2nd <- eigen(fit.fa.1st.cor) # get eigenvalues
aparallel.2nd <- parallel(subject=nrow(fit.fa.1st.cor), var=ncol(fit.fa.1st.cor), rep=100, cent=.05)
scree.results.2nd <- nScree(eigenvectors.2nd$values, aparallel.2nd$eigen$qevpea)
scree.results.2nd
plotnScree(scree.results.2nd) 

#number.factors.2nd = scree.results.2nd$Components$noc
number.factors.2nd = 3


##############  Do Second-Order Factor Analysis

# Ideally uncorrelated, but want it to be a good fit
#fit.fa.2nd = fa(fit.fa.1st.cor, number.factors.2nd, fa="minres", rotate="varimax")
fit.fa.2nd = fa(fit.fa.1st.cor, number.factors.2nd, fm="minres", rotate="varimax")
print(fit.fa.2nd, sort=TRUE)

#fa.diagram(fit.fa.2nd)
#cluster.plot(fit.fa.2nd)

##############  Map variables directly to second order analysis
# Equations as per Factor Analysis, Second Edition, by Gorsuch

U = fit.fa.2nd$uniquenesses * diag(number.factors.1st)
P = fit.fa.2nd$loadings
A = cbind(P, U)
Pvf = fit.fa.1st$loadings

Pvo = Pvf %*% A

############# HAVE A LOOK AT THESE RESULTS

#Pvo[1:24, 1:4]
# Interesting:  last.author.num.prev.geoae.sharing.tr          0.134139157 -0.066308705  0.138307260 -0.04026715
# what about how it would correlate with our main endpoint?


factor.names.2nd = c(
	"MR1"="Mendeley",
	"MR2"="Citations and downloads",
	"MR3"="Facebook and Comments")

# On original variables
for (afactor in names(factor.names.2nd)) {
    print(paste(afactor, ": ", factor.names.2nd[afactor], sep=""))    
    print.thresh(Pvo[, afactor], .3, TRUE)
    print.thresh(Pvo[, afactor], -0.3, FALSE)
}

# On first-order factors
for (afactor in names(factor.names.2nd)) {
    print(paste(afactor, ": ", factor.names.2nd[afactor], sep=""))
    print.thresh(fit.fa.2nd$loadings[, afactor], .3, TRUE)
    print.thresh(fit.fa.2nd$loadings[, afactor], -0.3, FALSE)
}
########   IMPUTE VARIABLES SO CAN CALCULATE SCORES


# Just want one output variables
#dat$dataset.in.geo.or.ae.int = dat.nums$in.ae.or.geo
#dat.impute.input = dat[,!names(dat) %in% c("dataset.in.geo", "dataset.in.geo.or.ae")]
dat.impute.input = dat.indep.stats.norm

# Show the pattern of NAs
library(mice)
#md.pattern(dat.impute.input)

### Impute variables
mice.output = mice(dat.impute.input, m=1)  # singular

 
# Now flush out the rest of the scores 
dat.imputed = complete(mice.output, 1)
#dat.for.scores = dat.imputed[,!names(dat.imputed) %in% c("dataset.in.geo.or.ae.int")]
dat.for.scores = dat.imputed



###### COMPUTE FIRST ORDER SCORES

scores.1st = factor.scores.bartlett(dat.for.scores, fit.fa.1st)

dat.scores.merge = cbind(dat, scores.1st)

for (afactor in names(factor.names.1st)) {
    print(paste(afactor, ": ", factor.names.1st[afactor], sep=""))
    quartz()
    plsmo(-1*dat.scores.merge$days.since.published, dat.scores.merge[,afactor], ylab=factor.names.1st[afactor], datadensity=TRUE)
    title(factor.names.1st[afactor])
    loaded = names(which(abs(fit.fa.1st$loadings[, afactor]) > 0.3))
    par(new=TRUE) 
    for (loaded_var in loaded) {
        print(loaded_var)
        plsmo(-1*dat.scores.merge$days.since.published, dat.scores.merge[,loaded_var], add=T, datadensity=T, col="red", axes=FALSE)
        axis(4)    
    }
}

    plot(dat.scores.merge$days.since.published, dat.scores.merge[,names(dat.merge) %in% loaded], add=T)

######### SECOND ORDER 


loadings.2nd = Pvo

#fit.pa.2nd.tovars = list(loadings=loadings.2nd[,(1+length(colnames(fit.pa.2nd$weights))):length(colnames(loadings.2nd))])
fit.pa.2nd.tovars = list(loadings=loadings.2nd[,colnames(fit.fa.2nd$loadings)])
fit.pa.2nd.tovars$uniquenesses = apply(fit.pa.2nd.tovars$loadings^2, 1, sum)

scores.to.dat.2nd = factor.scores.bartlett(dat.for.scores, fit.pa.2nd.tovars)


for (afactor in names(factor.names.1st)) {
    print(paste(afactor, ": ", factor.names.1st[afactor], sep=""))
    quartz()
    plsmo(-1*dat.regress$days.since.published, dat.regress[afactor], ylab=factor.names.1st[afactor])
    title(factor.names.1st[afactor])
    loaded = names(which(abs(fit.fa.1st$loadings[, afactor]) > 0.3))
    plot(-1*dat.merge$days.since.published, dat.merge[,names(dat.merge) %in% loaded], add=T)
}

####### MERGE WITH AIM3
load("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats/dat_aim3.RData")

dat.plos = cbind(dat, scores.1st)

dat.merge = merge(dat.plos, dat.aim3, by="pmid")

dat.regress = data.frame(dataset.in.geo.or.ae.int = dat.merge$dataset.in.geo.or.ae.int)
score.names = dimnames(scores.1st)[[2]]
dat.regress[,score.names] = dat.merge[,names(dat.merge) %in% score.names]

vars.indep = c("days.since.published", 
"num.authors.tr",
"first.author.num.prev.pubs.tr", 
"first.author.num.prev.pmc.cites.tr",
"last.author.num.prev.pubs.tr",
"last.author.num.prev.pmc.cites.tr",
"num.grant.numbers.tr",
"pubmed.is.humans",
"pubmed.is.cancer",
"country.usa",
"institution.rank",
"nih.sum.sum.dollars.tr")

dat.regress[,vars.indep] = dat.merge[,vars.indep]






plot(MR2 ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(MR2 ~ dataset.in.geo.or.ae.int, dat=dat.regress, main="MR2")
boxplot(log(MR1) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR2) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR3) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR4) ~ dataset.in.geo.or.ae.int, dat=dat.regress)
boxplot(log(MR5) ~ dataset.in.geo.or.ae.int, dat=dat.regress)

for (afactor in names(factor.names.1st)) {
    print(paste(afactor, ": ", factor.names.1st[afactor], sep=""))
    quartz()
    boxplot(dat.regress[,afactor] ~ dat.regress$dataset.in.geo.or.ae.int)
    title(factor.names.1st[afactor])
}



##############################################################################

####### FIRST ORDER REGRESSION
# Not sure if this will be a primary result


# If I name them here, then setting up the regressions is more awkward due to long names
#names(dat.regress) = c("dataset.in.geo.or.ae.int", factor.names.1st[names(dat.regress)[-1]])

library(rms)
dd.regress = datadist(dat.regress)
options(datadist='dd.regress')
options(digits=2)

f.1st.nonlinear.interactions.reduced = ols(formula = MR2 ~ (dataset.in.geo.or.ae.int +
    (rcs(days.since.aug.2009, 10) +
    rcs(num.authors.tr, 3) ) +
    rcs(first.author.num.prev.pubs.tr, 3) + 
    rcs(first.author.num.prev.pmc.cites.tr, 3) +
    rcs(last.author.num.prev.pubs.tr, 3) + 
    rcs(last.author.num.prev.pmc.cites.tr, 3) + 
    as.numeric(pubmed.is.humans) + 
    as.numeric(pubmed.is.cancer) + 
    as.numeric(country.usa) )
#    rcs(institution.rank, 3) )
    ,
    dat=dat.regress, x=T, y=T)
anova(f.1st.nonlinear.interactions.reduced)

f.1st.nonlinear.interactions.reduced = ols(formula = MR2 ~ (dataset.in.geo.or.ae.int +
    rcs(days.since.aug.2009, 10) + 
    (rcs(num.authors.tr, 3) + 
    rcs(last.author.num.prev.pmc.cites.tr, 3) + 
    as.numeric(pubmed.is.humans) + 
    as.numeric(pubmed.is.cancer) + 
    as.numeric(country.usa) ) )
    #rcs(institution.rank, 3) )
    # which plos journal?
    # watch degrees of freedom!
    # try num.cites.scopus
    # read up about ols.  Am I requiring it to be linear?
    # is MR3 normal?  does it matter?  sqrt it or log it?
    ,
    dat=dat.regress, x=T, y=T)
anova(f.1st.nonlinear.interactions.reduced)

s = summary(f.1st.nonlinear.interactions.reduced)
s
plot(s)




summ.1st.nonlinear.interactions.reduced = summary(f.1st.nonlinear.interactions.reduced)
summ.1st.nonlinear.interactions.reduced.dimnames = dimnames(summ.1st.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.1st.nonlinear.interactions.reduced)[[1]]),2)]
dimnames(summ.1st.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.1st.nonlinear.interactions.reduced)[[1]]),2)] = factor.names.1st[summ.1st.nonlinear.interactions.reduced.dimnames]
plot(summ.1st.nonlinear.interactions.reduced, q = c(0.95), col=gray(0.5), log=T, cex=.8)
title("Multivariate nonlinear regressions with interactions")

### Dots of first-order factors
dat.regress.named = dat.regress
names(dat.regress.named) = c("dataset.in.geo.or.ae.int", factor.names.1st[names(dat.regress)[-1]])

dots.1st.nonlinear.interactions.reduced = summary(dataset.in.geo.or.ae.int ~ .,
    dat=dat.regress.named)
plot(dots.1st.nonlinear.interactions.reduced, cex.labels=0.5, cex=0.7, 
    xlab="Percentage of studies with datasets in GEO or ArrayExpress", 
    main="Univariate data sharing behaviour on first order factors")
#plot.summary.formula.response
#?summary.formula
			


### HERE

dat.regress.2nd = data.frame(dataset.in.geo.or.ae.int = dat.merge$dataset.in.geo.or.ae.int) 
score.names.2nd = dimnames(scores.to.dat.2nd)[[2]]
dat.regress.2nd[,score.names.2nd] = dat.merge[,names(dat.merge) %in% score.names.2nd]

dat.regress.2nd[,vars.indep] = dat.merge[,vars.indep]

library(rms)

dd.regress.2nd = datadist(dat.regress.2nd)
options(datadist='dd.regress.2nd')
options(digits=2)


f.2nd.nonlinear.interactions.reduced = lrm(formula = dataset.in.geo.or.ae.int ~ 
    (rcs(days.since.aug.2009, 10) + rcs(MR1, 3))^2,
    dat=dat.regress.2nd, x=T, y=T)
anova(f.2nd.nonlinear.interactions.reduced)

f.2nd.nonlinear.interactions.reduced = ols(formula = MR1 ~ (dataset.in.geo.or.ae.int +
    rcs(days.since.aug.2009, 10))^2, 
    dat=dat.regress.2nd, x=T, y=T)
anova(f.2nd.nonlinear.interactions.reduced)

f.2nd.nonlinear.interactions.reduced = ols(formula = MR1 ~ (dataset.in.geo.or.ae.int +
    (rcs(days.since.aug.2009, 10) +
    rcs(num.authors.tr, 3) ) +
    rcs(first.author.num.prev.pubs.tr, 3) + 
    rcs(first.author.num.prev.pmc.cites.tr, 3) +
    rcs(last.author.num.prev.pubs.tr, 3) + 
    rcs(last.author.num.prev.pmc.cites.tr, 3) + 
    as.numeric(pubmed.is.humans) + 
    as.numeric(pubmed.is.cancer) + 
    as.numeric(country.usa) )
#    rcs(institution.rank, 3) )
    ,
    dat=dat.regress.2nd, x=T, y=T)
anova(f.2nd.nonlinear.interactions.reduced)


f.2nd.nonlinear.interactions.reduced = lrm(formula = dataset.in.geo.or.ae.int ~ 
    (rcs(MR1, 4) + rcs(MR2, 4))^2,
    dat=dat.regress.2nd, x=T, y=T)
anova(f.2nd.nonlinear.interactions.reduced)

summ.2nd.nonlinear.interactions.reduced = summary(f.2nd.nonlinear.interactions.reduced)
summ.2nd.nonlinear.interactions.reduced.dimnames = dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]]),2)]
dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]][seq(1,length(dimnames(summ.2nd.nonlinear.interactions.reduced)[[1]]),2)] = factor.names.2nd[summ.2nd.nonlinear.interactions.reduced.dimnames]
plot(summ.2nd.nonlinear.interactions.reduced, q = c(0.95), col=gray(0.5), log=T, cex=.8)
title("Multivariate nonlinear regression with interactions")

### Dots of second-order factors
dat.regress.2nd.named = dat.regress.2nd
names(dat.regress.2nd.named) = c("dataset.in.geo.or.ae.int", factor.names.2nd[names(dat.regress.2nd)[-1]])

dots.2nd.nonlinear.interactions.reduced = summary(dataset.in.geo.or.ae.int ~ .,
    dat=dat.regress.2nd.named)
plot(dots.2nd.nonlinear.interactions.reduced, cex.labels=0.5, cex=0.7, 
    xlab="Percentage of studies with datasets in GEO or ArrayExpress", 
    main="Univariate data sharing behaviour\non second order factors")



########

#save.image("image.RData")
#setwd("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats")
#load("image.RData")

#setwd("/Users/hpiwowar/Documents/Code/hpiwowar/pypub/trunk/src/aim3/stats")
#for (iii in 1:4) source("aim3_stats_20100217b.R", echo=TRUE)
