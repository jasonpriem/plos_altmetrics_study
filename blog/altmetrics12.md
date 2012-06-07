<!-- pandoc -r markdown -w html -H header.html altmetrics.md  > altmetrics12.html -->

# Altmetrics in the Wild: Using Social Media to Explore Scholarly Impact

Jason Priem(1), Heather A. Piwowar(2), Bradley M. Hemminger(1)

*The first two authors contributed equally to this paper.*

(1) School of Information and Library Science , University of North Carolina at Chapel Hill
(2) National Evolutionary Synthesis Center (NESCent)


##Introduction
In growing numbers, scholars are integrating social media tools like blogs, Twitter, and Mendeley into their professional communications. The online, public nature of these tools exposes and reifies scholarly processes once hidden and ephemeral. Consequently, tracking mentions of scholarly articles across this expanding online landscape could inform new, broader, faster measures of impact, complementing traditional citation metrics.

Before this can be done, however, it is important to explore, describe, and characterize the properties of these social media-based metrics or "altmetrics." 

##Method
We gathered altmetrics and citations on a set of 24,331 articles published by the Public Library of Science (PLoS) between 2003-08-18 and 2010-12-23. Editorials, perspectives, etc. were excluded from the analysis dataset, resulting in 21,096 research articles.

Using the PLoS Article Level Metrics (PLoS ALM) interface and those of independent tools, we gathered metrics for each of these articles.  Sources included:

* Delicious
* Wikipedia
* Mendeley
* Facebook
* Twitter
* CiteULike
* Nature Blogs
* Postgenomic blogs
* Research Blogging blogs
* Scopus citations
* CrossRef citations
* PubMed Central citations
* Monthly pdf, html, and xml views/downloads
* PLoS comments count
* Web of Science citations
* Faculty of 1000 rankings

Approximately 1.8 million altmetric events were gathered in total.  

In addition, in November 2010 and December 2011, we gathered Thomson Reuters Web of Science citation counts manually through the Web of Science website interface.

We normalized these counts by publication date and journal venue.  Normalization was done by dividing each metric value by a weighted average of its neighbors: metrics for articles published in the same journal in the same timeframe. 

##Findings

### No shortage of altmetrics data
There is no shortage of data from altmetrics sources, although different indicators vary greatly in activity. Around 80% of sampled articles were included in at least one Mendeley library, and a quarter of articles have nonzero data from five or more different sources. 

<img src="https://github.com/jasonpriem/plos_altmetrics_study/raw/master/stats/results/figures/figure2.png" class="plot" />

### Citations and altmetric indictors are correlated but independent
Altmetrics and citations track forms of impact that are distinct, but related; neither approach is able to describe the complete picture of scholarly use alone. There are moderate correlations between Mendeley and Web of Science citation (comparable to that between Web of Science and Scopus), but many altmetric indicators seem mostly orthogonal to citation.  

<img src="https://github.com/jasonpriem/plos_altmetrics_study/raw/master/stats/results/figures/figure12.png" class="plot" />

### Altmetrics reveal different "flavors" of impact 
Articles cluster in ways that suggest five different impact “flavors,” capturing impacts of different types on different audiences; for instance, some articles may be heavily read and saved by scholars but seldom cited. Together, these findings encourage more research into altmetrics as complements to traditional citation measures. 

<img src="https://github.com/jasonpriem/plos_altmetrics_study/raw/master/handmade/figure13-edited.png" class="plot" width=600/>

##Conclusions

These results are subject to several important limitations. First, data quality is a challenge, since different services come and go over time. Second, our sample should be carefully considered before attempting to generalize results: PLoS publishes only open-access journals, and the corpus of articles is dominated, particularly in later years, by the huge volume published by PLoS ONE. This unique publication has a volume and scope dwarfing that of traditional journals, inescapably giving rise to similarly unique patterns of readership. 

In 2005, Cronin argued: “It is clear that we will soon have access to a critical mass of web-based digital objects and usage statistics with which to develop multi-dimensional models of scholars’ communication behaviors” [Cronin 2005]. Seven years later, our data suggest that he was correct.

## References

Cronin B (2005) A hundred million acts of whimsy. Current Science 89: 1505 – 1509.

## Data, code, and full article availability

This extended abstract summarizes a full article, which is available on [ArXiv](http://arxiv.org/abs/1203.4745). Datasets and statistics scripts are available [on GitHub](https://github.com/jasonpriem/plos_altmetrics_study) and under a [CCZero](http://creativecommons.org/publicdomain/zero/1.0/) waiver.

##Funding
JP was funded through a UNC Royster Fellowship. HP was funded by the US National Science Foundation through awards to DataONE (OCI-0830944), Dryad (NSF DBI-0743720) and NESCent (EF-0905606).  JP and HP are co-founders of an altmetrics application, [Total-Impact](http://total-impact.org).

