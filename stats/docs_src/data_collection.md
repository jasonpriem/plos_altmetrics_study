#Data:  Data collection details

*This page is part of the [[documentation|Home]] on stats for the PLoS alt-metrics study.*

##Overview

Alt-metrics data is being continually collected ([[crawler source|https://github.com/jasonpriem/plos_altmetrics_study/tree/master/crawler]]) and added to a [[live CouchDB database|http://72.14.190.127:5984/_utils/database.html?am]].  

A snapshot of this data was taken on December 1 2010 (**verify date**).  This snapshot data was supplemented with manually-exported ISI Web of Science citations. 

##ISI Web of Science data collection

###Types of data
We collected two types of data related to ISI Web of Science:

1.  Total citation counts for all PLoS articles 
2.  Individual citation instances for PLoS articles published in 2008

###Raw data collection
The data was collected October 31-mid November 2010, so includes all citations until those dates.

ISI Web of Science data can not be automatically extracted as per terms of use.  As a result, we manually queried ISI Web of Science for articles with journal "PLoS*".

**Data collection for Total citation counts for all PLoS articles**
We exported the detailed ISI Web of Science citation information for the 22k PLoS articles, 500 articles at a time (maximum permitted by ISI website).  The exported data looks like this:

    > head ../../alt-metrics_stats/data/raw/isi_wos.txt 

    PT	AU	BA	ED	GP	AF	CA	TI	SO	SE	LA	DT	CT	CY	CL	SP	HO	DE	ID	AB	C1	RP	EM	FU	FX	CR	NR	TC	PU	PI	PA	SN	BN	DI	J9	JI	PD	PY	VL	IS	PN	SU	SI	BP	EP	AR	DI	PG	SC	GA	UT

    J	Sun, QX; Jackson, RA; Ng, C; Guy, GR; Sivaraman, J			Sun, Qingxiang; Jackson, Rebecca A.; Ng, Cherlyn; Guy, Graeme R.; Sivaraman, J.	Additional Serine/Threonine Phosphorylation Reduces Binding Affinity but Preserves Interface Topography of Substrate Proteins to the c-Cbl TKB Domain	PLOS ONEnglish	Article							GROWTH-FACTOR RECEPTOR; TYROSINE KINASE INHIBITORS; CELL LUNG-CANCER; EGF RECEPTOR; TERMINAL DOMAIN; DOWN-REGULATION; SERINE; ACTIVATION; MUTATIONS; SPROUTY	The E3-ubiquitin ligase, c-Cbl, is a [...] investigations.	[Sun, Qingxiang; Sivaraman, J.] Natl Univ Singapore, Dept Biol Sci, Singapore 0511, Singapore; [Jackson, Rebecca A.; Ng, Cherlyn; Guy, Graeme R.] Biopolis, Inst Mol & Cell Biol, Singapore, Singapore	Sun, QX, Natl Univ Singapore, Dept Biol Sci, Singapore 0511, Singapore	mcbgg@imcb.a-star.edu.sg; dbsjayar@nus.edu.sg	Biomedical Research Council (BMRC) [BMRC 05/1/21/19/385]; Singapore Millennium Foundation (SMF) 	This work was supported by the Biomedical Research Council (BMRC) grant (BMRC 05/1/21/19/385). Sun Qingxiang is a graduate student of the National University of Singapore, receiving a Singapore Millennium Foundation (SMF) scholarship (http://www.smf-scholar.org/). The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript.	ADACHI S, 2009, CARCINOGENESIS, V30, P1544, DOI 10.1093/carcin/bgp166; BELL DW, 2005, J CLIN ONCOL, V23, P8081, DOI 10.1200/JCO.2005.02.7078; CHIANG SH, 2001, NATURE, V410, P944; COUNTAWAY JL, 1992, J BIOL CHEM, V267, P1129; DELANO WL, 2002, PYMOL MOL GRAPHICS S; DIKIC I, 2003, BIOCHEM SOC T 6, V31, P1178; EMSLEY P, 2004, ACTA CRYSTALLOGR  12, V60, P2126, DOI 10.1107/S0907444904019158; [..] ; WONG ESM, 2002, EMBO J, V21, P4796; WONG ESM, 2006, METH MOL B, V327, P61; YARDEN Y, 1987, BIOCHEMISTRY-US, V26, P1434; YARDEN Y, 1987, BIOCHEMISTRY-US, V26, P1443	40	0	PUBLIC LIBRARY SCIENCE	SAN FRANCISCO	185 BERRY ST, STE 1300, SAN FRANCISCO, CA 94107 USA	1932-6203		10.1371/journal.pone.0012819	PLOS ONE	PLoS One	SEP 22	2010	5	9		e12819		11	Biology; Multidisciplinary Sciences	653AC	ISI:000282053100012

**Data collection for Individual citation instances for PLoS articles published in 2008**

In ISI web of Science, we then filtered the PLoS articles to look at only those published in 2008.  We chose to look at their "Cited By" articles:  all the articles, indexed by ISI WoS, that cite the 2008 PLoS articles.  We exported these "Cited By" articles 500 at a time.  The details about which PLoS articles were cited by which "Cited By" articles were captured in the References field of the "Cited By" article export (format same as above).

At this time, pending clarification of ISI terms of use, we are not posting these complete data extractions.

###Data preprocessing
The raw data extractions required preprocessing to get it into a format we could use for statistics.

**Preprocessing for Total citation counts for all PLoS articles**

The individual data collection files were all concatenated with cat (without moving the repeated headers, whoops.)

Extracting Total citation counts was done by [[preprocessing_wos_eventcounts.R|https://github.com/jasonpriem/plos_altmetrics_study/blob/master/stats/scripts/preprocessing_wos_eventcounts.R]].  This simply involved extracting columns for DOI and citation count. 

{{ d['sections']['scripts/preprocessing_wos_eventcounts.R|idio|rint|pyg']['ISI WoS preprocessing'] }}

*TODO:  The NAs are due to the repeated headers.  Need to remove.*

The resulting file is [[available in the downloads directory|https://github.com/jasonpriem/plos_altmetrics_study/downloads]].

**Preprocessing for Individual citation instances for PLoS articles published in 2008**

Extracting Individual citation instances for PLoS articles published in 2008 was done in a few steps:

1.  Concatenate all the individual data collection files together (cat)
2.  Extract all of the PLoS references out of the references fields.  Done in Python in [[preprocessing_parse_wos_events.py|https://github.com/jasonpriem/plos_altmetrics_study/blob/master/stats/scripts/preprocessing_parse_wos_events.py]]
3.  Preprocess these events into the same format as the rest of the alt-metrics.  Done in R in [[preprocessing_wos_events.R|https://github.com/jasonpriem/plos_altmetrics_study/blob/master/stats/scripts/preprocessing_wos_events.R]].  

**notes**
Because references are so heterogeneous, this information is quite dirty.

{{ d['sections']['scripts/preprocessing_wos_events.R|idio|rint|pyg']['ISI WoS preprocessing'] }}

