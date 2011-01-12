{% set num_mean_over_time_figures = 20 %}

#How correlated are the alt-metrics?

**notes**
These are correlations only include Research Articles.  It would be interesting to look at the difference across article types, too.


##How correlated are the alt-metrics to each other

It depends.  Lots of factors.  Some are explored below.


##How to the correlations change across different journals?  How to they change over time?

Using Pearson correlations on evencounts that have been transformed and normalized on a per alt-metrics basis, by journal over time.

Sizes:
<pre>
	> table(dat$year)

	2003 2004 2005 2006 2007 2008 2009 2010 
	  27  178  397  881 2193 4007 6042 6219 
	
	
	> table(dat$journal.x)

	 pbio  pcbi  pgen  pmed  pntd  pone  ppat 
	 1266  1289  1547   633   562 13271  1376 
	 
	
	> table(dat$journal.x, dat$year)

	       2003 2004 2005 2006 2007 2008 2009 2010
	  pbio   27  166  172  187  195  196  181  142
	  pcbi    0    0   54  135  206  250  342  302
	  pgen    0    0   65  169  207  325  436  345
	  pmed    0   12   71  158  130  116   78   68
	  pntd    0    0    0    0   28  133  179  222
	  pone    0    0    0  137 1253 2717 4403 4761
	  ppat    0    0   35   95  174  270  423  379
	> 
</pre>	

<table>
<tr>
<td>journal</td>
<td>all years</td>
{% for year in range(2004,2011) %}
<td>{{year}}</td>	
{% endfor %}

</tr>
{% for journal in ["all", "pbio", "pcbi", "pgen", "pmed", "pone", "ppat", "pntd"] %}

<tr>
<td>{{journal}}</td>

<td>
	<img src="../../artifacts/heatmap_altmetrics_{{journal}}all.png" align="middle" alt="Heatmap of altmetrics correlations for {{journal}}, {{year}}" height="100" width="100">
</td>

{% for year in range(2004,2011) %}
<td>

<img src="../../artifacts/heatmap_altmetrics_{{journal}}{{year}}.png" align="middle" alt="Heatmap of altmetrics correlations for {{journal}}, {{year}}" height="100" width="100">
{% endfor %}


</td>
</tr>
{% endfor %}
</table>



##Correlation with events at 2 years
See [[results_prediction]] document.

##Correlation of articles and alt-metrics

<img src="../../artifacts/heatmap_articles_vs_altmetrics_2008.png" align="middle" alt="Correlation of articles and alt-metrics, 2008">


###Effect of normalization and correlation type

These all across all journals, all years:

Not normalized, Pearson:
<img src="../../artifacts/heatmap_altmetrics_pearson_notNormalized.png" align="middle" alt="Heatmap of altmetrics correlations" height="200" width="200">

Normalized, Pearson:
<img src="../../artifacts/heatmap_altmetrics_pearson_normalized.png" align="middle" alt="Heatmap of altmetrics correlations" height="200" width="200">

Not normalized, Spearman:
<img src="../../artifacts/heatmap_altmetrics_spearman_notNormalized.png" align="middle" alt="Heatmap of altmetrics correlations" height="200" width="200">

Normalized, Spearman:
<img src="../../artifacts/heatmap_altmetrics_spearman_normalized.png" align="middle" alt="Heatmap of altmetrics correlations" height="200" width="200">

With dendrograms (not sure why reordering puts diagonal in opposite direction):
<img src="../../artifacts/heatmap_altmetrics_dend_spearman_normalized.png" align="middle" alt="Heatmap of altmetrics correlations" height="200" width="200">
