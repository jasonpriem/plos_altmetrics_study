{% set num_mean_over_time_figures = 20 %}

#How correlated are the alt-metrics?

**notes**
These are correlations only include Research Articles.  It would be interesting to look at the difference across article types, too.

##Correlation of articles and alt-metrics

<img src="../../artifacts/heatmap_articles_vs_altmetrics_2008.png" align="middle" alt="Correlation of articles and alt-metrics, 2008">


##How correlated are the alt-metrics to each other

It depends.  Lots of factors.  Some are explored below.

###Effect of normalization and correlation type

These all across all journals, all years:

Not normalized, Pearson:
<img src="../../artifacts/heatmap_altmetrics_pearson_notNormalized.png" align="middle" alt="Heatmap of altmetrics correlations">

Normalized, Pearson:
<img src="../../artifacts/heatmap_altmetrics_pearson_normalized.png" align="middle" alt="Heatmap of altmetrics correlations">

Not normalized, Spearman:
<img src="../../artifacts/heatmap_altmetrics_spearman_notNormalized.png" align="middle" alt="Heatmap of altmetrics correlations">

Normalized, Spearman:
<img src="../../artifacts/heatmap_altmetrics_spearman_normalized.png" align="middle" alt="Heatmap of altmetrics correlations">

With dendrograms (not sure why reordering puts diagonal in opposite direction):
<img src="../../artifacts/heatmap_altmetrics_dend_spearman_normalized.png" align="middle" alt="Heatmap of altmetrics correlations">

##How to the correlations change across different journals?

These are for 2008, using Spearman and normalized data.

{% for journal in ["pbio", "pcbi", "pgen", "pmed", "pone", "ppat"] %}
<img src="../../artifacts/heatmap_altmetrics_{{journal}}2008.png" align="middle" alt="Heatmap of altmetrics correlations">
{% endfor %}


##How to the correlations change over time?

These are for PLoS Biology, using Spearman and normalized data.

{% for year in range(2003,2010) %}
<img src="../../artifacts/heatmap_altmetrics_pbio{{year}}.png" align="middle" alt="Heatmap of altmetrics correlations">

{% endfor %}


##Correlation with events at 2 years
See [[results_prediction]] document.
