{% set num_hist_figures = 20 %}

#What do the alt-metrics look like?

##Number of occurrences

**proportion of research articles with at least one event, by alt-metric**
{{ d['sections']['scripts/description.R|idio|rint|pyg']['research articles with at least one event'] }}

**histogram of research articles by number of alt-metric types for which they have at least one event**

All research articles have at least one event for PDF, html, and xml downloads.  Very few articles have events from all the alt-metrics.
{{ d['sections']['scripts/description.R|idio|rint|pyg']['metrics with at least one event'] }}

Here is a graph:

<img src="../../artifacts/{{ d['a']['hist_research_nonzero_event_counts'] }}" border="1" align="middle" height="400" width="400" alt="metrics with at least one event">

##Distribution of eventcounts by journal
To see the impact of the transformation and normalization, here are distribution plots of event-counts across all the alt-metrics.

{% for i in range(1,num_hist_figures+1) %}
Figure {{i}}
<img src="../../artifacts/hist_figure{{i}}.png" border="1" align="middle" height="400" width="400" alt="metrics with at least one event">

{% endfor %}


##Distribution across time
See the [[results_preprocessing]] page

##Future work
Would be neat to look at distribution across event creators.
