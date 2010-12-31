{% set number_figures = 2 %}
{% set num_mean_over_time_figures = 7 %}

<link rel="stylesheet" href="../../assets/pastie.css" />

#What data do we have?

{
  ".dexy|dexy" : {},
  "../scripts/preprocessing_wos_eventcounts.R|pyg" : {},
  "../scripts/preprocessing_wos_eventcounts.R|idio" : {},
  "../scripts/preprocessing_wos_eventcounts.R|idio|rint" : {},
  "../scripts/preprocessing_wos_eventcounts.R|rart|pyg" : {},
  "../docs_src/data_collection.md|jinja" : { "allinputs": true }
}

{{ d['scripts/preprocessing_wos_eventcounts.R|pyg'] }}

{{ d['scripts/preprocessing_wos_eventcounts.R|idio'] }}

{{ d['sections']['scripts/preprocessing_wos_eventcounts.R|idio']['ISI WoS raw export'] }}

{{ d['scripts/preprocessing_wos_eventcounts.R|rart|pyg'] }}

{{ d['sections']['scripts/preprocessing_wos_eventcounts.R|idio|rint']['ISI WoS raw export'] }}

{{ d['sections']['scripts/preprocessing_wos_eventcounts.R|idio|rint']['ISI WoS extracted event counts'] }}


##What alt-metrics data do we have?

We have two types of data:

1.  A snapshot of events count data
1.  Events with timing information

**eventcounts running**
{{ d['sections']['scripts/preprocessing.R|idio|rint|pyg']['description eventcounts'] }}

**events running**
{{ d['sections']['scripts/preprocessing.R|idio|rint|pyg']['description events'] }}

{% for i in range(1,number_figures+1) %}
** figure {{i}} **
{% set figurename = ["figure", i]|join("") %}
<img src="artifacts/{{ d['a'][figurename] }}" width="400" height="400" align="middle" alt="Figure {{i}}">
{% endfor %}


##Where did we get the data?


##Idiocyncricies


##Preprocessing and normalization

If we are going to compare metrics, we have to normalize the data for each alt-metric across time.  There are two ways that **the date of publication impacts the number of alt-metric events** accumulated by an article, as measured at a given point in time:

1.  Two articles may receive a different number of events **if they were published at times when the alt-metric had a different level of baseline activity**.  The activity levels of an alt-metric service may vary as a field grows or shrinks or a service becomes more or less popular. 
1.  Two articles may also receive a different number of events-to-date **if the articles have had a different amount of time to accumulate events**.  Some metrics, such as citations and bookmarking, might be more sensitive to differing accumulation durations than other metrics.

Given only events-to-date data we can not distinguish these events from each other, but we can normalize for both of them simultaneously.  

To normalize, let's get the mean number of events within a given time window and divide each event size by this "expected" event size to normalize the number of events over time.

**decisions**

- **use mean or median?**  I think the mean. It is a relevant distinction if 60% vs 80% of the articles have zero events:  the medians would be 0 in both cases but the means would differ.
- **transform the event counts before normalization?** I think yes.
- **also normalize variance?** I think no: see graphs below.

###Transformation

Since this is count data, use a square root transformation with a +1 offset, as recommended by:
>  Osborne, Jason (2002). Notes on the use of data transformations. Practical Assessment, Research & Evaluation, 8(6). Retrieved February 11, 2010 from [http://PAREonline.net/getvn.asp?v=8&n=6](http://PAREonline.net/getvn.asp?v=8&n=6), [archived version](http://www.webcitation.org/query?url=http%3A%2F%2Fpareonline.net%2Fgetvn.asp%3Fv%3D8%26n%3D6&date=2010-02-11)

Implemented in R:
{{ d['sections']['scripts/normalize.R|idio']['transformation function'] }}

###Normalization

**decision**

- **use all object types to establish baseline?**  I think only Research Articles, to keep apples-apples.  What about across the full range of journals?  Journal mix might be skewing baseline.  Hrm.  There is definitely an argument for doing a journal-specific normalization, since otherwise as the mix of journals changed so too might the normalization.  Maybe we use the two journal types that have been relatively constant across time to establish baseline.


{% for i in range(1,num_mean_over_time_figures+1) %}
** figure {{i}} **
{% set filename = ["mean_over_time_figure"]|join("") %}
<img src="artifacts/mean_over_time_figure{{i}}.png" align="middle" alt="Mean over time, Figure {{i}}">
{% endfor %}
