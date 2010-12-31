{% set number_figures = 2 %}
{% set num_mean_over_time_figures = 7 %}

<link rel="stylesheet" href="../../assets/pastie.css" />

#Transformation and normalization

##What alt-metrics data do we have?

We have two types of data:

1.  A snapshot of events count data
1.  Events with timing information

**eventcounts running**
{{ d['sections']['scripts/preprocessing.R|idio|rint|pyg']['description eventcounts'] }}

**events running**
{{ d['sections']['scripts/preprocessing.R|idio|rint|pyg']['description events'] }}


##Where did we get the data?

TBD

##Idiocyncricies

TBD

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

- **use all object types to establish baseline?**  I think only Research Articles, to keep apples-apples, since the mix of article types may also have changed over time.  **For simplicity right now the rest of the stats are only using research articles** though this could be changed with a bit of effort to interpolate the baseline
- **should the baseline be journal specific?**  The changing journal mix might be skewing baseline.  Hrm.  I'd rather not, because it would be nice to get "beyond the journal" and not do things that are journal specific, but in this case I think it is required.

**Graphs of event-counts for research articles and the mean event counts within a 180 day window, by journal, over time**

{% for i in range(1,num_mean_over_time_figures+1) %}
{% set filename = ["mean_over_time_figure"]|join("") %}
<img src="http://www.researchremix.org/data/alt-metrics/artifacts/mean_over_time_figure{{i}}.png" align="middle" alt="Mean over time, Figure {{i}}">
{% endfor %}



