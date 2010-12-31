#Can they predict citations?


##How early can the prediction be useful?

- Spearman correlations of alt-metrics after n months (1, 2, 3, 4, 5, 6, 12, 18, 24) to Web of Science counts at 2 years.
- normalized over time
- uses research articles published in 2008
- I think only PLoS ONE?  Have to check...

**takeaways**

- correlations strongest to pdf views, html views
- correlations 80-90% steady state levels at 3-6 months

<img src="http://researchremix.org/data/alt-metrics/artifacts/correlations_over_time.png" border="1" align="middle" height="400" width="400" alt="metrics with at least one event">


<pre>
	> aggregate(sh[,3:10], by=list(sh$month), mean, na.rm=T)
	  Group.1 cor.with.wos.backtweets cor.with.wos.citeulike cor.with.wos.delicious
	1       1                     NaN           -0.023137085           1.056851e-02
	2       2                     NaN           -0.000561967           3.956344e-03
	3       3                     NaN            0.014740978           8.235083e-03
	4       4                     NaN            0.014093062           6.946462e-03
	5       5                     NaN            0.022902863          -9.675288e-04
	6       6                     NaN            0.015026802           2.950992e-03
	7      12            -0.029996495            0.038714127           2.135271e-03
	8      18             0.013978084            0.042980817           7.802498e-05
	9      24            -0.007498423            0.047679973           9.220793e-03
	  cor.with.wos.html views cor.with.wos.native comments cor.with.wos.pdf views cor.with.wos.wos
	1               0.1816000                   0.02143652              0.2422599                1
	2               0.2436401                   0.03503153              0.3068158                1
	3               0.2663671                   0.03073339              0.3385095                1
	4               0.2767338                   0.03163433              0.3585233                1
	5               0.2826667                   0.03029573              0.3710055                1
	6               0.2907202                   0.02769871              0.3860938                1
	7               0.3046234                   0.03110171              0.4146792                1
	8               0.3077868                   0.03196143              0.4267892                1
	9               0.3124597                   0.03061699              0.4315896                1
	  cor.with.wos.xml views
	1             0.07748482
	2             0.11747502
	3             0.13276559
	4             0.13765472
	5             0.13592124
	6             0.13587894
	7             0.13238731
	8             0.13943532
	9             0.13110190

</pre>
##How do the correlations to citations change over time?

As above, Spearman correlations of alt-metrics after n months (1, 2, 3, 4, 5, 6, 12, 18, 24) to Web of Science counts at 2 years.  One row for every given n.  Each correlation includes articles published in a 30 day period.  The different columns are different samples across time (so the articles published in March 2008 vs July 2008 vs Sept 2008, for example)... would  expect them to exhibit similar patterns to each other, if the patterns are generalizable to new samples.


<table>
{% for i in [1,2,3,4,5,6,12,18,24] %}
<tr>
<td>
Month {{i}}
</td>
</tr><tr>

{% for ii in [700,820,910] %}
<td>
<img src="http://researchremix.org/data/alt-metrics/artifacts/heatmap_cor_with_wos{{i}}_{{ii}}.png" border="1" align="middle" height="300" width="300" alt="metrics with at least one event">
</td>
{% endfor %}
</tr>
{% endfor %}
</table>

##How predictive are alt-metrics to citations?

Finally, let's have a look at the 3 month mark, to see how predictive.  Exclude html counts for now because they are so correlated with pdf counts.  Two examples, again different months in 2008, to show how different the result can be across different samples.  Not quite sure what to think about this yet.  Will pull in more data to show the trend in greater detail.

###for one month in 2008

<pre>
Call:
lm(formula = wos ~ ., data = event_counts_month[, c("citeulike", 
    "delicious", "backtweets", "native comments", "pdf views", 
    "xml views", "wos")])

Residuals:
     Min       1Q   Median       3Q      Max 
-14.1260  -1.7367  -0.8156   0.8177  19.1269 

Coefficients: (1 not defined because of singularities)
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.6302384  0.1388825  11.738   2e-16 ***
citeulike          0.2879915  0.1527419   1.885 0.059738 .  
delicious         -0.5398294  0.1165851  -4.630 4.28e-06 ***
backtweets                NA         NA      NA       NA    
`native comments` -0.4474783  0.1283612  -3.486 0.000518 ***
`pdf views`        0.0089455  0.0010101   8.856   2e-16 ***
`xml views`       -0.0005396  0.0010900  -0.495 0.620719    

Residual standard error: 2.786 on 778 degrees of freedom
Multiple R-squared: 0.102,	Adjusted R-squared: 0.09626 
F-statistic: 17.68 on 5 and 778 DF,  p-value:  2.2e-16

</pre>

###another month

<pre>
Call:
lm(formula = wos ~ ., data = event_counts_month[, c("citeulike", 
    "delicious", "backtweets", "native comments", "pdf views", 
    "xml views", "wos")])

Residuals:
    Min      1Q  Median      3Q     Max 
-7.9553 -1.4774 -0.5622  0.8499 17.5155 

Coefficients: (1 not defined because of singularities)
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        0.6667296  0.1977217   3.372 0.000793 ***
citeulike         -0.0257847  0.2110989  -0.122 0.902824    
delicious         -0.9516969  0.1876148  -5.073  5.2e-07 ***
backtweets                NA         NA      NA       NA    
`native comments`  0.0040945  0.0802297   0.051 0.959315    
`pdf views`        0.0108346  0.0008884  12.196   2e-16 ***
`xml views`        0.0018684  0.0008381   2.229 0.026157 *  


Residual standard error: 2.506 on 616 degrees of freedom
Multiple R-squared: 0.2229,	Adjusted R-squared: 0.2166 
F-statistic: 35.35 on 5 and 616 DF,  p-value: 2.2e-16

</pre>

