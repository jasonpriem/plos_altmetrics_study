<link rel="stylesheet" href="../../assets/pastie.css" />

#Factor Analysis


##What type of factor analysis?

##How many factors?

One ballpark suggests at least 3 variables per factor, so 20 alt-metrics/3 variables is 6-7 factors.  Six factors is consistent with the Scree plot:

<img src="../../artifacts/scree_plot.png" align="middle" alt="Scree plot">


##Factor analysis results


**decisions**

- Using data with journal-specific normalization across time
- Spearman correlations
- all years
- oblique factors to allow correlation between factors

{{ d['sections']['scripts/factor_analysis_embed.R|idio|rint|pyg']['factor analysis'] }}


##What does the factor structure look like?
<img src="../../artifacts/factor_analysis_diagram.png" align="middle" alt="Scree plot">


##How are the factors correlated?
<img src="../../artifacts/heatmap_factors_nodend.png" align="middle" alt="factors">

<img src="../../artifacts/heatmap_factors_dend.png" align="middle" alt="factors">

##What do the factor scores look like?
<img src="../../artifacts/factor_scores_heatmap.png" align="middle" alt="Scree plot">

##What do the residuals look like?
<img src="../../artifacts/heatmap_factor_residuals.png" align="middle" alt="residuals">