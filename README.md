# Replication code for "Housing policies accelerate energy efficiency participation"

This is the code repository for the Albany Housing Project. The R-code consists of 2 parts. The first file consists of the fixed-effects model and matching algorithms to compare energy use in Albany, GA, among HUD-funded programs' participants and non-participants. The second part includes geographical visualization of the data (R script and supporting files). 


## Installation

The R scripts are written in R 4.0.2 and run protocols for Smart Cities Albany Housing Project: Policy and Program Evaluation of HUD-funded properties and electricity consumption over the span from 2004 to 2019 years. The following R packages are used in this analysis:

- Tidyverse, including readr (read data), dplyr (manipulate data), ggplot2 (create graphics)
5. ggalt
6. gridExtra
7. gdata
8. tibble
9. plm 
10. Matching
11. MatchIt
12. Hmisc
13. rgenoud
14. lmtest
15. sandwich
16. multiwayvcov
17. devtools
18. cem
19. MatchingFrontier
20. censusxy

20. sf
21. tmap
22. tigris
23. rappdirs
24. shinyjs
25. RColorBrewer
26. raster
27. biscale
28. cowplot
29. rgeos
30. maps
31. cartography
32. censusxy
33. plyr


## Data Analysis

Original source files have been pre-processed for analysis. To replicate the results, the two datasets are required:

- `PropertyStats.csv`
- `ELC.csv`

Participating properties are compared to a statistical reference group of non-participating properties using statistical techniques. 

First, the R code `Data_analysis.R` implements the following algorithms to reduce self-selection bias for the estimated treatment effect: propensity score matching, genetic matching, and matching frontier. The matching variables include property age and size in square footage, number of beds and baths, baseline electricity consumption (average consumption over the pre-treatment period), and property market value. These covariates are used to construct counterfactual to the treated units out of properties that did not receive HUD funding. 

The R code `Data_analysis.R` evaluates the overall treatment effect of HUD funding, along with the treatment effect per program and per project, on the monthly electricity consumption (logged normalized by sqft.) from 2004 to 2019. Regression adjustments include time and group fixed effects. The model is implemented to the initial dataset as well as to the datasets after matching, standard errors are clustered at the property ID level. The script also contains the code for the visualizations except the Albany map.

The R code `Map.R` supports the visualization of the Albany map.
