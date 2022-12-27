# Replication code for "Housing policies accelerate energy efficiency participation"

This is the code repository for the Albany Housing Project. The R-code consists of 2 parts. The first file consists of the fixed-effects model and matching algorithms to compare energy use in Albany, GA, among HUD-funded programs' participants and non-participants. The second part includes geographical visualization of the data (R script and supporting files). The data is deposited at [https://doi.org/10.7910/DVN/SF1DRW](https://doi.org/10.7910/DVN/SF1DRW).


## Installation

The R scripts are written in R 4.0.2 and run protocols for Smart Cities Albany Housing Project: Policy and Program Evaluation of HUD-funded properties and electricity consumption over the span from 2004 to 2019 years. The following R packages are used in this analysis:

### Data handling
- 'tidyverse', including 'readr' (read data), 'tibble' (update dataframes), 'dplyr' (manipulate data), 'ggplot2' (create graphics)
- 'ggalt' (extra coordinate systems, 'geoms', statistical transformations, scales and fonts for 'ggplot2')
- 'gdata' (data manipulation)
- 'censusxy' (U.S. Census Bureau's API for matching American street addresses with their longitude and latitude)

### Matching algorithms
- 'MatchIt' (multivariate matching)
- 'cem' (loss function computation)
- 'devtools' (developer tools such as indtallation from GitHub)
- 'MatchingFrontier' (computation of the balance - sample size frontier)

### Panel data analysis
- 'plm' (linear models for panel data) 
- 'lmtest' (testing linear regression models)
- 'sandwich' (robust and cluster errors)
- 'multiwayvcov' (multi-way clustering)
- 'Hmisc' (miscellaneous functions for data analysis)
- 'did' (difference-in-differences (DiD) design by Callaway and Sant’Anna (2021))
- 'DIDmultiplegt' (difference-in-differences (DiD) design by Chaisemartin and D'Haultfoeuille (2020))
- 'TwoWayFEWeights' (treatment effect heterogeneity estimation)

### Visualization
- 'cowplot' (streamlined plot theme and plot annotations for 'ggplot2')
- 'gridExtra' (miscellaneous functions for "grid" graphics)
- 'sf' (spatial vector data encoder)
- 'tmap' (visualization of spatial data distributions)
- 'tigris' (download shapefiles)
- 'rappdirs' (data saving)
- 'shinyjs' (call custom JavaScript functions from R)
- 'RColorBrewer' (color palettes)
- 'raster' (high-level functions for raster and vector data)
- 'biscale' (tools and palettes for mapping)
- 'rgeos' (Open Source ('GEOS') using the C 'API' for topology operations on geometries)
- 'maps' (display of maps)
- 'cartography' (map integration)


## Data Analysis

Original source files have been pre-processed for analysis. To replicate the results, the two datasets are required:

- `PropertyStats.csv`
- `ELC.csv`

Participating properties are compared to a statistical reference group of non-participating properties using statistical techniques. 

First, the R code `Data_analysis.R` implements the following algorithms to reduce self-selection bias for the estimated treatment effect: propensity score matching, genetic matching, and matching frontier. The matching variables include property age and size in square footage, number of beds and baths, baseline electricity consumption (average consumption over the pre-treatment period), and property market value. These covariates are used to construct counterfactual to the treated units out of properties that did not receive HUD funding. 

The R code `Data_analysis.R` evaluates the overall treatment effect of HUD funding, along with the treatment effect per program and per project, on the monthly electricity consumption (logged normalized by sqft.) from 2004 to 2019. Regression adjustments include time and group fixed effects. The model is implemented to the initial dataset as well as to the datasets after matching, standard errors are clustered at the property ID level. The script also contains the code for the visualizations except the Albany map.

The folder `Map` consists of `Map.R` that executes the map code and shape files for Federal Opportunity Zones and Tract and City Boundaries, and supports the visualization of the map of Albany.
