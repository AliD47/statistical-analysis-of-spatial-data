# LBRTI2101A-Statistical-analysis-of-spatial-data

This project analyzes groundwater depth in the Bakersfield region of California (2015–2021) using spatial and temporal statistical methods.
The objective is to detect spatial dependencies, identify outliers, and generate interpolated maps of groundwater depth while accounting for the effect of terrain elevation.


## Files
* [DEM_grid.csv](DEM_grid.csv) :  Digital Elevation Model (DEM) grid providing terrain elevation data.
* [monthly_mean_delimited.csv](monthly_mean_delimited.csv) : Main dataset containing groundwater depth measurements.
* [grid.csv](grid.csv) : Spatial grid used for the elevation.
* [projet_spatial_data.rmd](projet_spatial_data.rmd) : Main script with the statistical analyses (R markdown file).

## How to run the code
Install R (version 4.4.1 or later) and the required R packages:
```R
install.packages(c("ggplot2", "ggnewscale", "ggrepel", "gridExtra", 
                   "car", "MASS", "PerformanceAnalytics", "tidyverse", 
                   "viridis", "lubridate", "data.table", "fields", 
                   "gstat", "pander", "visreg", "matrixStats"))
```
and then just be sure to change the working directory in the line 38:
```R
setwd("C:/Users/Administrator/Desktop/STUDY/UCL/spacial stats")
```

---
### Contributors
* [Ayadi Youmna](https://github.com/YoumnaAyadi/YoumnaAyadi)
* Cleenewerck de Crayencour Harold
* [Abdelali Dssam](https://github.com/AliD47)
* [El Houda Teber Nour](https://github.com/NOUREL-art)
