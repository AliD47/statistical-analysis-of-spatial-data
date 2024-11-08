setwd("C:/Users/abdel/Desktop/spacial stats")
library(ggplot2) 
library(gridExtra) 
library(car) 
library(MASS) 
library(PerformanceAnalytics) 
library(stats) 
library(tidyverse) 
library(viridis) 
library(lubridate) 
library(data.table)
library(fields) 
library(gstat)
theme_set(theme_classic()) 

#grid <- read.csv("grid.csv")
#MNT <- read.csv("DEM_grid.csv")
#data <- read.csv("monthly_mean_delimited.csv")

monthly_mean_delimited <- read.csv(file = "monthly_mean_delimited.csv", header = TRUE, sep = ",", row.names = 1) 
DEM_grid <- read.csv(file = "DEM_grid.csv", header = TRUE, sep = ",") 



summary(data)
f_data <- data[!data$year %in% c(2015, 2016, 2017, 2018, 2019, 2020), ]

f1_data <- subset(data, (year %in% c(2015, 2016, 2017, 2018, 2019, 2020) and data$month == 10))

f1_data <- data[data$year == c(2015,2016,2017,2018,2019,2020,2021) & data$month == 10 & data$site_code == "350253N1189496W001", ]

ggplot(data = MNT, aes(x = x, y = y)) +
  geom_point(aes(color = elevation), size = 4, shape = 19) + 
  scale_colour_distiller(palette = "Spectral") +
  geom_point(data = data ,aes(x = x, y = y)) + 
  labs(color = "Elevation") +   # Add a color legend label for clarity
  theme_minimal()


