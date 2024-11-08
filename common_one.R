

# list of packages used for running the code chunks below 

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
library(visreg)
theme_set(theme_classic()) 

#setwd("~/Documents/ ") 
setwd("C:/Users/abdel/Desktop/spacial stats")
# Loading the data 

monthly_mean_delimited <- read.csv(file = "monthly_mean_delimited.csv", header = TRUE, sep = ",", row.names = 1) 
DEM_grid <- read.csv(file = "DEM_grid.csv", header = TRUE, sep = ",") 

head(monthly_mean_delimited) 

str(monthly_mean_delimited) 

# Valeurs mesurées durant les mois d’octobre entre 2015 et 2021 

october_mean <- monthly_mean_delimited[monthly_mean_delimited$month == 10 & monthly_mean_delimited$year %in% 2015:2021,] 
summary(october_mean)

# Grouping the data into the different stations and calculating the mean depth for each station 

mean_depth <- october_mean %>%  
  group_by(site_code, x, y, longitude, latitude) %>%  
  summarise(site_mean_depth = mean(mean_gse_gwe)) 

# Converting the tibble object into a data frame object 

mean_depth <- as.data.frame(mean_depth) 
head(mean_depth) 

# Selecting the station 353539N1191118W001 

selected_station = mean_depth[mean_depth$site_code == "353539N1191118W001",] 

# Dessin de la carte des profondeurs moyennes + altitude + encercler point de station  

ggplot(mean_depth) + 
  geom_tile(data = DEM_grid, aes(x = x, y = y, fill = elevation)) + 
  scale_fill_gradient(name = "Elévation du sol [pieds]", low = "white", high = "black") + 
  geom_point(aes(x = x, y = y, color = site_mean_depth), size = 2) + 
  scale_color_viridis(name = "Profondeur moyenne \n de la nappe \n phréatique [pieds]", option = 'turbo') + 
  geom_point(data = selected_station, aes(x, y), 
             pch = 21, fill = NA, size = 4, colour = "red", stroke = 1) + 
  labs(x = "x" , y = "y" , title = "Carte de la profondeur moyenne de la nappe phréatique en octobre \n entre 2015 et 2021 à toutes les stations de mesure \n et élévation du sol autour de Bakersfield") + 
  theme(plot.title = element_text(hjust = 0.5)) 

hist(mean_depth$site_mean_depth)
hist(log(mean_depth$site_mean_depth))


# Série temporelle profondeur nappe 

station_monthly_mean <- monthly_mean_delimited[monthly_mean_delimited$site_code == "353539N1191118W001",] 
station_monthly_mean$date <- as.Date(paste(station_monthly_mean$year, station_monthly_mean$month, "01", sep="-"), "%Y-%m-%d") 

head(station_monthly_mean) 

# Plotting of the time series 

ggplot(station_monthly_mean) + geom_point(aes(x = date, y = mean_gse_gwe)) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  scale_y_reverse( ) +
  labs(x = "Date", y = "Profondeur moyenne mensuelle \n de la nappe phréatique [pieds]") +
  ggtitle("Niveau de la nappe phréatique pour la station 353539N1191118W001") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90,  hjust = 0.5, vjust = 0.5)) 



###____________###-------------###____________###-------------###____________###-------------###
# Load required packages
library(dplyr)

# Join the datasets based on X and Y coordinates
merged_data <- station_monthly_mean %>%
  left_join(DEM_grid, by = c("x" = "x", "y" = "y"))

# Identify rows in station_monthly_mean that have no corresponding elevation data in DEM_grid
non_matching_coords <- merged_data %>%
  filter(is.na(elevation))

# Display the non-matching coordinates
if (nrow(non_matching_coords) == 0) {
  print("All coordinates in station_monthly_mean have matching elevation data in DEM_grid.")
} else {
  print("Some coordinates in station_monthly_mean do not match any elevation data in DEM_grid:")
  print(non_matching_coords[, c("x", "y")])
}

# Directly merge the datasets based on matching x and y values
matched_data <- station_monthly_mean %>%
  inner_join(DEM_grid, by = c("x", "y"))

# View the resulting matched dataset to verify
head(matched_data)

###____________###-------------###____________###-------------###____________###-------------###

                  #### Modèles de régression linéaire ####

# Créer un fonction qui fait la régression linéaire pour n'importe quelle année entre 2015 et 2021: 

linear_reg <- function(year){ 
  
    yearly_october_mean <- october_mean[october_mean$year == year,] 
    
    stations_grid <- yearly_october_mean[c("x","y")] 
    
    elevation_voronoi <- idw(formula = elevation ~ 1, data = DEM_grid,
                           locations = ~ x + y, newdata = stations_grid,
                           nmax = 1)
  
    yearly_october_mean$elevation <- elevation_voronoi$var1.pred 
  
    mod <- lm(mean_gse_gwe ~ elevation, data = yearly_october_mean) 
  
  # Printing the results 
  
    print(summary(mod)) 
  
    print("Confindence intervals") 
  
    print(confint(mod)) 
  
    print(anova(mod)) 
  
  # Checking if the linear regression hypotheses are met 
    
  # (normal distribution of the residues, homoscedasticity) 
  
    par(mfrow=c(2,2)) 
    plot(mod) 

  # Visualising the linear regression 
  
    par(mfrow=c(1,1)) 
    visreg(mod, main = paste("Visualisation de la droite de régression \n et son intervalle de confiance pour l'année", year)) 
  
  # Calculating the predicted depth from the linear model 
    xpred = data.frame(elevation = yearly_october_mean$elevation) 
    pred <- predict(mod, xpred, interval = "prediction") 
    yearly_october_mean$depth.pred <- pred 
    yearly_october_mean$depth.res <- residuals(mod) 
    
    print(summary(mod)$r.squared)
    print(summary(mod)$adj.r.squared)
    return(yearly_october_mean) 
  
}



# Fonction qui dessine la répartition spatiale de la profondeur de la nappe avant et après avoir retiré l’effet de l’altitude du sol 

residuals_plot <- function(yearly_october_mean){ 
  
  year = yearly_october_mean$year[1] 
  
  plot_mean_depth <- ggplot(yearly_october_mean) + 
    
    geom_point(aes(x = x, y = y, color = mean_gse_gwe), size = 2) + 
    
    scale_color_viridis(name = "Profondeur moyenne \n de la nappe \n phréatique [pieds]", option = 'turbo') + 
    
    labs(x = "x" , y = "y" , title = paste("Carte de la profondeur moyenne de la nappe phréatique en octobre", year,"\n à toutes les stations de mesure autour de Bakersfield")) + 
    
    theme(plot.title = element_text(hjust = 0.5)) 
  
  plot_res <- ggplot(yearly_october_mean) + 
    
    geom_point(aes(x = x, y = y, color = depth.res), size = 2) + 
    
    scale_color_viridis(name = "Résidus de la \n profondeur moyenne \n de la nappe \n phréatique [pieds]", option = 'turbo') + 
    
    labs(x = "x" , y = "y" , title = paste("Carte des résidus de la profondeur moyenne de la nappe phréatique en octobre", year,"\n à toutes les stations de mesure autour de Bakersfield")) + 
    
    theme(plot.title = element_text(hjust = 0.5)) 
  
  grid.arrange(plot_mean_depth, plot_res) 
  
} 



# Linear regression for the year 2015 

year <- 2019
october_mean_peryear <- linear_reg(year) 
#head(october_mean_2015) 

ggplot(october_mean_peryear) + 
  
  geom_point(aes(x = elevation, y = depth.res)) + 
  
  geom_hline(aes(yintercept = 0))  

hist(october_mean_peryear$depth.res)
sd(october_mean_peryear$depth.res)
max(october_mean_peryear$depth.res)
residuals_plot(october_mean_peryear) 


###### constructing the experimental variograms

library(gstat)
E.gstat <- gstat(formula = depth.res~1, data = october_mean_peryear, locations = ~x+y)

E.vario <- variogram(E.gstat)
variance_E <- var(october_mean_peryear$depth.res)

ggplot(E.vario) + geom_point(aes(x = dist,y=gamma)) +
  scale_x_continuous(limits = c(0, max(E.vario$dist))) +
  scale_y_continuous(limits = c(0, max(E.vario$gamma)))+ 
  geom_hline(aes(yintercept = variance_E ), linetype = "dotted")

### modeling the variograms

vario.model <- vgm(psill = 10000, model = "Exp", range = 25000, nugget = 1200)
#plot(vario.model, cutoff = 14)
fit.vario.model <- fit.variogram(E.vario, model = vario.model)
#plot(fit.vario.model, cutoff = 14)
vario.model.values <- variogramLine(fit.vario.model, maxdist = 32000)
ggplot(E.vario) + geom_point(aes(x = dist,y = gamma)) + 
  geom_line(data = vario.model.values, aes(x = dist,y = gamma))+ 
  geom_hline(aes(yintercept = variance_E ), linetype = "dotted")

#View(vario.model.values)
vario.model.values


