# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the datasets (mean imputation)  
co2_data <- read.csv("Data/Imputed Data/Co2(Mean Imp).csv", stringsAsFactors = FALSE)
energy_data <- read.csv("Data/Imputed Data/Energy(Mean Imp).csv", stringsAsFactors = FALSE)

# View the structure of both datasets (OPTIONAL)
str(co2_data)
str(energy_data)

# Preview the first few rows of both datasets (OPTIONAL)
head(co2_data)
head(energy_data)

# Check for missing values in both datasets (OPTIONAL)
colSums(is.na(co2_data))
colSums(is.na(energy_data))

# Summary statistics for CO2 Emissions and Energy Generation (OPTIONAL)
summary(co2_data)
summary(energy_data)

# Remove the world
co2_data <- co2_data[co2_data$cCountry != "World", ]
####  DATA VISUALIZATION ####

#Histogram
hist(co2_data, 
     breaks = 30,  # Number of bins
     col = "lightblue",  # Fill color
     border = "black",  # Border color
     main = "Histogram of Total Co2 Emissions", 
     xlab = "",  # X-axis label
     ylab = "Emissions")  # Y-axis label
