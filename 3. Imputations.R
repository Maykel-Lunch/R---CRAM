
# IMPUTATION 1 - Fill missing values with median (Median-Imputation)
Fin_Co2_med <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_med <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_med <- Fin_Co2_med[Fin_Co2_med$Country != "World", ]
Fin_Energy_med <- Fin_Energy_med[Fin_Energy_med$Country != "World",]

# Get numeric columns
numeric_cols <- sapply(Fin_Co2_med, is.numeric)
numeric_cols2 <- sapply(Fin_Energy_med, is.numeric)

# Replace the NA with medians and print the median value of each column
for (col in names(Fin_Co2_med)[numeric_cols]) {
  median_value <- median(Fin_Co2_med[[col]], na.rm = TRUE)  # Calculate median
  Fin_Co2_med[[col]][is.na(Fin_Co2_med[[col]])] <- median_value      # Replace NA with median
  cat("Median of", col, ":", median_value, "\n")   # Print median
}

for (col in names(Fin_Energy_med)[numeric_cols2]) {
  median_value <- median(Fin_Energy_med[[col]], na.rm = TRUE)  # Calculate median
  Fin_Energy_med[[col]][is.na(Fin_Energy_med[[col]])] <- median_value      # Replace NA with median
  cat("Median of", col, ":", median_value, "\n")   # Print median
}

# Check for again for missing values
sum(is.na(Fin_Co2_med))
sum(is.na(Fin_Energy_med))

# Save to file
write.csv(Fin_Co2_med, "Data/Imputed Data/Co2(Med Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_med, "Data/Imputed Data/Energy(Med Imp).csv", row.names = FALSE)





### CLEAN THE ENVIRONMENT PANEL FIRST ###

# IMPUTATION 2 - Fill missing values with mean (Mean-Imputation)
Fin_Co2_mean <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_mean <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_mean <- Fin_Co2_mean[Fin_Co2_mean$Country != "World", ]
Fin_Energy_mean <- Fin_Energy_mean[Fin_Energy_mean$Country != "World",]

# Get numeric columns
numeric_cols <- sapply(Fin_Co2_mean, is.numeric)
numeric_cols2 <- sapply(Fin_Energy_mean, is.numeric)


# Loop through numeric columns to replace NA and print means
for (col in names(Fin_Co2_mean)[numeric_cols]) {
  mean_value <- mean(Fin_Co2_mean[[col]], na.rm = TRUE)
  Fin_Co2_mean[[col]][is.na(Fin_Co2_mean[[col]])] <- mean_value
  cat("Mean of", col, ":", mean_value, "\n")
}

for (col in names(Fin_Energy_mean)[numeric_cols2]) {
  mean_value <- mean(Fin_Energy_mean[[col]], na.rm = TRUE)
  Fin_Energy_mean[[col]][is.na(Fin_Energy_mean[[col]])] <- mean_value
  cat("Mean of", col, ":", mean_value, "\n")
}

# Check for again for missing values
sum(is.na(Fin_Co2_mean))
sum(is.na(Fin_Energy_mean))

# Save to file
write.csv(Fin_Co2_mean, "Data/Imputed Data/Co2(Mean Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_mean, "Data/Imputed Data/Energy(Mean Imp).csv", row.names = FALSE)
