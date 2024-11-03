library(zoo)       # For LOCF, NOCB, and interpolation
library(mice)      # For multiple imputation
library(splines)   # For spline interpolation
library(caret)     # For predictive modeling
library(forecast)

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

# Check again for missing values
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

### CLEAN THE ENVIRONMENT PANEL FIRST ###

# IMPUTATION 3: Fill missing values with 0 (0-imputations)
Fin_Co2_0 <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_0 <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_0 <- Fin_Co2_0[Fin_Co2_0$Country != "World", ]
Fin_Energy_0 <- Fin_Energy_0[Fin_Energy_0$Country != "World",]

# Replace NA values to 0
Fin_Co2_0[is.na(Fin_Co2_0)] <- 0
Fin_Energy_0[is.na(Fin_Energy_0)] <- 0

# Check again for missing values
sum(is.na(Fin_Co2_0))
sum(is.na(Fin_Energy_0))

# Save to file
write.csv(Fin_Co2_0, "Data/Imputed Data/Co2(0 Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_0, "Data/Imputed Data/Energy(0 Imp).csv", row.names = FALSE)



# IMPUTATIONS 4: Interpolation
Fin_Co2_inter <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_inter <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_inter <- Fin_Co2_inter[Fin_Co2_inter$Country != "World", ]
Fin_Energy_inter <- Fin_Energy_inter[Fin_Energy_inter$Country != "World",]

# Fill missing value using interpolation
for (col in names(Fin_Co2_inter)) {
  if (is.numeric(Fin_Co2_inter[[col]])) {
    Fin_Co2_inter[[col]] <- na.approx(Fin_Co2_inter[[col]], na.rm = FALSE)
  }
}

for (col in names(Fin_Energy_inter)) {
  if (is.numeric(Fin_Energy_inter[[col]])) {
    Fin_Energy_inter[[col]] <- na.approx(Fin_Energy_inter[[col]], na.rm = FALSE)
  }
}
# Check again for missing values
sum(is.na(Fin_Co2_inter))
sum(is.na(Fin_Energy_inter))

# Save to file
write.csv(Fin_Co2_inter, "Data/Imputed Data/Co2(Interpolation Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_inter, "Data/Imputed Data/Energy(Interpolation Imp).csv", row.names = FALSE)


# IMPUTATIONS 5: LOCF/NOCB Imputation
Fin_Co2_locf_nocb <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_locf_nocb <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_locf_nocb <- Fin_Co2_locf_nocb[Fin_Co2_locf_nocb$Country != "World", ]
Fin_Energy_locf_nocb <- Fin_Energy_locf_nocb[Fin_Energy_locf_nocb$Country != "World",]

#Fill missing values using last observation carried forward (LOCF) and next observation carried backward (NOCB) methods
for (col in names(Fin_Co2_locf_nocb)) {
  if (is.numeric(Fin_Co2_locf_nocb[[col]])) {
    Fin_Co2_locf_nocb[[col]] <- na.locf(Fin_Co2_locf_nocb[[col]], na.rm = FALSE)
    Fin_Co2_locf_nocb[[col]] <- na.locf(Fin_Co2_locf_nocb[[col]], fromLast = TRUE)
  }
}

for (col in names(Fin_Energy_locf_nocb)) {
  if (is.numeric(Fin_Energy_locf_nocb[[col]])) {
    Fin_Energy_locf_nocb[[col]] <- na.locf(Fin_Energy_locf_nocb[[col]], na.rm = FALSE)
    Fin_Energy_locf_nocb[[col]] <- na.locf(Fin_Energy_locf_nocb[[col]], fromLast = TRUE)
  }
}

# Check again for missing values
sum(is.na(Fin_Co2_locf_nocb))
sum(is.na(Fin_Energy_locf_nocb))

# Save to file
write.csv(Fin_Co2_locf_nocb, "Data/Imputed Data/Co2(LOCF-NOCB Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_locf_nocb, "Data/Imputed Data/Energy(LOCF-NOCB Imp).csv", row.names = FALSE)

# IMPUTATIONS 6: Multiple(MICE) Imputation
Fin_Co2_mice <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_mice <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_mice <- Fin_Co2_mice[Fin_Co2_mice$Country != "World", ]
Fin_Energy_mice <- Fin_Energy_mice[Fin_Energy_mice$Country != "World",]

#Fill missing values using MICE Imputations methods
Fin_Co2_mice <- mice(Fin_Co2_mice, m = 5, method = 'pmm', maxit = 50, seed = 500)
Fin_Co2_mice <- complete(Fin_Co2_mice, 1)

Fin_Energy_mice <- mice(Fin_Energy_mice, m = 5, method = 'pmm', maxit = 50, seed = 500)
Fin_Energy_mice <- complete(Fin_Energy_mice, 1)

# Check again for missing values
sum(is.na(Fin_Co2_mice))
sum(is.na(Fin_Energy_mice))

# Save to file
write.csv(Fin_Co2_mice, "Data/Imputed Data/Co2(MICE Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_mice, "Data/Imputed Data/Energy(MICE Imp).csv", row.names = FALSE)

# IMPUTATIONS 7: Time-Based Interpolation (Spline, Polynomial) Imputation
Fin_Co2_tbi <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")
Fin_Energy_tbi <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Remove the world
Fin_Co2_tbi <- Fin_Co2_tbi[Fin_Co2_tbi$Country != "World", ]
Fin_Energy_tbi <- Fin_Energy_tbi[Fin_Energy_tbi$Country != "World",]

#Fill missing values using Time-based interpolation Imputations methods (Spline Interpolation)
for (col in names(Fin_Co2_tbi)) {
  if (is.numeric(Fin_Co2_tbi[[col]]) && sum(is.na(Fin_Co2_tbi[[col]])) > 0) {
    Fin_Co2_tbi[[col]] <- na.spline(Fin_Co2_tbi[[col]], na.rm = FALSE)  # Spline interpolation
  }
}

for (col in names(Fin_Energy_tbi)) {
  if (is.numeric(Fin_Energy_tbi[[col]]) && sum(is.na(Fin_Energy_tbi[[col]])) > 0) {
    Fin_Energy_tbi[[col]] <- na.spline(Fin_Energy_tbi[[col]], na.rm = FALSE)  # Spline interpolation
  }
}

# Check again for missing values
sum(is.na(Fin_Co2_tbi))
sum(is.na(Fin_Energy_tbi))

# Save to file
write.csv(Fin_Co2_tbi, "Data/Imputed Data/Co2(Spline Interpolation Imp).csv", row.names = FALSE)
write.csv(Fin_Energy_tbi, "Data/Imputed Data/Energy(Spline Interpolation Imp).csv", row.names = FALSE)

