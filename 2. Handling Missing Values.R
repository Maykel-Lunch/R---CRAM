# Analyze the missing values and apply appropriate imputations #

Fin_Co2_df <- read.csv("Data/Merge Data/CO2 Emissions(cleansed).csv")

# Convert specific columns to numeric
Fin_Co2_df[, c(1, 3:6)] <- lapply(Fin_Co2_df[, c(1, 3:6)], as.numeric)

# Save to file (This overwrite the previous file)
write.csv(Fin_Co2_df, "Data/Merge Data/CO2 Emissions(cleansed).csv", row.names = FALSE)

# Count the NA / Missing Values
sum(is.na(Fin_Co2_df))

# Get row, column indices and country of NA values
na_locations <- which(is.na(Fin_Co2_df), arr.ind = TRUE)

# Loop through each NA and print the row, column, and country
for (i in 1:nrow(na_locations)) {
  row_index <- na_locations[i, "row"]
  col_index <- na_locations[i, "col"]
  country <- Fin_Co2_df[row_index, "Country"]  
  
  # Get column name where NA occurs
  column_name <- colnames(Fin_Co2_df)[col_index]
  
  # Print the row, column, and country
  print(paste("Row:", row_index, "| Column:", column_name, "| Country:", country))
}


Fin_Energy_df <- read.csv("Data/Merge Data/Electricity Generation(cleansed).csv")

# Convert specific columns to numeric
Fin_Energy_df[, c(1, 3:10)] <- lapply(Fin_Energy_df[, c(1, 3:10)], as.numeric)

# Save to file (This overwrite the previous file)
write.csv(Fin_Energy_df, "Data/Merge Data/Electricity Generation(cleansed).csv", row.names = FALSE)

# Count the NA / Missing Values
sum(is.na(Fin_Energy_df))

# Get row, column indices and country of NA values
na_locations <- which(is.na(Fin_Energy_df), arr.ind = TRUE)

# Loop through each NA and print the row, column, and country
for (i in 1:nrow(na_locations)) {
  row_index <- na_locations[i, "row"]
  col_index <- na_locations[i, "col"]
  country <- Fin_Energy_df[row_index, "Country"]  
  
  # Get column name where NA occurs
  column_name <- colnames(Fin_Energy_df)[col_index]
  
  # Print the row, column, and country
  print(paste("Row:", row_index, "| Column:", column_name, "| Country:", country))
}
