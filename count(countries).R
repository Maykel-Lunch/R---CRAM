df <- read.csv("Data/CO2 Emissions.csv")

row_numbers <- c()
n <- 1  # Start with the first term
while (TRUE) {
  # Calculate the current row number
  current_row <- 6 * n - 5
  
  # Break the loop if the current row exceeds the number of rows in the data
  if (current_row > nrow(df)) {
    break
  }
  
  # Append the current row number to the vector
  row_numbers <- c(row_numbers, current_row)
  
  # Increment n for the next term
  n <- n + 1
}


first_col_values <- df[row_numbers, 2]

countries <- first_col_values
write.csv(countries,"Data/Countries.csv", row.names = FALSE)

