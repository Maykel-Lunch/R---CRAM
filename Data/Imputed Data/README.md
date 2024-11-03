
# Imputation Methods in R

This document outlines various imputation methods that can be used to handle missing data in R. Each method is described with a brief explanation and accompanied by sample R code.

## 1. Imputation with 0

This method replaces missing values with 0.

```r
# Sample R code for 0 imputation
data <- c(1, NA, 3, NA, 5)
data[is.na(data)] <- 0
print(data)
```

## 2. Mean Imputation

This method replaces missing values with the mean of the non-missing values.

```r
# Sample R code for mean imputation
data <- c(1, NA, 3, NA, 5)
mean_value <- mean(data, na.rm = TRUE)
data[is.na(data)] <- mean_value
print(data)
```

## 3. Median Imputation

This method replaces missing values with the median of the non-missing values.

```r
# Sample R code for median imputation
data <- c(1, NA, 3, NA, 5)
median_value <- median(data, na.rm = TRUE)
data[is.na(data)] <- median_value
print(data)
```

## 4. Last Observation Carried Forward (LOCF) and Next Observation Carried Backward (NOCB)

This method fills missing values by carrying the last observed value forward (LOCF) or the next observed value backward (NOCB).

```r
# Sample R code for LOCF-NOCB
library(zoo)

data <- c(1, NA, 3, NA, 5)
data_locf <- na.locf(data)
data_nocb <- na.locf(data, fromLast = TRUE)

print(data_locf)  # Last Observation Carried Forward
print(data_nocb)  # Next Observation Carried Backward
```

## 5. Spline Interpolation

This method uses spline functions to estimate missing values.

```r
# Sample R code for spline interpolation
library(splines)

data <- c(1, NA, 3, NA, 5)
spline_model <- splinefun(1:length(data), data, method = "natural")
data[is.na(data)] <- spline_model(which(is.na(data)))

print(data)
```

## 6. Interpolation

This method estimates missing values based on surrounding data points.

```r
# Sample R code for interpolation
data <- c(1, NA, 3, NA, 5)
data <- na.approx(data)
print(data)
```

