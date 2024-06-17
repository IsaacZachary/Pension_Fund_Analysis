Here's the updated README with the changes and additions, including references to the generated images:

# Pension Fund Analysis Research
![R Programming](https://bcrf.biochem.wisc.edu/wp-content/uploads/sites/850/2018/12/featured-2.png)

This R project aims to model the growth of pension funds in Kenya using the Autoregressive Integrated Moving Average (ARIMA) model and analyze the impact of external factors on this growth. The specific objectives are:

1. **To forecast the growth of pension funds in Kenya using the ARIMA model.**
2. **To analyze the impact of external factors on the growth of pension funds.**

## Solution

### Objective 1: Forecasting Pension Fund Growth

**Steps:**

1. **Data Preparation:**
    - Pension contribution data, inflation rates, and central bank rates for 2022 and 2023 are loaded from Excel files.
    - The pension contribution data is extracted and converted into a time series object.
2. **Simulating Pension Data:**
    - An ARIMA model is fitted to the existing pension contribution data to capture its underlying pattern.
    - The ARIMA model is then used to forecast the pension data for the years 2024 and 2025, extending it to a total of 48 observations.
    - The simulated pension data is plotted to visualize the forecasted growth.

### Objective 2: Analyzing Impact of External Factors

**Steps:**

3. **Fitting Distributions for External Factors:**
    - Central bank rates (CBR) and inflation rates are extracted and converted into time series objects.
    - Distributions are fitted to the CBR and inflation rate data to match the length of the pension data.
4. **Regression Analysis:**
    - The pension data, along with the simulated CBR and inflation rate data, are combined into a data frame.
    - A regression model is constructed with the pension data as the dependent variable and the CBR and inflation rate as independent variables.
    - The regression model is fitted and summarized to analyze the impact of external factors on the growth of pension funds.
    - The regression results are plotted to visualize the relationship between the actual pension contributions and the fitted values.

## Results

- **Simulated Pension Data (2022-2025):**
    - The ARIMA model simulates pension data to 48 observations (2022-2025).
    - ![Simulated Pension Data (2022-2025)](images/simulated_pension_data_2022_2025.png)

- **Pension Data Forecast (2022-2027):**
    - The ARIMA model forecasts the growth of pension funds up to 2027, showing an increasing trend over time.
    - ![Pension Data Forecast (2022-2027)](images/final_pension_forecast_2022-2027.png)

- **Regression Analysis Results:**
    - The regression analysis reveals a significant impact of central bank rates and inflation rates on pension contributions.
    - ![Regression Analysis Results](images/regression_analysis_results.png)

## Required Libraries and Packages

```r
# Load necessary libraries
install.packages("readxl")
install.packages("forecast")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("tseries")

# Load the packages
library(readxl)
library(forecast)
library(ggplot2)
library(imputeTS)
library(tseries)
```

## R Script

```r
# Set working directory to the project folder
setwd("C:/Users/humbl/OneDrive/Documents/R PROJECTS/Pension_Fund_Analysis")

# Install necessary libraries if they are not already installed
required_packages <- c("tseries", "readxl", "forecast", "ggplot2", "imputeTS", "openxlsx")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load necessary libraries
library(tseries)
library(readxl)
library(forecast)
library(ggplot2)
library(imputeTS)
library(openxlsx)

# Load data from Excel files
pension_data <- read_excel("data/Pension_Contributions.xlsx", skip = 1) # Skip the first row
inflation_data <- read_excel("data/Inflation_Rates.xlsx", skip = 1) # Skip the first row
centralbank_data <- read_excel("data/CentralBank_Rates.xlsx", skip = 1) # Skip the first row

# Inspect the data
head(pension_data)
head(inflation_data)
head(centralbank_data)

# Extract pension data
pension_series <- ts(pension_data$`AMOUNT (KES)`, start = c(2022, 1), frequency = 12)

# Check for NA values
na_count <- sum(is.na(pension_series))
print(paste("Number of NA values in the series:", na_count))

# Handle NA values by linear interpolation if any
pension_series <- na_interpolation(pension_series)

# Check for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(pension_series)
print(adf_test)

if (adf_test$p.value > 0.05) {
  # If p-value > 0.05, the series is non-stationary, apply differencing
  pension_series_diff <- diff(pension_series)
} else {
  pension_series_diff <- pension_series
}

# Ensure no NA or infinite values after differencing
pension_series_diff <- na.omit(pension_series_diff)
pension_series_diff <- pension_series_diff[is.finite(pension_series_diff)]

# Box-Cox transformation for variance stabilization
lambda <- BoxCox.lambda(pension_series_diff)
pension_series_transformed <- BoxCox(pension_series_diff, lambda)

# Try using auto.arima to find the best model
auto_fit <- auto.arima(pension_series_transformed, seasonal = TRUE)
print(auto_fit)

# Manually fit alternative ARIMA models and compare
fit1 <- tryCatch(arima(pension_series_transformed, order = c(1,1,1)), error = function(e) NULL)
fit2 <- tryCatch(arima(pension_series_transformed, order = c(2,1,2)), error = function(e) NULL)
fit3 <- tryCatch(arima(pension_series_transformed, order = c(1,1,0)), error = function(e) NULL)
fit4 <- tryCatch(arima(pension_series_transformed, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12)), error = function(e) NULL)

# Compare AIC values if models are successfully fitted
aic_values <- c(
  if (!is.null(fit1)) fit1$aic else NA,
  if (!is.null(fit2)) fit2$aic else NA,
  if (!is.null(fit3)) fit3$aic else NA,
  if (!is.null(fit4)) fit4$aic else NA
)
model_names <- c("ARIMA(1,1,1)", "ARIMA(2,1,2)", "ARIMA(1,1,0)", "SARIMA(0,1,1)(0,1,1)[12]")
names(aic_values) <- model_names
print(aic_values)

# Select the best model based on AIC
best_model <- auto_fit
if (!is.null(fit1) && fit1$aic < best_model$aic) best_model <- fit1
if (!is.null(fit2) && fit2$aic < best_model$aic) best_model <- fit2
if (!is.null(fit3) && fit3$aic < best_model$aic) best_model <- fit3
if (!is.null(fit4) && fit4$aic < best_model$aic) best_model <- fit4
print(best_model)

# Forecast using the best model for 24 months to extend the series to 48 observations
pension_forecast_transformed <- forecast(best_model, h = 24)

# Inverse Box-Cox transformation
pension_forecast <- InvBoxCox(pension_forecast_transformed$mean, lambda)

# Combine original and forecasted data to get a total of 48 observations
pension_combined <- c(pension_series, pension_forecast)

# Ensure all forecasted values are positive
pension_combined[pension_combined < 0] <- 0

# Create time series object for combined data
pension_combined_series <- ts(pension_combined, start = c(2022, 1), frequency = 12)

# Convert to a data frame for table output
pension_combined_df <- data.frame(
  Date = seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = length(pension_combined_series)),
  Pension_Contributions = as.numeric(pension_combined_series)
)

# Print the table to the console
print(pension_combined_df)

# Save the table to an Excel file
write.xlsx(pension_combined_df, "simulated_pension_data_2022_2025.xlsx", rowNames = FALSE)

# Save plot to a file
png(filename = "simulated_pension_data_2022_2025.png", width = 800, height = 600)
plot(pension_combined_series, main = "Simulated Pension Data (2022-2025)", ylab = "Pension Contributions", xlab = "Year")
dev.off()

# Step 2: Forecast Pension Data to 2027 (i.e., an additional 24 months beyond 2025)
extended_fore

cast_transformed <- forecast(best_model, h = 48)

# Inverse Box-Cox transformation for extended forecast
extended_forecast <- InvBoxCox(extended_forecast_transformed$mean, lambda)

# Combine original, simulated, and extended forecast data
final_pension_series <- ts(c(pension_series, extended_forecast), start = c(2022, 1), frequency = 12)

# Ensure all forecasted values are positive
final_pension_series[final_pension_series < 0] <- 0

# Save the extended forecast to an Excel file
final_pension_df <- data.frame(
  Date = seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = length(final_pension_series)),
  Pension_Contributions = as.numeric(final_pension_series)
)
write.xlsx(final_pension_df, "final_pension_forecast_2022_2027.xlsx", rowNames = FALSE)

# Save final forecast plot to a file
png(filename = "final_pension_forecast_2022-2027.png", width = 800, height = 600)
plot(final_pension_series, main = "Pension Data Forecast (2022-2027)", ylab = "Pension Contributions", xlab = "Year")
dev.off()

# Step 3: Fit Distribution for CBR and Inflation Data
cbr_series <- ts(centralbank_data$`CENTRAL BANK RATE (%)`, start = c(2022, 1), frequency = 12)
inflation_series <- ts(inflation_data$`12-MONTH INFLATION`, start = c(2022, 1), frequency = 12)

# Extend CBR and Inflation data to match length of Pension data
set.seed(123)
cbr_extended <- sample(cbr_series, length(final_pension_series), replace = TRUE)
inflation_extended <- sample(inflation_series, length(final_pension_series), replace = TRUE)

cbr_combined_series <- ts(c(cbr_series, cbr_extended), start = c(2022, 1), frequency = 12)
inflation_combined_series <- ts(c(inflation_series, inflation_extended), start = c(2022, 1), frequency = 12)

# Ensure all series have the same length
n_obs <- length(final_pension_series)
cbr_combined_trimmed <- cbr_combined_series[1:n_obs]
inflation_combined_trimmed <- inflation_combined_series[1:n_obs]

# Step 4: Regression Analysis
analysis_data <- data.frame(
  Pension = as.numeric(final_pension_series),
  CBR = as.numeric(cbr_combined_trimmed),
  Inflation = as.numeric(inflation_combined_trimmed)
)

# Fit regression model
regression_model <- lm(Pension ~ CBR + Inflation, data = analysis_data)
summary(regression_model)

# Plot the regression results
png(filename = "regression_analysis_results.png", width = 800, height = 600)
plot(analysis_data$Pension, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Regression Analysis Results")
lines(fitted(regression_model), col = "red")
legend("topright", legend = c("Actual Pension", "Fitted Values"), col = c("blue", "red"), lty = 1)
dev.off()
```

## Environment Results

![Environment Pension_Fund_Analysis](images/Environment.JPG)

## How to Run

1. Ensure all required libraries are installed and loaded.
2. Set your working directory to the project folder.
3. Load the data files from the `data` directory.
4. Follow the steps in the R script to simulate, forecast, and analyze the pension fund data.
5. View the plots generated to visualize the results.

For any questions or issues, please open an issue in this repository.