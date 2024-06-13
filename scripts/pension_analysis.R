# Set working directory to the project folder
setwd("C:/Users/izach/Desktop/izach/R-Projects/Pension_Fund_Analysis")

# Load necessary libraries
install.packages("tseries")
install.packages("readxl")
install.packages("forecast")
install.packages("ggplot2")
install.packages("imputeTS")

library(tseries)
library(readxl)
library(forecast)
library(ggplot2)
library(imputeTS)

# Check versions to verify successful installation
packageVersion("readxl")
packageVersion("forecast")
packageVersion("ggplot2")

# Load data from Excel files
pension_data <- read_excel("data/Pension_Contributions.xlsx", skip = 1)  # Skip the first row
inflation_data <- read_excel("data/Inflation_Rates.xlsx",  skip = 1) # Skip the first row
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

# Handle NA values by linear interpolation
pension_series <- na_interpolation(pension_series)

# Check for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(pension_series)
print(adf_test)

if (adf_test$p.value > 0.05) {
  # If p-value > 0.05, the series is non-stationary, apply differencing
  pension_series <- diff(pension_series)
}

# Step 1: Simulate Pension Data to 48 observations (2022-2025)
# Fit ARIMA model to existing data
pension_arima_fit <- auto.arima(pension_series)

# Forecast additional 24 observations (2 years)
pension_forecast <- forecast(pension_arima_fit, h = 24)

# Combine original and forecasted data
pension_combined <- c(pension_series, pension_forecast$mean)

# Create time series object for combined data
pension_combined_series <- ts(pension_combined, start = c(2022, 1), frequency = 12)

# Save plot to a file
png(filename = "simulated_pension_data_2022_2025.png", width = 800, height = 600)
plot(pension_combined_series, main = "Simulated Pension Data (2022-2025)", ylab = "Pension Contributions", xlab = "Year")
dev.off()

# Step 2: Forecast Pension Data to 2027
combined_arima_fit <- auto.arima(pension_combined_series)
extended_forecast <- forecast(combined_arima_fit, h = 24)
final_pension_forecast <- c(pension_combined_series, extended_forecast$mean)
final_pension_series <- ts(final_pension_forecast, start = c(2022, 1), frequency = 12)

# Save final forecast plot to a file
png(filename = "final_pension_forecast_2022_2027.png", width = 800, height = 600)
plot(final_pension_series, main = "Pension Data Forecast (2022-2027)", ylab = "Pension Contributions", xlab = "Year")
dev.off()

# Step 3: Fit Distribution for CBR and Inflation Data
cbr_series <- ts(centralbank_data$`CENTRAL BANK RATE (%)`, start = c(2022, 1), frequency = 12)
inflation_series <- ts(inflation_data$`12-MONTH INFLATION`, start = c(2022, 1), frequency = 12)

# Extend CBR and Inflation data to match length of Pension data
set.seed(123)
cbr_extended <- sample(cbr_series, length(final_pension_series), replace = TRUE)
inflation_extended <- sample(inflation_series, length(final_pension_series), replace = TRUE)

cbr_combined_series <- ts(cbr_extended, start = c(2022, 1), frequency = 12)
inflation_combined_series <- ts(inflation_extended, start = c(2022, 1), frequency = 12)

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
