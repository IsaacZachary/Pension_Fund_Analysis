# Set working directory to the project folder
setwd("C:/Users/izach/Desktop/izach/R-Projects/Pension_Fund_Analysis")

# Load necessary libraries
install.packages("readxl")
install.packages("forecast")
install.packages("ggplot2")

# Load the packages
library(readxl)
library(forecast)
library(ggplot2)

# Check versions to verify successful installation
packageVersion("readxl")
packageVersion("forecast")
packageVersion("ggplot2")
  
# Load data from Excel files
pension_data <- read_excel("data/Pension_Contributions.xlsx", skip = 1)  # Skip the first row
inflation_data <- read_excel("data/Inflation_Rates.xlsx",  skip = 1) # Skip the first row
centralbank_data <- read_excel("data/CentralBank _Rates.xlsx", skip = 1) # Skip the first row

# Inspect the data
head(pension_data)
head(inflation_data)
head(centralbank_data)

# Extract pension data
pension_series <- ts(pension_data$`AMOUNT (KES)`, start = c(2022, 1), frequency = 12)

# Step 1: Simulate Pension Data to 48 observations (2022-2025)
pension_arima_fit <- auto.arima(pension_series)
pension_forecast <- forecast(pension_arima_fit, h = 24)
pension_combined <- c(pension_series, pension_forecast$mean)
pension_combined_series <- ts(pension_combined, start = c(2022, 1), frequency = 12)
plot(pension_combined_series, main = "Pension Data (2022-2025)", ylab = "Pension Contributions", xlab = "Year")

# Step 2: Forecast Pension Data to 2027
combined_arima_fit <- auto.arima(pension_combined_series)
extended_forecast <- forecast(combined_arima_fit, h = 24)
final_pension_forecast <- c(pension_combined_series, extended_forecast$mean)
final_pension_series <- ts(final_pension_forecast, start = c(2022, 1), frequency = 12)
plot(final_pension_series, main = "Pension Data Forecast (2022-2027)", ylab = "Pension Contributions", xlab = "Year")

# Step 3: Fit Distribution for CBR and Inflation Data
cbr_series <- ts(centralbank_data$`CENTRAL BANK RATE (%)`, start = c(2022, 1), frequency = 12)
inflation_series <- ts(inflation_data$`12-MONTH INFLATION`, start = c(2022, 1), frequency = 12)

set.seed(123)
cbr_extended <- sample(cbr_series, 72, replace = TRUE)
inflation_extended <- sample(inflation_series, 72, replace = TRUE)

cbr_combined <- c(cbr_series, cbr_extended)
inflation_combined <- c(inflation_series, inflation_extended)

cbr_combined_series <- ts(cbr_combined, start = c(2022, 1), frequency = 12)
inflation_combined_series <- ts(inflation_combined, start = c(2022, 1), frequency = 12)

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

regression_model <- lm(Pension ~ CBR + Inflation, data = analysis_data)
summary(regression_model)

# Plot the regression results
plot(analysis_data$Pension, type = "l", col = "blue", xlab = "Time", ylab = "Value")
lines(fitted(regression_model), col = "red")
legend("topright", legend = c("Actual Pension", "Fitted Values"), col = c("blue", "red"), lty = 1)