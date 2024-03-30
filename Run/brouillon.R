library("here")
source(here("Functions", "f_black_scholes"))
source(here("Functions", "f_load_data"))
source(here("Functions", "f_days_to_years"))
source(here("Functions", "f_linear_interpolation"))

# Load data
data <- f_load_data()

# Define data
sp500 <- data$sp500
vix <- data$vix
rf <- data$rf
calls <- data$calls
puts <- data$puts

# Set risk-free maturities (in years)
rf_maturities <- as.numeric(attr(rf, "names"))

# Set risk-free rates
rf_rates <- rf[,1]

# Find option maturity in # years
years = f_days_to_years(c(20, 20, 40, 40), 250)

# Interpolate linearly the rates for each maturity
interpolations = f_linear_interpolation(years, rf_maturities, rf_rates)

