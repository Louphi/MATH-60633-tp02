library("here")
library("PerformanceAnalytics")
source(here("Functions", "f_black_scholes"))
source(here("Functions", "f_load_data"))
source(here("Functions", "f_days_to_years"))
source(here("Functions", "f_linear_interpolation"))
source(here("Functions", "f_generate_log_returns"))

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

# Set last SP500 price
last_sp500 <- tail(sp500[, 1], n=1)

# Set last VIX value
last_vix <- tail(vix[, 1], n=1)

# Define Black-Scholes variables
S <- rep(last_sp500, 4)
K <- c(1600, 1650, 1750, 1800)
T <- f_days_to_years(c(20, 20, 40, 40), 250)
r <- f_linear_interpolation(T, rf_maturities, rf_rates)
sig <- rep(last_vix, 4)
types <- rep("C", 4)

# Compute option values
option_prices <- f_black_scholes(S, K, r, T, sig, type="C")

# Option quantities
quantities <- c(1, 1, 1, 1)

# Portfolio value
value <- t(quantities) %*% option_prices


