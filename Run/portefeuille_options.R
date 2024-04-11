library("here")
library("PerformanceAnalytics")

setwd(here())
files.sources = list.files(path = here("Functions"), full.names = TRUE)
sapply(files.sources, source)

# Load & define data
data <- f_load_data()
sp500 <- data$sp500
vix <- data$vix
rf <- data$rf
calls <- data$calls
puts <- data$puts

# Set risk-free maturities (in years) & rates
rf_maturities <- as.numeric(attr(rf, "names"))
rf_rates <- rf[,1]

# Set last SP500 & VIX price
last_sp500 <- as.numeric(tail(sp500[, 1], n=1))
last_vix <- as.numeric(tail(vix[, 1], n=1))

# Define Black-Scholes variables
S <- rep(last_sp500, 4)
K <- c(1600, 1650, 1750, 1800)
T <- f_days_to_years(c(20, 20, 40, 40), 250)
T_r <- f_days_to_years(c(20, 20, 40, 40), 360)
r <- f_linear_interpolation(T_r, rf_maturities, rf_rates)
sig <- rep(last_vix, 4)
types <- rep("C", 4)

# Compute option values
option_prices <- f_black_scholes(S, K, r, T, sig, type="C")

# Option quantities
quantities <- c(1, 1, 1, 1)

# Portfolio value
value <- t(quantities) %*% option_prices
