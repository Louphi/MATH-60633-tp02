library("here")
library("PerformanceAnalytics")
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
last_price <- as.numeric(sp500[length(sp500)])
period <- 5 
# Compute sp500 returns
rets_sp500 <- PerformanceAnalytics::CalculateReturns(sp500, method = "log")[-1]

# Compute mean and average
mu <- mean(rets_sp500)
sig <- var(rets_sp500)

random_numbers <- rnorm(50000, mu, sig)

scenarios <- matrix(random_numbers, nrow = 10000, ncol = 5, byrow = TRUE) +1


scenarios[,1] <- scenarios[,1] * last_price

for i in 2:period{
  p_scenarios[,i] <- scenarios[,i-1] * scenarios[,i]
}