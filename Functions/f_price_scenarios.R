source(here("Functions", "f_load_data"))
library(PerformanceAnalytics)
library(MASS)
data <- f_load_data()

# Define data
sp500 <- data$sp500
vix <- data$vix

  
# Compute sp500 returns
rets_sp500 <- PerformanceAnalytics::CalculateReturns(sp500, method = "log")[-1]
rets_vix <- PerformanceAnalytics::CalculateReturns(vix, method = "log")[-1]

last_price = as.numeric(sp500[nrow(sp500),1])
last_vix = as.numeric(vix[nrow(vix),1])
# Compute mean and average


mu <- c(mean(rets_sp500), mean(rets_vix))
sig <- cov(cbind(rets_sp500, rets_vix))

# Simulate scenarios
period <- 5 
mu_sim <- mu * period
sig_sim <- sig * period
# Comme les returns sont iid, on peut simuler toute la période en même temps

random_numbers <- mvrnorm(10000, mu_sim, sig_sim)


scenarios <- cbind(last_price * exp(random_numbers[,1]), last_vix * exp(random_numbers[,2]))
scenarios <- as.data.frame(scenarios)