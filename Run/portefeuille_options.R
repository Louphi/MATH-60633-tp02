library("here")
source(here("Functions", "f_black_scholes"))
source(here("Functions", "f_load_data"))
source(here("Functions", "f_days_to_years"))
source(here("Functions", "f_linear_interpolation"))
source(here("Functions", "f_price_scenarios"))
source(here("Functions", "f_price_vix_scenarios"))
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

### Fixation du prix d’un portefeuille d’options ###

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


### Un facteur de risque et un modèle gaussien univarié ### 

# Initisalization 

period <- 5
n_sim <- 10000
T_uni <- f_days_to_years(c(20, 20, 40, 40)-period, 250)
T_r_uni <- f_days_to_years(c(20, 20, 40, 40)-period, 360)
r_uni <- f_linear_interpolation(T_r_uni, rf_maturities, rf_rates)


# Compute sp500 returns
rets_sp500 <- PerformanceAnalytics::CalculateReturns(sp500, method = "log")[-1]

# Compute mean, st_dev, kurtosis and skewness

mu <- mean(rets_sp500)
std <- var(rets_sp500)
kurt <- PerformanceAnalytics::kurtosis(rets_sp500)
skew <- PerformanceAnalytics::skewness(rets_sp500)

# Compute price scenarios for SP500
scenarios = f_price_scenarios(sp500, period, n_sim)

# Compute option prices and portfolio value
port_value_uni <- rep(0, n_sim)

for (i in 1:n_sim) {
 
  port_value_uni[i] <- sum(f_black_scholes(scenarios[i], K, r_uni, T_uni, sig, type="C"))
}

hist(port_value_uni, breaks=50, main="Histogram of portfolio value", xlab="Portfolio value")

# Compute VaR and ES 95%
VaR_95_uni <- quantile(port_value_uni, 0.05)

ES_95_uni <- mean(port_value_uni[port_value_uni <= VaR_95_uni])


### Deux facteurs de risque et un modèle gaussien bivarié ###

# Initialization
period <- 5
n_sim <- 10000
T_bi <- f_days_to_years(c(20, 20, 40, 40)-period, 250)
T_r_bi <- f_days_to_years(c(20, 20, 40, 40)-period, 360)
r_bi <- f_linear_interpolation(T_r_bi, rf_maturities, rf_rates)

# Compute sp500 returns and vix returns
rets_sp500 <- PerformanceAnalytics::CalculateReturns(sp500, method = "log")[-1]
rets_vix <- PerformanceAnalytics::CalculateReturns(vix, method = "log")[-1]


scenarios_sp_vix <- f_price_vix_scenarios(sp500, vix, period, n_sim)

## Simu à revoir ou savoir si valeur extreme normales ? 

# Compute option prices and portfolio value
port_value_bi <- rep(0, n_sim)

for (i in 1:n_sim) {
  
  port_value_bi[i] <- sum(f_black_scholes(scenarios_sp_vix[i,1], K, r_bi, T_bi, scenarios_sp_vix[i,2], type="C"))
}

hist(port_value_bi, breaks=50, main="Histogram of portfolio value", xlab="Portfolio value")
# Compute VaR and ES 95%
VaR_95_bi <- quantile(port_value_bi, 0.05)

ES_95_bi <- mean(port_value_bi[port_value_bi <= VaR_95_bi])


