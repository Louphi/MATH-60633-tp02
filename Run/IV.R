library(here)
library(PerformanceAnalytics)
library(MASS)
library(plotly)
library(reshape2)
library(dplyr)
source(here("Functions", "f_load_data"))
# Load data
data <- f_load_data()

# Define data
sp500 <- data$sp500
vix <- data$vix
rf <- data$rf
calls <- data.frame(data$calls)
puts <- data.frame(data$puts)
last_sp500 <- as.numeric(sp500[nrow(sp500),1])
last_vix <- as.numeric(vix[nrow(sp500),1])

option_info <- rbind(calls, puts)
option_info <- calls
option_info$m <- option_info$K / last_sp500


# Parameters Initialization
alpha1 = 0 # Car la vol d'une option ATM à maturité est nulle
alpha2 = 0 #  On suppose une volatilité constante sur m
alpha3 = 0 # On suppose une volatilité constante sur m
alpha4 = last_vix - alpha1 # car alpha1 + alpha4 = VIX à maturité 1 an ATM

theta0 = c(alpha1, alpha2, alpha3, alpha4)

# Optimization

# Define the function to minimize

f_MSE <- function(theta){
  alpha1 <- theta[1]
  alpha2 <- theta[2]
  alpha3 <- theta[3]
  alpha4 <- theta[4]
  
  AE <- sum(abs(option_info$IV - alpha1 + alpha2 * (1- option_info$m)^2 + alpha3 *(1- option_info$m)^3 + alpha4 * sqrt(option_info$tau)))
  
  return(AE)
}

# Contraint matrix construction  ( IV should be positive)

a <- cbind(c(rep(1,nrow(option_info))),c((1-option_info$m)^2), c((1-option_info$m)^3), c(sqrt(option_info$tau)))
b <- c(rep(0,nrow(option_info)))

# Minimize the function f

opt <- constrOptim(theta0, f_MSE, NULL, ui = a, ci = b)
theta <- opt$par

# Database creation

option_info$IV_est <- theta[1] + theta[2] * (1- option_info$m)^2 + theta[3] *(1- option_info$m)^3 + theta[4] * sqrt(option_info$tau)

IV_surface <- option_info[c("m", "tau", "IV_est")]





# Plot the surface (pas fonctionnel)

IV_matrix <- acast(IV_surface, "m" ~ "tau", value.var = "IV_est")
matrice <- xtabs(IV_est ~ m + tau, data = IV_surface)
fig <- plot_ly(x = as.numeric(IV_surface$m), y = as.numeric(IV_surface$tau), z = as.numeric(IV_surface$IV_est)) %>% add_surface()

fig

     
     

     