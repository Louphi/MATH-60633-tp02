

### Deux facteurs de risque et modèle copule-marginal (Student-t et copule gaussienne) ###

# Initialization
period <- 5
n_sim <- 10000
T_cop <- f_days_to_years(c(20, 20, 40, 40)-period, 250)
T_r_cop <- f_days_to_years(c(20, 20, 40, 40)-period, 360)
r_cop <- f_linear_interpolation(T_r_cop, rf_maturities, rf_rates)


mu <- c(mean(rets_sp500), mean(rets_vix)) * period
r <- cor(cbind(rets_sp500, rets_vix))
sig <- cov(cbind(rets_sp500, rets_vix)) * period
last_sp500 <- as.numeric(sp500[nrow(sp500),1])
last_vix <- as.numeric(vix[nrow(sp500),1])


# Copule gaussienne 

Z <- mvrnorm(n = n_sim, mu = mu, Sigma = sig)
U_1 <- pnorm(q = Z[, 1], mu[1], sqrt(sig[1,1])) 
U_2 <- pnorm(q = Z[, 2], mu[2], sqrt(sig[2,2])) 

plot(x = U_1, y = U_2, type = "p",
     xlab = "U1", ylab = "U2",
     main = "Tirages d'une copule gaussienne",
     pch = 20, cex = 0.8, tck = 0)

price_stu = qt(U_1, df = 10) 
vix_stu = qt(U_2, df = 5) 
hist(exp(price_stu), breaks=50, main="Histogram of SP500", xlab="SP500")

copule_scenarios <- cbind(price_stu, vix_stu)

# Compute option prices and portfolio value

port_value_cop <- rep(0, n_sim)

for (i in 1:n_sim) {
  
  port_value_cop[i] <- sum(f_black_scholes(exp(copule_scenarios[i,1]), K, r_cop, T_cop, exp(copule_scenarios[i,2]), type="C"))
}

hist(port_value_cop, breaks=50, main="Histogram of portfolio value", xlab="Portfolio value")
