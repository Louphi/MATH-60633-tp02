library("MASS")

# Define function to generate draws from univariate normal
f_generate_univariate_log_returns <- function(mu, sigma, scenarios, n) {
    sample <- rnorm(n * scenarios, mu, sigma)
    sample
}

# Define function to generate draws from bivariate normal
f_generate_bivariate_log_returns <- function(mu, Sigma, scenarios, n) {
    sample <- mvrnorm(n * scenarios, mu, Sigma)
    sample
}

# Define function to generate draws from normal copula
f_generate_copula_log_returns <- function(fit, scenarios, n, theta1, theta2) {
    U_sim <- rCopula(n * scenarios, fit@copula)
    rets1_sim <- qstd(U_sim[,1], 
                      mean = theta1[1], 
                      sd = theta1[2] * sqrt((theta1[3] - 2) / theta1[3]), 
                      nu = theta1[3])
                      
    rets2_sim <- qstd(U_sim[,2], mean = theta2[1], 
                      sd = theta2[2] * sqrt((theta2[3] - 2) / theta2[3]), 
                      nu = theta2[3])

    rets_sim <- cbind(rets1_sim, rets2_sim)
    rets_sim
}