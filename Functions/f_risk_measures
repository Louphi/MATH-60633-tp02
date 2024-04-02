# Define a function to calculate the Value at Risk (VaR)
f_VaR <- function(y, alpha) {
    # Calculate the VaR at the specified confidence level
    VaR <- quantile(y, 1 - alpha)
    VaR
}

# Define a function to calculate the Expected Shortfall (ES)
f_ES <- function(y, alpha) {
    # Calculate the VaR using the previously defined function
    VaR <- f_VaR(y, alpha)
    # Calculate the ES as the mean of losses equal to or exceeding VaR
    ES <- mean(y[y <= VaR])
    ES
}
