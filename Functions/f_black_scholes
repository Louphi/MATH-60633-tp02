# Define a function to calculate the price of a European option using the Black-Scholes formula
f_black_scholes <- function(S, K, r, T, sigma, type) {
  # S: Current stock price
  # K: Strike price of the option
  # r: Risk-free interest rate
  # T: Time to expiration (in years)
  # sigma: Volatility of the stock
  # type: Type of option ("C" for Call, "P" for Put)

  # Calculate the call option price if the type is "C"
  if(type == "C") {
    d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)

    # Black-Scholes formula for call option
    price = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(price)
  }

  # Calculate the put option price if the type is "P"
  if (type == "P") {
    d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)

    # Black-Scholes formula for put option
    price =  K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
    return(price)
  }
}
