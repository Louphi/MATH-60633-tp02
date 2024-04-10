# Define a function for linear interpolation of risk-free rates
f_linear_interpolation <- function(years, maturities, rates) {
  # Create interpolation function for known maturities and rates
  interp_func <- approxfun(maturities, rates, method = "linear")
  
  # Interpolate the rate for given maturity
  interpolated_rate <- interp_func(years)
  interpolated_rate
}
