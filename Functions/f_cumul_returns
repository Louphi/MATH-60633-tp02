f_cumul_returns <- function(x, scenarios) {
    cumul_rets <- exp(t(apply(x, 1, cumsum)))
    cumul_rets <- data.frame(X0 = rep(1, scenarios), cumul_rets)
    cumul_rets
}