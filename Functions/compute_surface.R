f_compute_iv <- function(m, tau, alpha) {
    alpha_1 = alpha[1]
    alpha_2 = alpha[2]
    alpha_3 = alpha[3]
    alpha_4 = alpha[4]
    sig <- alpha_1 + alpha_2 * (m - 1)^2 + alpha_3 * (m - 1)^3 + alpha_4 * sqrt(tau)
    sig
}

f_error <- function(alpha, m, tau , obs_iv) {
    model_iv <- f_compute_iv(m, tau, alpha)
    error <- abs(obs_iv - model_iv)
    sum(error)
}