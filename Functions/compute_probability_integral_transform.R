library("fGarch")

f_PIT_t <- function(x, theta) {
    U <- pstd(x, mean = theta[1], sd = theta[2] * sqrt((theta[3] - 2) / theta[3]), nu = theta[3])
    U
}