simulate_mr <- function(n, beta, gamma, kx, ky, alpha) {
  G <- rbinom(n, 2, 0.3)
  U <- rnorm(n)
  X <- gamma * G + kx * U + rnorm(n)
  Y <- beta * X + ky * U + alpha * G + rnorm(n)
  data.frame(G, U, X, Y)
}

df <- simulate_mr(20000, beta = 0.6, gamma = 0.1, kx = 0.6, ky = 0.6, alpha = 0)
b_wald <- coef(lm(Y ~ G, df))["G"]/coef(lm(X ~ G, df))["G"]
b_2sls <-  coef(lm(Y ~ fitted(lm(X ~ G, df)), df))["fitted(lm(X ~ G, df))"]
c(wald = b_wald, tsls = b_2sls)

s1 <- summary(lm(X ~ G, df))
s1