set.seed(42)

n <- 2000
beta <- 0.5
gamma <- 0.8
kx <- 0.6
ky <- 0.6
alpha <- 0

G <- rbinom(n, 2, 0.3)
U <- rnorm(n)
X <- gamma * G + kx * U + rnorm(n)
Y <- beta * X + ky * U + alpha * G + rnorm(n)

#OLS直接回归
fit_ols <- lm(Y ~ X)
fit_ols
summary(fit_ols)
coef(fit_ols)["X"]


#2SLS
stage1 <- lm(X ~ G)
summary(lm(X ~ G))$fstatistic[1]   
beta_xg <- coef(stage1)["G"]
stage2 <- lm(Y ~ G)
beta_yg <- coef(stage2)["G"]
beta_ygx <- beta_yg/beta_xg
beta_ygx


cat("真实 β =", beta,
    "\nOLS 估计 =", round(coef(fit_ols)["X"], 3),
    "\n2SLS 估计 =", round(beta_ygx, 3), "\n")
