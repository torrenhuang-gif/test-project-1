#simplify SAMPLE
set.seed(45)
n <- 2000
k <- 5
beta <- 0.5
kx <- 0.6
ky <- 0.6
gamma <- c(0.5, 0.05, 0.6, 0.8, 0.1)

U <- rnorm(n)
G <- sapply(1:k, function(j) rbinom(n, 2, runif(1, 0.1, 0.5)))

X = as.vector(G%*%gamma) + kx*U + rnorm(n)
Y = beta*X + ky*U + rnorm(n)

#GWAS
#summary(lm(X ~ G[,5]))$coefficients
exposure <- do.call(rbind, lapply(1:k, function(j){
  s <- summary(lm(X ~ G[,j]))$coefficient[2,]
  data.frame(
    SNP = paste0("rs", j),
    effect_allele = "A",
    other_allele = "G",
    eaf = sum(G[, j])/(2*n),
    beta = unname(s[1]),
    se = unname(s[2]),
    pval = unname(s[4])
  )
} ))

outcome <- do.call(rbind, lapply(1:k, function(j) {
  s <- summary(lm(Y ~ G[, j]))$coefficient[2,]
  data.frame(
    SNP = paste0("rs", j),
    effect_allele = "A",
    other_allele = "G",
    eaf = sum(G[, j])/(2*n),
    beta = unname(s[1]),
    se = unname(s[2]),
    pval = unname(s[4])
  )
  }))

exposure

outcome
