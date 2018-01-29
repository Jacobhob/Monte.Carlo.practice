library("ggplot2")

data <- read.csv("GOOG_AAPL.csv", stringsAsFactors = FALSE)
n <- nrow(data)
GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r) / sqrt(1 / 252)
AAPL <- data[, 3]
AAPL.r <- diff(log(AAPL))
AAPL.vol <- sd(AAPL.r) / sqrt(1/252)

Sigma <- cov(data.frame(GOOG.r, AAPL.r)) * 252
#all.equal(AAPL.vol^2, Sigma[2, 2])
C <- chol(Sigma)
#all.equal(Sigma, t(C) %*% C)

set.seed(2)
d <- 100000
stdNormal <- matrix(rnorm(d * 2), nrow = 2)
corrNormal <- t(C) %*% stdNormal

cov(t(corrNormal))
Sigma

K <- 400
T <- 1
r <- 0.05
s1 <- rep(0, 2)
s2 <- rep(0, 2)
f <- rep(0, d)
s1[1] <- GOOG[n]
s2[1] <- AAPL[n]

for (j in 1:d) {
  ret1 <- (r - 0.5 * GOOG.vol^2) * T + sqrt(T) * corrNormal[1, j]
  ret2 <- (r - 0.5 * AAPL.vol^2) * T + sqrt(T) * corrNormal[2, j]
  s1[2] <- s1[1] * exp(ret1) # Google's stock price at time T
  s2[2] <- s2[1] * exp(ret2) # Apple's stock price at time T
  f[j] <- exp(-r * T) * max(mean(c(s1[2], s2[2])) - K, 0)
}

cat("Option Price Estimate: ", round(mean(f), 4), "\n")
cat("Standard Error: ", round(sd(f) / sqrt(d), 4), "\n")
