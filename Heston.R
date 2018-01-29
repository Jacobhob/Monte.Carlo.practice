set.seed(2)
data <- read.csv("GOOG_AAPL.csv", stringsAsFactors = FALSE)
n <- nrow(data)
GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r) / sqrt(1 / 252)
AAPL <- data[, 3]
AAPL.r <- diff(log(AAPL))
AAPL.vol <- sd(AAPL.r) / sqrt(1/252)
d <- 100000

r <- 0.02            # risk-free rate
theta <- GOOG.vol^2  # long-run mean
kappa <- 3           # speed of mean reversion
xi <- 0.5            # vol of vol
K <- 500

nu <- s <- rep(0, m + 1)  # nu is the list of vol
nu[1] <- theta
s[1] <- GOOG[length(GOOG)]
f <- rep(0, d)  # discounted payoff

for (j in 1:d) {
  for (i in 1:m) {
    dW1 <- sqrt(dt) * rnorm(1)
    dW2 <- sqrt(dt) * rnorm(1)
    ds <- r * s[i] * dt + sqrt(nu[i]) * s[i] * dW1
    dnu <- kappa * (theta - nu[i]) * dt + xi * sqrt(nu[i]) * dW2
    s[i + 1] <- s[i] + ds
    nu[i + 1] <- max(nu[i] + dnu, 0)
  }
  f[j] <- exp(-r * T) * max(mean(s) - K, 0)
}

cat("Option Price Estimate: ", round(mean(f), 4), "\n")
cat("Standard Error: ", round(sd(f) / sqrt(d), 4), "\n")

