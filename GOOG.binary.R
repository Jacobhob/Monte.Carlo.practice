data <- read.csv("GOOG_AAPL.csv", stringsAsFactors = FALSE)
n <- nrow(data)
GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r) / sqrt(1 / 252)

d <- 500000
T <- 1
r <- 0.05
K <- 500
f <- rep(0, times = d)
for (j in 1:d) {
  W <- sqrt(T) * rnorm(1)
  s <- GOOG[n] * exp((r - 0.5 * GOOG.vol^2) * T + GOOG.vol * W)
  f[j] <- exp(-r * T) * if (s > K) 1 else 0
}
cat ("Option Price Estimate: ", round(mean(f), 4), "\n")
cat("Standard Error:", round(sd(f) / sqrt(d), 4), "\n")
