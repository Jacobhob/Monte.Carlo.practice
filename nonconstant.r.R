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
r <- s <- rep(0, m + 1)
r[1] <- r0
s[1] <- GOOG[length(GOOG)]
K <- 500
f <- rep(0, d)

for (j in 1:d) {
  r.int <- 0
  for (i in 1:m) {
    r.int <- r.int + r[i] * dt
    dW1 <- sqrt(dt) * rnorm(1)
    dW2 <- sqrt( dt) * rnorm(1)
    ds <- r[i] * s[i] * dt + GOOG.vol * s[i] * dW1
    dr <- k * (theta - r[i]) * dt + beta * dW2
    s[i + 1] <- s[i] + ds
    r[i + 1] <- r[i] + dr
  }
  f[j] <- exp(-r.int) * max(mean(s) - K, 0)
}

cat("Option Price Estimate: ", round(mean(f), 4), "\n")
cat("Standard Error:/ ", round(sd(f) / sqrt(d), 4), "\n")
