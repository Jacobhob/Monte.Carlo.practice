library("ggplot2")

data <- read.csv("GOOG_AAPL.csv", stringsAsFactors = FALSE)
n <- nrow(data)
GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r) / sqrt(1 / 252)

set.seed(2)
d <- 100000
T <- 1
m <- T * 252 # number of subintervals
delta.t <- T/ m
r <- 0.05
K <- 500
s <- rep(0, times = m + 1)
f <- rep(0, times = d)
s[1] <- GOOG[n]
x <- 1:(T * 252 + 1)

for (j in 1:d) {
  for (i in 2:(m + 1)) {
    W <- sqrt(delta.t) * rnorm(1)
    ret <- (r - 0.5 * GOOG.vol^2) * delta.t + GOOG.vol * W
    s[i] <- s[i - 1] * exp(ret)
  }
  
  if (1 != 1) {
    stock <- data.frame(x, s)
    qplot(
      x,
      s,
      data = stock,
      geom = "line",
      xlab = "Time",
      ylab = "Stock price",
      main = "GOOG"
    )
  }
  
  f[j] <- exp(-r * T) * max(mean(s) - K, 0)
}

cat("Option Price Estimate: ", round(mean(f), 4), "\n")
cat("Standard Error: ", round(sd(f) / sqrt(d), 4), "\n")

