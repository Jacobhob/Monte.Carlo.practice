r0 <- 0.02     # initial interest rate
theta <- 0.04  # mean interest rate
k <- 2         # speed of mean reversion kappa
beta <- 0.03   # volatility parameter

n <- 10
T <- 1
m <- T * 252
dt <- T / m

r <- matrix(rep(0, (m + 1) * n), ncol = n)
r[1, ] <- r0

for (j in 1:n) {
  for (i in 2:(m + 1)) {
    dr <-  k * (theta - r[i - 1, j]) * dt + beta * sqrt(dt) * rnorm(1)
    r[i, j] <- r[i -1, j] + dr
  }
}

t <- seq(0, T, dt)
matplot(t, r[, 1:10], type = "l", lty = 1,
        main = "Short Rate Paths", ylab = "rt")
abline(h = theta, col = "red", lty = 2)
points(0, r0)

rT.expected <- theta + (r0 - theta) * exp(-k * t)
rT.stdev <- sqrt(beta^2 / (2 * k) * (1 - exp(-2 * k * t)))
lines(t, rT.expected, lty = 2)
lines(t, rT.expected + 1.96 * rT.stdev, lty = 2)
lines(t, rT.expected - 1.96 * rT.stdev, lty = 2)
