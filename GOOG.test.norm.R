data <- read.csv("GOOG_AAPL.csv", stringsAsFactors = FALSE)
n <- nrow(data)
head(data)
GOOG <- data[, 2]
GOOG.r <- diff(log(GOOG))
GOOG.vol <- sd(GOOG.r) / sqrt(1 / 252) # volatility
acf(GOOG.r)
qqnorm(GOOG.r)
qqline(GOOG.r)