

# Styling
mck_blue      <- "#0033A0"
mck_teal      <- "#00A3A1"
mck_grey      <- "#6D6E71"
mck_lightblue <- "#D9EAF7"
op <- par(family = "Arial")

# Data
library(MASS)
head(cats, n=3)
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
library(MASS)
dat <- MASS::cats
Bwt <- dat$Bwt
Hwt <- dat$Hwt



# 3A
plot(Bwt, Hwt,
     pch = 16, col = mck_grey,
     xlab = "Kroppsvekt (kg)", ylab = "Hjertevekt (g)",
     main = "3a: Hjertevekt vs Kroppsvekt")
fit <- lm(Hwt ~ Bwt)
abline(fit, col = mck_blue, lwd = 2)



# 3B
summary(fit)
coef(fit)
confint(fit, level = 0.95)



# 3C

newx <- seq(min(Bwt), max(Bwt), length.out = 100)
ci <- predict(fit, newdata = data.frame(Bwt = newx), interval = "confidence", level = 0.95)
pi <- predict(fit, newdata = data.frame(Bwt = newx), interval = "prediction", level = 0.95)

plot(Bwt, Hwt,
     pch = 16, col = mck_grey,
     xlab = "Kroppsvekt (kg)", ylab = "Hjertevekt (g)",
     main = "3c: Linje + 95% CI (smal) og 95% PI (bred)")
lines(newx, ci[,"fit"], col = mck_blue, lwd = 2)
lines(newx, ci[,"lwr"], col = mck_teal, lwd = 2, lty = 2)
lines(newx, ci[,"upr"], col = mck_teal, lwd = 2, lty = 2)
lines(newx, pi[,"lwr"], col = mck_lightblue, lwd = 2, lty = 3)
lines(newx, pi[,"upr"], col = mck_lightblue, lwd = 2, lty = 3)
legend("topleft",
       legend = c("Regresjonslinje","95% konfidensintervall","95% prediksjonsintervall"),
       col = c(mck_blue, mck_teal, mck_lightblue), lty = c(1,2,3), lwd = 2, bty = "n")