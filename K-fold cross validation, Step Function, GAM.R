#K-fold cross validation, Step Function, GAM
library(ISLR)
library(boot)
set.seed(1)
degree <- 10
cv.errs <- rep(NA, degree)
for (i in 1:degree) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}
plot(1:degree, cv.errs, xlab = 'Degree', ylab = 'Test MSE', type = 'l')
deg.min <- which.min(cv.errs)
points(deg.min, cv.errs[deg.min], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
age.range <- range(Wage$age)
age.grid <- seq(from = age.range[1], to = age.range[2])
fit <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

#b
cv.errs <- rep(NA, degree)
for (i in 2:degree) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}
plot(2:degree, cv.errs[-1], xlab = 'Cuts', ylab = 'Test MSE', type = 'l')
deg.min <- which.min(cv.errs)
points(deg.min, cv.errs[deg.min], col = 'red', cex = 2, pch = 19)

plot(wage ~ age, data = Wage, col = "darkgrey")
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

#8
set.seed(1)
pairs(Auto)

deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

cvs <- rep(NA, 10)
for (i in 2:10) {
  Auto$dis.cut <- cut(Auto$displacement, i)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

library(splines)
cvs <- rep(NA, 10)
for (i in 3:10) {
  fit <- glm(mpg ~ ns(displacement, df = i), data = Auto)
  cvs[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(3:10, cvs[-c(1, 2)], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

fit <- gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)