#Gini Index, Cross entropy, Classification test errors
p = seq(0, 1, 0.001)
gini.index = 2 * p * (1 - p)
class.error = 1 - pmax(p, 1 - p)
cross.entropy = - (p * log(p) + (1 - p) * log(1 - p))
matplot(p, cbind(gini.index, class.error, cross.entropy), ylab = "gini.index, class.error, cross.entropy", col = c("green", "blue", "orange"))

#7
library(MASS)
library(randomForest)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
Boston.train <- Boston[train, -14]
Boston.test <- Boston[-train, -14]
Y.train <- Boston[train, 14]
Y.test <- Boston[-train, 14]
rf.boston1 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = ncol(Boston) - 1, ntree = 500)
rf.boston2 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = (ncol(Boston) - 1) / 2, ntree = 500)
rf.boston3 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = sqrt(ncol(Boston) - 1), ntree = 500)
plot(1:500, rf.boston1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), cex = 1, lty = 1)

#9
library(tree)
library(ISLR)
set.seed(3)
tr <- sample(1:nrow(OJ), 800)
oj.tr <- OJ[tr,]
oj.te <- OJ[-tr,]
tree.oj <- tree(Purchase ~., oj.tr)
summary(tree.oj)

tree.oj

#b
tree.OJ = tree(Purchase ~ ., data = OJtrain)
summary(tree.OJ)

plot(tree.OJ)
text(tree.OJ, pretty = 0)

#c
tree.oj

#d,e
plot(tree.oj, main='OJ Purchase Decision Tree')
text(tree.oj, pretty=0)

full.te.predict <- predict(tree.oj, oj.te, type='class')
table(full.te.predict, oj.te$Purchase)

mean(full.te.predict != oj.te$Purchase)

#f-k
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj
#g
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree size", ylab = "Cross Validation Error")
prune.oj <- prune.misclass(tree.oj, best = 2)
mean(predict(prune.oj, oj.tr, type='class')!=oj.tr$Purchase)
mean(predict(prune.oj, oj.te, type='class')!=oj.te$Purchase)

#10a
library(ISLR)
full.hit <- Hitters[!is.na(Hitters$Salary),]
full.hit$Salary <- log(full.hit$Salary)
#b
tr <- sample(1:nrow(full.hit), 200)
hit.tr <- full.hit[tr,]
hit.te <- full.hit[-tr,]

#c
shrinkage <- seq(0,0.03,0.00005)
tr.mse <- array(NA,length(shrinkage))
te.mse <- array(NA,length(shrinkage))

#d
library(gbm)

for (i in 1:length(shrinkage)) {
  hit.boost <- gbm(Salary ~., data=hit.tr, distribution='gaussian',
                   n.trees=1000, shrinkage=shrinkage[i], verbose=F)
  tr.mse[i] <- mean((predict(hit.boost, hit.tr, n.trees=1000) - hit.tr$Salary)^2)
  te.mse[i] <- mean((predict(hit.boost, hit.te, n.trees=1000) - hit.te$Salary)^2)
}

#e
min(te.mse)
hit.lm <- lm(Salary ~., hit.tr)
mean((predict(hit.lm, hit.te) - hit.te$Salary)^2)
library(glmnet)
library(Matrix)
hit.las.cv <- cv.glmnet(as.matrix(hit.tr[,-c(19,20,14,15)]),
                        as.matrix(hit.tr[,19]), alpha=1)
hit.las <- glmnet(as.matrix(hit.tr[,-c(19,20,14,15)]),
                  as.matrix(hit.tr[,19]), alpha=1, lambda=hit.las.cv$lambda.min)
mean((predict(hit.las,
              as.matrix(hit.te[,-c(19,20,14,15)])) - hit.te$Salary)^2)

#f
summary(hit.boost)

#g
library(randomForest)
hit.rf <- randomForest(Salary ~., hit.tr, mtry=(ncol(hit.tr)-1), importance=T)
mean((predict(hit.rf, hit.te) - hit.te$Salary)^2)






