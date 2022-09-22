# School-Projects

4/12/2022 

Primary aim: to build a machine-learning model that accurately predicts Parkinson's Disease (PD) based on extracted features from patient voice recordings using RStudio/R. 
We aim to evaluate feature subset performance in accurately classifying PD patients as such, and to determine which of several different classification models has the highest accuracy for predicting PD.

Methods:
To know if we can figure out a good model, we’ll need to be able to cross-validate. We used statistical methods to estimate the accuracy of the models that we created on, on unseen data. We also wanted a more concrete estimate of the accuracy of the best model on unseen data by evaluating it on actual unseen data. So, we held back some data that the algorithms did not see, and we used this data to get a second and independent idea of how accurate the best model might be. The details of our methods are as follows:

Study Population:
Three voice recordings were collected from each of 252 subjects, a sample which included PD patients and healthy individuals. The age range for PD patients was 33 to 87 (65.1 ± 10.9) years, and for healthy individuals 41 to 82 (61.1 ± 8.9) years. 

Predictors and Outcome:
For each subject, seven feature subsets were extracted from each recording. The outcome variable “class” was dichotomous, coded as 0 (without PD) and 1 (having PD)

Data Pre-Processing:
As mentioned above, three recording samples were collected for each individual. Because this was triplicate data, to avoid introducing bias by including multiple measures from the same participant, we took the mean of the three measurements for each subject. The sample of 252 patients was split into training (n = 176) and test (n = 76) sets. We then standardized the feature vectors to achieve uniformity in mean and variance. 

Statistical Methods:
The training dataset was fed to multiple classifiers to distinguish between patients with 
and without PD. We calculated the overall accuracy for each of the following learning algorithms: 
Support Vector Machines (SVMs) with Linear and Radial Basis Function (RBF) kernels, Random Forest, Naive Bayes, Logistic Regression, Linear Discriminant Analysis (LDA), and K-Nearest Neighbors (k-NN) algorithms.


Below is the R code used in RStudio to find the best machine learning model

---
title: "Fundamentals of Data Analytics and Predictions (FINAL PROJECT)"
author: "Mira Baltaji, Asri Margono, Madeline Roberts, Muneeza Qureshi, Hyun Kim"
date: "4/14/2022"
output: html_document
---

Here will be the following codes for each model we utilized. 

1. Random Forest
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)
library(tree)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
ctrl = trainControl(method = "repeatedcv", number=10, savePredictions = "all", classProbs = TRUE)

set.seed(1)
train_RF = train(PD~. -class -id , data = train, method = "rf", preProcess = c("center","scale"), trControl = ctrl)
print(train_RF)

set.seed(1)
varImp(train_RF)

PD_RFTree = tree(PD ~. -class -id, data = train)
summary(PD_RFTree)
```

2. kNN 
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
train_index <- createDataPartition(train[,"PD"],p=0.75,list=FALSE)
data_trn <- train[train_index,]
data_tst <- train[-train_index,]
ctrl  <- trainControl(method  = "cv",number  = 10) #, summaryFunction = multiClassSummary

knn <- train(PD~. -class -id, data = data_trn, method = "knn",
             trControl = ctrl, 
             preProcess = c("center","scale"), 
             tuneGrid =data.frame(k=seq(5,100, by=5)))

pred <- predict(knn,data_tst)
confusionMatrix(table(data_tst[,"PD"],pred))
print(knn)
plot(knn)
```

3. SVM Linear
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
ctrl = trainControl(method = "repeatedcv", number=10, savePredictions = "all", classProbs = TRUE)

set.seed(1)
train_SVMLinear = train(PD~. -class -id , data = train, method = "svmLinear", preProcess = c("center","scale"), tuneLength = 20, trControl = ctrl)
print(train_SVMLinear)
```

4. SVM Radial
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train, PD)

set.seed(1)
train.index <- createDataPartition(train[,"PD"],p=0.70,list=FALSE)
train1 <- train[train.index,]
test1 <- train[-train.index,]

log_model = glm(PD~. -class -id, data = train1, family = binomial)
summary(log_model)

glm.probs = predict(log_model, test1, type = "response")
summary(glm.probs)

glm.probs = predict(log_model, train1, type = "response")
glm.pred = rep("0", length(glm.probs))
glm.pred[glm.probs > 0.5] = "1"

training_set = train1$PD[1:371]
length(training_set)
length(glm.probs)
table(glm.pred, training_set)

mean(glm.pred == training_set)
```

5. Logistic Regression
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
ctrl = trainControl(method = "repeatedcv", number=10, savePredictions = "all", classProbs = TRUE)

set.seed(1)
train_Log = train(PD~. -class -id , data = train, method = "glm", preProcess = c("center","scale"), trControl = ctrl)
print(train_Log)
```

6. Naive Bayes
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
ctrl = trainControl(method = "cv", number=10, savePredictions = "all", classProbs = TRUE)

set.seed(1)
train_NB = train(PD~. -class -id , data = train, method = "nb", metric = "Accuracy", trControl = ctrl)
print(train_NB)
```

7. LDA
```{r}
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)

set.seed(1)
setwd("/Users/davidkim/Desktop/Masters in Public Health/Spring 2022/Fundamentals of Data Analytics and Predictions/project")
train_p = read.csv("project_training set_p.csv", header=TRUE)

set.seed(1)
PD = factor (ifelse(train_p$class == 0, "NonPD", "PD"))
train = data.frame(train_p, PD)

set.seed(1)
ctrl = trainControl(method = "repeatedcv", number=10, savePredictions = "all", classProbs = TRUE)

set.seed(1)
train_LDA = train(PD~. -class -id , data = train, method = "lda", preProcess = c("center","scale"), trControl = ctrl)
print(train_LDA)
```

Prediction using Random Forest
```{r}
#Load library and dataset
options(warn=-1) 
library(readr) 
library(caret)
require(mlbench)
library(glmnet)
library(tree)

setwd("C:/Users/dhkki/Desktop")
train = read.csv("project_training set_p.csv", header=TRUE)
test = read.csv("project_test set_p.csv", header=TRUE)

#factor for column class
set.seed(1)
train$class = as.factor(train$class)
test$class = as.factor(test$class)
levels(train$class)=c("NonPD","PD")
levels(test$class)=c("NonPD","PD")

#training control
ctrl = trainControl(method = "repeatedcv", number=10, savePredictions = "all", classProbs = TRUE,  summaryFunction = twoClassSummary)

#Random Forest on train model
set.seed(1)
train_RF = train(class ~. -id, data = train, method = "rf", preProcess = c("center","scale"), tuneLength = 20, trControl = ctrl)
print(train_RF)

#prediction on test data
test.pred = predict(train_RF, newdata = test)
head(test.pred)

#predicting on test data with previous random forest model on training data
test_data = data.frame(test)
test.data$class = predict(train_RF, newdata = test)

#displaying final test data with the prediction
final_test = test_data[, c("id","class")]
final_test
```






