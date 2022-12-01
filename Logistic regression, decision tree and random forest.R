install.packages("tidyverse")
install.packages("readxl")
install.packages("DMwR2")
install.packages("ROCR")
install.packages("caret")
install.packages('ggplot2')
library(ggplot2)
library("caret")
library(readxl)
library(ROSE)
library(DMwR2)
library(dplyr)
library(rpart)
library("rpart.plot")
library(tidyverse)
library(ROCR)
install.packages("Rcpp")
library(Rcpp)

file <- file.choose()
library(readxl)
manipulator <- read_excel(file, sheet =2)
nonmanipulator <- read_excel(file, sheet =3)
completedata <- read_excel(file, sheet =4)
modelsamples <- read_excel(file, sheet =5)


colnames(completedata)[1] <- "companyid"
colnames(completedata)[11] <- "cmanipulator"

completedata <- completedata[, -c(1,10)]
#Q1
cd2 <- completedata
fulldata <- completedata

cd2$Mscore <- (4.84*(-1)) + (0.92*cd2$DSRI) + (0.528*cd2$GMI) + (0.404*cd2$AQI) + (0.892*cd2$SGI) + (0.115*cd2$DEPI) + (0.172*(-1)cd2$SGAI) + (4.679*cd2$ACCR) + (0.327(-1)*cd2$LEVI)

cd2$Actual <- cd2$cmanipulator
cd2

mean(cd2$Mscore)

#Q2
table(is.na(completedata))
which(is.na(completedata))
#no na values found


#Q2 add theory

completedata$target <- as.factor(ifelse(completedata$cmanipulator == "1", 1, 0))

completedata <- completedata[, -9]
summary(completedata$target)


#MAKE A TEST AND TRAIN DATASET THEN BALANCE TRAIN DATA  SEE LEC NOTES
library(ROSE)

balanced.data1 <- ovun.sample(target ~., data = completedata, method = "under" , N = 1000)$data
summary(balanced.data$target)

balanced.data2 <- ovun.sample(target ~., data = completedata, method = "over" , N = 2000)$data
summary(balanced.data$target)

balanced.data3 <- ovun.sample(target ~., data = completedata, method = "both" , N = 2000)$data
summary(balanced.data$target)



#Q3


sample.data <- ovun.sample(target ~., data = completedata, method = "under" , N = 220)$data
summary(sample.data$target)
#to make sure all 39 values for which manipulation is 1 is covered in the smple data
set.seed(256)
indx <- sample(2, nrow(sample.data), replace = T, prob = c(0.8, 0.2))
train <- sample.data[indx == 1, ]
test <- sample.data[indx == 2, ]


logitModel <- glm(target ~ ., data = train, family = "binomial", maxit = 100)
summary(logitModel)

#variable selection
full<-lm(target ~.,data =  train)
null<-lm(target ~ 1, data = train)

# for a threshold of 5%(alpha), we have p values of the main variables less than alpha,
#hence making the model acceptable
#based on the p value from the model above GMI,DEPI,SGAI,LEVI have p value greater than alpha,
#therefor we can omit the following columns from our analysis

#Q4
logitModelnew <- glm(target ~ DSRI + AQI + SGI + ACCR, data = train, family = "binomial", maxit = 100)
summary(logitModelnew)

#based on the analysis(pvalue<alpha), we have obtained a new logistic regression model with 
#significant variables

#deviance(0 to infinity) smaller the number, the better the model fits the sample data
rd <- summary(logitModelnew)$deviance
rd
#pearson chi square test: smaller the value, better the model
pvalue<-1-pchisq(rd, 10)
pvalue


Pred <- predict(logitModelnew, newdata = test, type = "response")
Pred
Class <- as.factor(ifelse(Pred >= 0.5, 1, 0))
Class

t<-table(Class, test$target)
t
library("caret")
confusionMatrix(t)

#Q5



library("caret")

# Drawing evaluation charts
library(ROCR)
#pred <- prediction(logitModel$votes[, 2], as.factor(ifelse(as.character(sample.data$target) == "cmanipulator",1,0)))
pred <- prediction(Pred,test$target)

# Gain Chart
perf_gain <- performance(pred, "tpr", "rpp")
plot(perf_gain)

# Response Chart
perf_response <- performance(pred, "ppv", "rpp")
plot(perf_response)



# Lift Chart 
perf_lift <- performance(pred, "lift", "rpp")
plot(perf_lift)

# ROC Curve
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# auc
auc <- performance(pred, "auc")
auc
auc1 <- unlist(slot(auc, "y.values"))
auc1

# sensitivity/specificity curve (x-axis: specificity,
# y-axis: sensitivity)
perf_ss <- performance(pred, "sens", "spec")
perf_ss
plot(perf_ss)

# Identifying the optimal cut-off point from ROC curve using the distance to (0,1) approach
library(ROCR)
#pred<-prediction(rf$votes[,2], sample.data$target)

# We want to write a function that receives "perf". It then computes the distances of all the points in (perf@x.values, perf@y.values) from the point (0, 1) and finds the minimum

x <- unlist(perf@x.values)
y <- unlist(perf@y.values)
p <- unlist(perf@alpha.values)
mydistance <- function(x,y,p){
  d=(x-0)^2+(y-1)^2 
  # given the points (x, y), compute the distance to the corner point (0,1)
  ind <- which(d==min(d, na.rm =T)) 
  # Find the minimum distance and its index
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]]) # return the corresponding tpr, fpr and the cutoff point
}


opt.cut <- function(perf){
  cut.ind <- mapply(FUN = mydistance, 
                    perf@x.values, perf@y.values,perf@alpha.values)
}
Output <- opt.cut(perf)
print(Output[,1])

Threshold1 <- Output[,1]["cutoff"]
Threshold1


# Youden's formula online -- specificity + sensitivity - 1
# specificity is  1 - fpr
# sensitivity is tpr



foryouden <- function(x,y,p){
  m=y-x
  # given the points (x, y), compute the distance to the corner point (0,1)
  ind1 <-  which(m==max(m))
  # Find the minimum distance and its index
  c(recall = y[[ind1]], specificity = 1-x[[ind1]],cutoff = p[[ind1]]) # return the corresponding tpr, fpr and the cutoff point
}


opt.cut1 <- function(perf){
  cut.ind1 <- mapply(FUN = foryouden, 
                     perf@x.values, perf@y.values,perf@alpha.values)
}
Output1 <- opt.cut1(perf)
print(Output1[,1])

Threshold2 <- Output1[,1]["cutoff"]
Threshold2

# our threshold values 1 and 2 from the 2 functions are very close
#the reason is the very small dataset for our test
Pred <- predict(logitModelnew, newdata = test, type = "response")
Pred
predictedClass <- as.factor(ifelse(Pred >= Threshold, 1, 0))

t1<-table(predictedClass, test$target)
t1
library("caret")
confusionMatrix(t1)

#Q6.our threshold values on rerunning the model several times, oscillates
#between 0.2 to 0.6(aprx). A value of anywhere anound 0.4 would
#suggest a good cutoff point. 
# We can calculate the mscore here by using Y = 0.37
#ln(0.37/0.63) = -0.5 gives us the z value
# our data set is very small, hence the class and predicted class are almost the same

#Q7 Develop a decision tree model. What insights do you obtain from the tree model?
#newdata <- completedata[, -c(1,9)]

colnames(modelsamples)[1] <- "companyid"
colnames(modelsamples)[11] <- "cmanipulator"

modelsamples <- modelsamples[, -c(1,10)]

table(is.na(modelsamples))
which(is.na(modelsamples))

modelsamples$Target1 <- as.factor(ifelse(modelsamples$cmanipulator == "1", "manipulator", "nonmanipulator"))
modelsamples <- modelsamples[, -9]

summary(modelsamples$Target1)
modelsamples1 <- ovun.sample(Target1 ~., data = modelsamples, method = "under" , N = 100)$data
#this will help us in balancing the given dataset as number of manipulators are way lesser
#than non manipulators, But we wont use the newly found balanced data
#to make sure we get the most accurate result for the 220 samples
summary(modelsamples1$Target1)

#partition into train and test

indx <- sample(2, nrow(modelsamples), replace = T, prob = c(0.8, 0.2))
train3 <- modelsamples[indx == 1, ]
test3 <- modelsamples[indx == 2, ]
nrow(train3)
nrow(test3)
library(rpart)
library("rpart.plot")
decision_tree_model <- rpart(Target1 ~ ., train3)
print(decision_tree_model)
rpart.rules(decision_tree_model)

rpart.plot(decision_tree_model)
#LEVI, SGI, DSRI are our most important variables
#for the decision making using the Decision tree model
# To obtain the predicted classes or predicted probabilities we can use the "predict" function.
tree_pred_prob <- predict(decision_tree_model, train3)
tree_pred_prob <- predict(decision_tree_model, train3, type = "prob")
tree_pred_class <- predict(decision_tree_model, train3, type = "class")


# Error rate of the decision tree model on training data
mean(tree_pred_class != train3$Target1)

# Error rate of the decision tree model on test data
tree_pred_test <- predict(decision_tree_model, test3, type = "class")
base_error <- mean(tree_pred_test != test3$Target1)

# Changing the parameters of rpart
tree_model2 <- rpart(Target1 ~ ., train3, parms = list(split = "information"), control = rpart.control(minbucket = 0, minsplit = 0, cp = 0))
rpart.plot(tree_model2)

pred_test <- predict(tree_model2, test3, type = "class")
error_preprun <- mean(pred_test != test3$Target1)

mincp_i <- which.min(tree_model2$cptable[, 'xerror']) #the row (index) corresponding to the min xerror

# We can select the best cp in two different approach:
# 1
optCP <- tree_model2$cptable[mincp_i, "CP"]

# 2
#The optimal xerror is the min_xError + xstd
optError <- tree_model2$cptable[mincp_i, "xerror"] + tree_model2$cptable[mincp_i, "xstd"]
#the row(index) of the xerror value which is closest to optError
optCP_i <- which.min(abs( tree_model2$cptable[,"xerror"] - optError))
#finally, get the best CP value corresponding to optCP_i
optCP <- tree_model2$cptable[optCP_i, "CP"]
optCP


#Now we can prune the tree based on this best CP value
model_pruned  <- prune(tree_model2, cp = optCP)
rpart.plot(model_pruned)


# Compute the accuracy of the pruned tree
test3$predicted <- predict(model_pruned, test3, type = "class")
error_postprun <- mean(test3$predicted != test3$Target1)
df <- data.frame(base_error, error_preprun, error_postprun)
print(df)


#Q8

#finaldata <- ovun.sample(target ~., data = completedata, method = "over" , N = 2000)$data
fulldata$fullTarget <- as.factor(ifelse(fulldata$cmanipulator == "1", 1, 0))
fulldata <- fulldata[, -9]
summary(fulldata$fullTarget)

set.seed(1239)
indxfull <- sample(2, nrow(fulldata), replace = T, prob = c(0.8, 0.2))
trainfull <- fulldata[indxfull == 1, ]
testfull <- fulldata[indxfull == 2, ]


logitModelcomplete <- glm(fullTarget ~ ., data = trainfull, family = "binomial", maxit = 100)

summary(logitModelcomplete)
#the redundant variables in this are also the same as that of sample, i.e. GMI, DEPI,SGAI, LEVI
#Other thing noticed are the very small p values(ex: 2.49e^-13)
logitModelcomplete2 <- glm(fullTarget ~ DSRI + AQI + SGI +ACCR , data = trainfull, family = "binomial", maxit = 100)

summary(logitModelcomplete2)
#this is a better regression model

rdfull <- summary(logitModelcomplete2)$deviance
rdfull
#deviance value for the full model is slightly larger than the sample, showing sample as a slightly
#better model
pvaluefull<-1-pchisq(rdfull, 10)
pvaluefull


Predfull <- predict(logitModelcomplete2, newdata = testfull, type = "response")
Predfull
Classfull <- ifelse(Predfull >= 0.5, 1, 0)
Classfull

tfull<-table(Classfull, testfull$fullTarget)
tfull
library("caret")
confusionMatrix(tfull)



library("caret")

# Drawing evaluation charts
library(ROCR)
#pred <- prediction(logitModel$votes[, 2], as.factor(ifelse(as.character(sample.data$target) == "cmanipulator",1,0)))
predfull2 <- prediction(Predfull,testfull$fullTarget)

# Gain Chart
perf_gain2 <- performance(predfull2, "tpr", "rpp")
plot(perf_gain2)

# Response Chart
perf_response2 <- performance(predfull2, "ppv", "rpp")
plot(perf_response2)



# Lift Chart 
perf_lift2 <- performance(predfull2, "lift", "rpp")
plot(perf_lift2)

# ROC Curve
perf2 <- performance(predfull2, "tpr", "fpr")
plot(perf2)

# auc
aucfull <- performance(predfull2, "auc")
aucfull
aucfull1 <- unlist(slot(aucfull, "y.values"))
aucfull1

# sensitivity/specificity curve (x-axis: specificity,
# y-axis: sensitivity)
perf_ss2 <- performance(predfull2, "sens", "spec")
perf_ss2
plot(perf_ss2)

# Identifying the optimal cut-off point from ROC curve using the distance to (0,1) approach
library(ROCR)

# We want to write a function that receives "perf". It then computes the distances of all the points in (perf@x.values, perf@y.values) from the point (0, 1) and finds the minimum

x1 <- unlist(perf2@x.values)
y1 <- unlist(perf2@y.values)
p1 <- unlist(perf2@alpha.values)
mydistancefull <- function(x1,y1,p1){
  d1=(x1-0)^2+(y1-1)^2 
  # given the points (x, y), compute the distance to the corner point (0,1)
  indfull <- which(d1==min(d1, na.rm =T)) 
  # Find the minimum distance and its index
  c(recall = y1[[indfull]], specificity = 1-x1[[indfull]],cutoff = p1[[indfull]]) # return the corresponding tpr, fpr and the cutoff point
}


opt.cutfull <- function(perf2){
  cut.indfull <- mapply(FUN = mydistancefull, 
                        perf2@x.values, perf2@y.values,perf2@alpha.values)
}
Outputfull <- opt.cut(perf2)
print(Outputfull[,1])

Thresholdfull <- Outputfull[,1]["cutoff"]
Thresholdfull

#threshold value is lower than that of the sample.


# Youden's formula online -- specificity + sensitivity - 1
# specificity is  1 - fpr
# sensitivity is tpr

foryoudenfull <- function(x1,y1,p1){
  m1=y1-x1
  # given the points (x, y), compute the distance to the corner point (0,1)
  indfull1 <-  which(m1==max(m1))
  # Find the minimum distance and its index
  c(recall = y[[indfull1]], specificity = 1-x[[indfull1]],cutoff = p[[indfull1]]) # return the corresponding tpr, fpr and the cutoff point
}


opt.cutfull1 <- function(perf2){
  cut.indfull1 <- mapply(FUN = foryoudenfull, 
                         perf2@x.values, perf2@y.values,perf2@alpha.values)
}
Outputfull1 <- opt.cutfull1(perf2)
print(Outputfull1[,1])

Thresholdfull1 <- Outputfull1[,1]["cutoff"]
Thresholdfull1

Predfull <- predict(logitModel, newdata = testfull, type = "response")
Predfull
predictedClassfull1 <- as.factor(ifelse(Predfull >= Thresholdfull, 1, 0))
tfull1<-table(predictedClassfull1, testfull$fullTarget)
tfull1

predictedClassfull2 <- as.factor(ifelse(Predfull >= Thresholdfull1, 1, 0))
tfull2<-table(predictedClassfull2, testfull$fullTarget)
tfull2


library("caret")
confusionMatrix(tfull1)
confusionMatrix(tfull2)

#overall sample data has better accuracy and higher threshold