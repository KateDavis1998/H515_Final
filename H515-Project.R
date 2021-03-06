#Import Packages
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
#install.packages("openxlsx")
library(openxlsx)
library(caret)
library(ROCR)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
#install.packages("parallelSVM")
library(parallelSVM)
library(parallel)
library(MASS)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("viridis")
library(viridis)
#install.packages("forcats")
library(forcats)
#install.packages("outliers")
library(outliers)
library(tibble)

#Set WD
#setwd('/Users/paigescott/Documents/IUPUI/INFO-H515/Project') #Paige
setwd('C:/Users/burns/OneDrive/Documents/GitHub/H515_Final') #John

#Import Data: studies.csv for all, studies_no_outliers.csv for outliers removed
scans = read.csv("studies_no_outliers.csv")

#Dataset Structure
head(scans)
dim(scans)
summary(scans)
count(scans)

#Set fields as factors
#shift, priority, resdict, examcode, modality, orgcode, eio, sect, radiologist
scans$shift <- factor(scans$shift)
scans$priority <- factor(scans$priority)
scans$resdict <- factor(scans$resdict, labels=c("no", "yes"))
scans$examcode <- factor(scans$examcode)
scans$modality <- factor(scans$modality)
scans$orgcode <- factor(scans$orgcode)
scans$eio <- factor(scans$eio)
scans$sect <- factor(scans$sect)
scans$radiologist <- factor(scans$radiologist)

#Analysis for Detecting Outliers & Extreme Outliers

#Grubbs Test for impact of outliers
  test_high <- grubbs.test(scans$cf_hrs)
  test_low <- grubbs.test(scans$cf_hrs, opposite= TRUE)
  test_high
  test_low 
  #High --> G = 58.42 U=0.933 (p-value <0.0001) 
  #Low  --> G = 46.40 U=0.8942 (p-value <0.0001)
  #Indicating our results are going to be impacted by outlier variables values (specifically cf_hrs= 1490.71, 12141.55, & -1173.95)
  
  #Rosner's Test to identify multiple outliers
  #install.packages("EnvStats")
  pdixon(p, 32228, type= 10, rev = FALSE)
  
  #Plot for Visualization
  mod <- lm(cf_hrs ~ ., data=scans)
  cooksd <- cooks.distance(mod)
  plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#fix fa_hrs to remove NULL
scans$fa_hrs = as.numeric(scans$fa_hrs)

#some plots
#hist of cf_hours - adjusted b/c of outliers
hist(scans$cf_hrs,
     main="Figure 1: Distribution of Turnaround Times", 
     xlab="Turnaround Time (Hours)", col="darkmagenta",
     xlim=c(0,50),
     breaks=500)

#boxplots of cf_hours by shift
boxplot(cf_hrs ~ shift, 
        data = scans, at = c(1:3) , names = c("First Shift", "Second Shift", "Third Shift"),
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "Figure 2: Turnaround (in Hours) by Practitioner Shift",
        ylab = "Turnaround Time (Hours)")

#boxplot of cf_hours by priority
boxplot(cf_hrs ~ priority, 
        data = scans, at = c(0:5) , names = c("0 (Lowest)", "1 (Low)", "2 (Low-Mid)", "3 (Mid)" , "4 (Mid-High)" , "5 (High)"),
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "Figure 3: Turnaround (in Hours) by Listed Priority Level",
        ylab = "Turnaround Time (Hours)", 
        xlab = "Priority Level")


#boxplot of cf_hours by resdict
boxplot(cf_hrs ~ resdict, 
        data = scans, at = c(0:1) , names = c("Practitioner", "Resident"),
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "Figure 4: Turnaround (in Hours) by Practitioner Training Level",
        ylab = "Turnaround Time (Hours)", 
        xlab = "Practitioner Training Level")

#cf_hrs by orgcode
boxplot(cf_hrs ~ orgcode, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "Figure 5: Turnaround (in Hours) by Organization",
        ylab = "Turnaround Time (Hours)", 
        xlab = "Organization Code")

# cf_hrs by sect
boxplot(cf_hrs ~ sect, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "Figure 5: Turnaround (in Hours) by Organization",
        ylab = "Turnaround Time (Hours)", 
        xlab = "Organization Code")


# Stratified Scatter Plots to show Turnaround Time by exam code between (Resident v. Practitioner ) & Sect
  ggplot( scans , aes(x= examcode, y= cf_hrs)) + geom_point() + facet_grid(resdict ~ sect )
  
  # Change data rownames as a real column called 'carName'
  data <- scans %>%
    rownames_to_column(var="Exam Code")
  
  # Plot
  ggplot(data, aes(x=examcode, y=cf_hrs)) +
    geom_point() + 
    geom_label( 
      data=scans %>% filter(cf_hrs> 360.0 ), # Filter data first
      aes(label= radiologist) , col="red",
    )
  
  qfdas <- (22.05*5)+5.59

#setting up Standard Deviation
cf_hours.mean = mean(scans$cf_hrs)
cf_hours.std = sd(scans$cf_hrs)
scans$cf_std <- with(scans, (abs(cf_hrs - cf_hours.mean))%/%cf_hours.std)
summary(scans$cf_std)
hist(as.numeric(scans$cf_std),
     main="CF_STD", 
     xlab="cf_std",
     breaks=60)
scans$cf_std <- factor(scans$cf_std)

#binary standard deviation
scans$cf_stdbin <- with(scans, ifelse(cf_std==0,0,1))
hist(scans$cf_stdbin,
     main="CF_STDBIN", 
     xlab="cf_stdbin",
     breaks=2)
scans$cf_stdbin <- factor(scans$cf_stdbin)

#For the cf_std and cf_stdbin analysis:
#removing oc_hours and of_hours as scans can be ordered for the future
#IE ordering a pizza for next week
#and what we are really insterested in is how long it takes a scan to be finalized by the radiologist
#removing examdesc, as this is the long form of examcode and duplicative
#removing cd_hrs and df_hrs as these add up to the cf_hrs
scans.min <- scans[,-c(1,2,3,5,13)]
head(scans.min)


#splitting into test/train
set.seed(1234)
split = createDataPartition(scans.min$cf_stdbin, p = 0.7, list = FALSE)
scans.min.train = scans.min[split,]
scans.min.test = scans.min[-split,]

#some basic regression
#note had to remove examcode/radiologist as it creates too many factors..
logscans = glm(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+as.numeric(priority)+orgcode+eio+sect, data=scans.min.train, family="binomial")
summary(logscans)
pred = predict(logscans, newdata=scans.min.test, type="response")
threshPred = (pred > .4) 
confusion.matrix = table(scans.min.test$cf_stdbin, threshPred)
confusion.matrix
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy #0.9708768
truePos = confusion.matrix[2,2]/sum(confusion.matrix[2,])
truePos #0.220339
falsePos = confusion.matrix[1,2]/sum(confusion.matrix[1,])
falsePos #0.005538986

#AUC for basic regression
pred_r <- prediction(pred, scans.min.test$cf_stdbin)
perf <- performance(pred_r, 'tpr', 'fpr')
plot(perf, main = "ROC Curve Basic Regression", colorize = T)
abline(a = 0, b = 1)
auc <- performance(pred_r, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.9283295

#Stepwise factor - https://bookdown.org/egarpor/PM-UC3M/lm-ii-modsel.html
logscans.full = glm(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+as.numeric(priority)+orgcode+eio+sect, data=scans.min.train, family="binomial")
logscans.min = glm(cf_stdbin ~ 1, data=scans.min.train, family="binomial")
step.reg = stepAIC(logscans.min, direction = "both", scope = list(lower = logscans.min, upper = logscans.full), k = log(nrow(scans)), trace=TRUE)
summary(step.reg)
pred = predict(step.reg, newdata=scans.min.test, type="response")
threshPred = (pred > .4) 
confusion.matrix = table(scans.min.test$cf_stdbin, threshPred)
confusion.matrix
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy #0.9702572
truePos = confusion.matrix[2,2]/sum(confusion.matrix[2,])
truePos #0.2135593
falsePos = confusion.matrix[1,2]/sum(confusion.matrix[1,])
falsePos #0.005965062

#AUC for stepwise factor regression
pred_r <- prediction(pred, scans.min.test$cf_stdbin)
perf <- performance(pred_r, 'tpr', 'fpr')
plot(perf, main = "ROC Curve Stepwise Factor", colorize = T)
abline(a = 0, b = 1)
auc <- performance(pred_r, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.9273478

#Stepwise (all numeric) - https://bookdown.org/egarpor/PM-UC3M/lm-ii-modsel.html
logscans.full = glm(cf_stdbin ~ as.numeric(shift)+hr_cmpl+hr_dict+dow_cmpl+as.numeric(resdict)+as.numeric(modality)+
                      as.numeric(priority)+as.numeric(orgcode)+as.numeric(eio)+as.numeric(sect)+as.numeric(radiologist)+as.numeric(examcode), data=scans.min.train, family="binomial")
logscans.min = glm(cf_stdbin ~ 1, data=scans.min.train, family="binomial")
step.reg = stepAIC(logscans.min, direction = "both", scope = list(lower = logscans.min, upper = logscans.full), k = log(nrow(scans)), trace=TRUE)
summary(step.reg)
pred = predict(step.reg, newdata=scans.min.test, type="response")
threshPred = (pred > .4) 
confusion.matrix = table(scans.min.test$cf_stdbin, threshPred)
confusion.matrix
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy #0.969431
truePos = confusion.matrix[2,2]/sum(confusion.matrix[2,])
truePos #0.05762712
falsePos = confusion.matrix[1,2]/sum(confusion.matrix[1,])
falsePos #0.001917341

#AUC for stepwise numeric regression
pred_r <- prediction(pred, scans.min.test$cf_stdbin)
perf <- performance(pred_r, 'tpr', 'fpr')
plot(perf, main = "ROC Curve Stepwise Numeric", colorize = T)
abline(a = 0, b = 1)
auc <- performance(pred_r, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.9066414

#Lasso/Ridge setup
#note - these take a long time to run, and have negative osr^2 
#x.train=model.matrix(cf_std ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.train)
x.train=model.matrix(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect,data=scans.min.train)
y.train=scans.min.train$cf_stdbin
#x.test=model.matrix(cf_std ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.test) 
x.test=model.matrix(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect,data=scans.min.test) 
y.test=scans.min.test$cf_stdbin
all.lambdas <- c(exp(seq(15, -10, -.1)))

#Ridge use binomial for binary predictor otherwise don't
ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas, family = "binomial")
#ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas, family = "multinomial")
plot(ridge.cv)
best.lambda.ridge <- ridge.cv$lambda.min
ridge.mse.min <- ridge.cv$cvm[ridge.cv$lambda == ridge.cv$lambda.min]
ridge.mse.min

ridge.tr=glmnet(x = x.train, y = y.train, alpha=0, lambda=best.lambda.ridge, family = "binomial")
ridge.tr$beta # all nonzero
ridge.r2 = ridge.tr$dev.ratio
ridge.r2
ridge.pred = predict(ridge.tr, newx=x.test, type="response")
SSE<-sum((ridge.pred-as.numeric(y.test))^2)
SST<-sum((mean(as.numeric(y.train))-as.numeric(y.test))^2)
ridge.OSR2<-1-SSE/SST
ridge.OSR2 
#This is negative for binary, so it fits worse than a horizontal line :/
#0.04562298 for the nonbinary version
ridge.OSMSE<-SSE/nrow(scans.min.test)
ridge.OSMSE

#Lasso use binomial for binary predictor otherwise don't
#lasso.cv=cv.glmnet(x = x.train, y = y.train, alpha=1, lambda=all.lambdas, family = "binomial")
lasso.cv=cv.glmnet(x = x.train, y = y.train, alpha=1, lambda=all.lambdas)
plot(lasso.cv)
best.lambda.lasso <- lasso.cv$lambda.min
lasso.mse.min <- lasso.cv$cvm[lasso.cv$lambda == lasso.cv$lambda.min]
lasso.mse.min

# This is how I did the assignment, but throws errors, which is indicative that this is the wrong thing to do
# https://stackoverflow.com/questions/32184776/glmnet-not-converging-for-lambda-min-from-cv-glmnet
# lasso.tr=glmnet(x.train,y.train, alpha=1, lambda=best.lambda.lasso, exact=FALSE, family = "binomial")
# lasso.tr$beta
# lasso.r2 = lasso.tr$dev.ratio
# lasso.r2

lasso.pred = predict(lasso.cv, newx=x.test, s = "lambda.min", type="response")
SSE<-sum((lasso.pred-as.numeric(y.test))^2)
SST<-sum((mean(as.numeric(y.train))-as.numeric(y.test))^2)
lasso.OSR2<-1-SSE/SST
lasso.OSR2 
#This is negative for binary, so it fits worse than a horizontal line :/
#0.0242218 for nonbinary
lasso.OSMSE<-SSE/nrow(scans.min.test)
lasso.OSMSE



#CART/Random Forest setup
#also negative for binary :/
#for binary use colum 18, for nonbinary use column 17

#binary train/test
x.train = scans.min.train[,-c(1,2,16,17,18)]
x.test = scans.min.test[,-c(1,2,16,17,18)]
y.train = scans.min.train[,ncol(scans.min.train)]
y.test = scans.min.test[,ncol(scans.min.train)]

#non binary train/test
x.train = scans.min.train[,-c(1,2,16,17,18)]
x.test = scans.min.test[,-c(1,2,16,17,18)]
y.train = scans.min.train[,ncol(scans.min.train)-1]
y.test = scans.min.test[,ncol(scans.min.train)-1]

#CART
train.cart = train(x.train,y.train,method="rpart",
                   tuneGrid=data.frame(cp=seq(0, 0.02, 0.001)),trControl=trainControl(method="cv", number=10), minbucket=5)
train.cart$results[,"RMSE"]
train.cart$bestTune
mod.cart = train.cart$finalModel
pred.cart = predict(mod.cart, newdata=scans.min.test)
r2.cart = 1-sum((as.numeric(y.test) - pred.cart)^2)/sum((as.numeric(y.test) - mean(as.numeric(y.train)))^2)
r2.cart #-35.96774 binary
error.cart = as.numeric(y.test) - pred.cart
sqrt(mean(error.cart^2)) #OSRMSE 0.7388973 binary
sqrt(mean(abs(error.cart))) #OSMAE 0.7283308 binary

prp(mod.cart, varlen=0)
mod.cart

train.cart$results


#Random Forest
#removing variables with more than 53 categories..
x.train = x.train[,-c(7,13)]
x.test = x.test[,-c(7,13)]
rf = randomForest(cf_std~shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect, data=scans.min.train, mtry = 3, nodesize=25, ntree = 80)
train.rf.oob = train(x.train, y.train, method="rf", tuneGrid=data.frame(mtry=1:10),
                     trControl=trainControl(method="oob"))

train.rf.oob$results

plot(train.rf.oob$results$mtry,train.rf.oob$results$Rsquared,
     xlab = "mtry",ylab = "Out-of-Bag R^2",type = "l")
#wow this plot is bad

#the train function stores the resulting final model
#this doesn't work for binary, not sure why
mod.rf = train.rf.oob$finalModel
mod.rf$mse

sqrt(tail(mod.rf$mse, 1)) #In Sample RMSE
r2.is.rf = 1-sum((y.train - mod.rf$predicted)^2)/sum((y.train - mean(y.train))^2) #in sample R2?
r2.is.rf #0.06324868
error.is.rf = y.train - mod.rf$predicted
sqrt(mean(error.is.rf^2)) #ISRMSE double check
sqrt(mean(abs(error.is.rf))) #ISMAE

pred.rf = predict(mod.rf, newdata=x.test)
r2.rf = 1-sum((y.test - pred.rf)^2)/sum((y.test - mean(y.train))^2)
r2.rf #0.09604545
error.rf = y.test - pred.rf
sqrt(mean(error.rf^2)) #OSRMSE 0.8211722
sqrt(mean(abs(error.rf))) #OSMAE 0.3473746





#Trying some SVM to predict binary
head(scans.min.train)
svm.train = scans.min.train[,-c(1,2,16)]
svm.test = scans.min.test[,-c(1,2,16)]
head(svm.train)
#linear
svml <- svm(cf_stdbin~., data = svm.train, kernel = "linear",
            cost = 0.01, scale = FALSE)
summary(svml)
pred.train=predict(svml, svm.train)
table(pred.train, truth=svm.train$cf_stdbin) #train error
(777)/nrow(svm.train) #train error rate of 0.03053503
pred.test=predict(svml, svm.test)
table(pred.test, truth=svm.test$cf_stdbin) #test error
(333)/nrow(svm.train) #test error rate of 0.01305483

#AUC for svm linear
pred_r <- prediction(as.numeric(pred.test), svm.test$cf_stdbin)
perf <- performance(pred_r, 'tpr', 'fpr')
plot(perf, main = "ROC Curve SVM Linear", colorize = T)
abline(a = 0, b = 1)
auc <- performance(pred_r, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.5

#the cost=0.01 model had 0 train/test errors but below is code for tuning in case I really messed up above
#this takes a long time to run
tuneModels<-tune(svm,cf_stdbin~., data = svm.train, kernel = "linear",
                 ranges=list(cost=c(.01,.05,.1,.5,1,2,3,4,5,6,7,8,9,10)))
summary(tuneModels)
tuneModels$best.model #best model cost=0.5
bestLin=tuneModels$best.model
table(predict=predict(bestLin, svm.train), truth=svm.train$cf_stdbin) #train error
(657+12)/nrow(svm.train) #train error rate 0.0296057
table(predict=predict(bestLin, svm.test), truth=svm.test$cf_stdbin) #test error
(285+7)/nrow(svm.test) #test error rate 0.03015594

#poly
svmp <- svm(cf_stdbin~., data = svm.train, kernel = "polynomial",
            cost = 0.01, scale = FALSE)
summary(svmp)
table(predict=predict(svmp, svm.train), truth=svm.train$cf_stdbin) #train error
(690+0)/nrow(svm.train) #train error rate of 0.03053503
table(predict=predict(svmp, svm.test), truth=svm.test$cf_stdbin) #test error
(295+0)/nrow(svm.train) #test error rate of 0.01305483

#radial
svmr <- svm(cf_stdbin~., data = svm.train, kernel = "radial",
            cost = 0.01, scale = FALSE)
summary(svmr)
table(predict=predict(svmr, svm.train), truth=svm.train$cf_stdbin) #train error
(690+0)/nrow(svm.train) #train error rate of 0.0305
table(predict=predict(svmr, svm.test), truth=svm.test$cf_stdbin) #test error
(295+0)/nrow(svm.train) #test error rate of 0.01305



#Trying some SVM to predict nonbinary
#clearly is not enough data to predict standard deviations
svm.train = scans.min.train[,-c(1,2,17)]
svm.test = scans.min.test[,-c(1,2,17)]
head(svm.train)
#linear
svml <- svm(cf_std~., data = svm.train, kernel = "linear", method="C-classification", cost = 0.01, scale = FALSE)
summary(svml)
predict=predict(svml, svm.test) #test error
xtab <- table(svm.test$cf_std, predict)
xtab #all predicted as 0

#poly
svmp <- svm(cf_std~., data = svm.train, kernel = "polynomial", method="C-classification", cost = 0.01, scale = FALSE)
summary(svmp)
predict=predict(svmp, svm.test) #test error
xtab <- table(svm.test$cf_std, predict)
xtab #also predicts everything as a 0

#radial
svmr <- svm(cf_std~., data = svm.train, kernel = "radial", method="C-classification", cost = 0.01, scale = FALSE)
summary(svmr)
predict=predict(svmr, svm.test) #test error
xtab <- table(svm.test$cf_std, predict)
xtab #also predicts everything as a 0



#why not use subsampling for SVM?
#using the parallelSVM library to speed up this process
#trying cf_stdbin first
svm.train = scans.min.train[,-c(1,2,16)]
svm.test = scans.min.test[,-c(1,2,16)]

#Linear
svmparl <- parallelSVM(cf_stdbin~., data = svm.train[,-1],numberCores = detectCores(), kernel = "linear",
                       scale = FALSE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = 2,
                       cross = 10, seed = 1234)
summary(svmparl)
pred.train=predict(svmparl, svm.train) #train error
table(pred.train, truth=svm.train$cf_stdbin) #train error
(17)/nrow(svm.train) #train error rate of (cost=.01, cross=10, 0.0155) (cost=2, cross=10, 0.0007080586) (cost=2, cross=100, 0.0007523123) 
pred.test=predict(svmparl, svm.test) #test error
table(pred.test, truth=svm.test$cf_stdbin) #test error
(8)/nrow(svm.train) #test error rate of (cost=.01, cross=10, 0.0068) (cost=2, cross=10, 0.0003540293) (cost=2, cross=100, 0.0003540293)

#Radial
svmparr <- parallelSVM(cf_stdbin~., data = svm.train[,-1],numberCores = detectCores(), kernel = "radial",
                       scale = TRUE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                       cross = 1, seed = 1234, gamma = 1)
summary(svmparr)
pred.train=predict(svmparr, svm.train) #train error
table(pred.train, truth=svm.train$cf_stdbin) #train error
(21907+1)/nrow(svm.train) #train error rate of 0.9695
pred.test=predict(svmparr, svm.test) #test error
table(pred.test, truth=svm.test$cf_stdbin) #test error
(9388+1)/nrow(svm.train) #test error rate of 0.4154

#Polynomial
svmparp <- parallelSVM(cf_stdbin~., data = svm.train[,-1],numberCores = detectCores(), kernel = "polynomial",
                       scale = TRUE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                       cross = 1, seed = 1)
summary(svmparp)
pred.train=predict(svmparp, svm.train) #train error
table(pred.train, truth=svm.train$cf_stdbin) #train error
(21907+1)/nrow(svm.train) #train error rate of 0.9695
pred.test=predict(svmparp, svm.test) #test error
table(pred.test, truth=svm.test$cf_stdbin) #test error
(9388+1)/nrow(svm.train) #test error rate of 0.4154


#trying cf_std 
#also doesn't work
#Linear
svmparl <- parallelSVM(cf_std~., data = svm.train[,-1],numberCores = detectCores(), kernel = "linear",
                       scale = FALSE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                       cross = 10, seed = 1234)
summary(svmparl)
pred = predict(svmparl, svm.test) #test error
xtab <- table(svm.test$cf_std, pred)
xtab #all predicted as 0


#Radial
svmparr <- parallelSVM(cf_std~., data = svm.train[,-1],numberCores = detectCores(), kernel = "radial",
                       scale = TRUE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                       cross = 1, seed = 1234, gamma = 1)
summary(svmparr)
pred = predict(svmparr, svm.test) #test error
xtab <- table(svm.test$cf_std, pred)
xtab #all predicted as 0

#Polynomial
svmparp <- parallelSVM(cf_std~., data = svm.train[,-1],numberCores = detectCores(), kernel = "polynomial",
                       scale = TRUE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                       cross = 1, seed = 1)
summary(svmparp)
pred = predict(svmparp, svm.test) #test error
xtab <- table(svm.test$cf_std, pred)
xtab #all predicted as 0





#redoing with a model with less columns to try to reduce number of inputs
#this takes a long time to run
set.seed(555)
head(scans.min.train) #4, 6, 7, 9, 10, 11, 12
svm.train = scans.min.train[,-c(1,2,16)]
svm.test = scans.min.test[,-c(1,2,16)]
#svm.train = scans.min.train[,-c(1,2,5,8,15,18)]
#svm.test = scans.min.test[,-c(1,2,5,8,15,18)]
tuneModels<-tune(svm,cf_stdbin~., data = svm.train, kernel = "linear",
                 ranges=list(cost=c(.01,.05,.1,.5,1,2,3,4,5,6,7,8,9,10)))
summary(tuneModels)
tuneModels$best.model #best model cost=2
bestLin=tuneModels$best.model
table(predict=predict(bestLin, svm.train), truth=svm.train$cf_stdbin) #train error
(0)/nrow(svm.train) #train error rate 0
table(predict=predict(bestLin, svm.test), truth=svm.test$cf_stdbin) #test error
(6)/nrow(svm.test) #test error rate 0.0006196427


#using the parallelSVM library to speed up this process
#Linear
svmparl <- parallelSVM(cf_stdbin~., data = svm.train,numberCores = detectCores(), kernel = "linear",
                       scale = FALSE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = 2,
                       cross = 10, seed = 1234)
summary(svmparl)
pred.train=predict(svmparl, svm.train) #train error
table(pred.train, truth=svm.train$cf_stdbin) #train error
(18)/nrow(svm.train) #train error rate of (cost=2, cross=10, 0.0007965659)
pred.test=predict(svmparl, svm.test) #test error
table(pred.test, truth=svm.test$cf_stdbin) #test error
(7)/nrow(svm.train) #test error rate of (cost=2, cross=10, 0.0003097756)
