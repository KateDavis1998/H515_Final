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

#Set WD
#setwd('/Users/paigescott/Documents/IUPUI/INFO-H515/Project') #Paige
setwd('C:/Filing/My Docs/GitHub/H515_Final') #John

#Import Data
scans = read.csv("studies.csv")

#Dataset Structure
head(scans)
dim(scans)
summary(scans)

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

#fix fa_hrs to remove NULL
scans$fa_hrs = as.numeric(scans$fa_hrs)

#some plots
#hist of cf_hours - adjusted b/c of outliers
hist(scans$cf_hrs,
     main="CF_HRS", 
     xlab="cf_hrs",
     xlim=c(0,150),
     breaks=500)

#boxplots of cf_hours by shift
boxplot(cf_hrs ~ shift, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "cf_hrs by shift",
        ylab = "cf_hrs", 
        xlab = "Shift")

#boxplot of cf_hours by priority
boxplot(cf_hrs ~ priority, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "cf_hrs by priority",
        ylab = "cf_hrs", 
        xlab = "Priority")

#boxplot of cf_hours by resdict
boxplot(cf_hrs ~ resdict, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "cf_hrs by resdict",
        ylab = "cf_hrs", 
        xlab = "resdict")

#cf_hrs by orgcode
boxplot(cf_hrs ~ orgcode, 
        data = scans, 
        boxwex = .5, 
        border = "black", 
        col="gray",
        main = "cf_hrs by orgcode",
        ylab = "cf_hrs", 
        xlab = "orgcode")


#setting up Standard Deviation
cf_hours.mean = mean(scans$cf_hrs)
cf_hours.std = sd(scans$cf_hrs)
scans$cf_std <- with(scans, (abs(cf_hrs - cf_hours.mean))%/%cf_hours.std)
summary(scans$cf_std)
hist(scans$cf_std,
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
logscans = glm(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect, data=scans.min.train, family="binomial")
summary(logscans)
pred = predict(logscans, newdata=scans.min.test, type="response")
threshPred = (pred > .4) 
confusion.matrix = table(scans.min.test$cf_stdbin, threshPred)
confusion.matrix
accuracy = sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy
truePos = confusion.matrix[2,2]/sum(confusion.matrix[2,])
truePos
falsePos = confusion.matrix[1,2]/sum(confusion.matrix[1,])
falsePos


#Lasso/Ridge setup
#note - these take a long time to run, and have negative osr^2 
x.train=model.matrix(cf_std ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.train)
#x.train=model.matrix(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect,data=scans.min.train)
y.train=scans.min.train$cf_std
x.test=model.matrix(cf_std ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.test) 
#x.test=model.matrix(cf_stdbin ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect,data=scans.min.test) 
y.test=scans.min.test$cf_std
all.lambdas <- c(exp(seq(15, -10, -.1)))

#Ridge use binomial for binary predictor otherwise don't
#ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas, family = "binomial")
ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas, family = "binomial")
plot(ridge.cv)
best.lambda.ridge <- ridge.cv$lambda.min
ridge.mse.min <- ridge.cv$cvm[ridge.cv$lambda == ridge.cv$lambda.min]
ridge.mse.min

ridge.tr=glmnet(x = x.train, y = y.train, alpha=0, lambda=best.lambda.ridge)
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
r2.cart
error.cart = as.numeric(y.test) - pred.cart
sqrt(mean(error.cart^2)) #OSRMSE
sqrt(mean(abs(error.cart))) #OSMAE

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
svm.train = scans.min.train[,-c(1,2,18)]
svm.test = scans.min.test[,-c(1,2,18)]

#linear
svml <- svm(cf_stdbin~., data = svm.train, kernel = "linear",
               cost = 0.01, scale = FALSE)
summary(svml)
table(predict=predict(svml, svm.train), truth=svm.train$cf_stdbin) #train error
(0)/nrow(svm.train) #train error rate of 0?
table(predict=predict(svml, svm.test), truth=svm.test$cf_stdbin) #test error
(0)/nrow(svm.train) #test error rate of 0?

#the cost=0.01 model had 0 train/test errors but below is code for tuning in case I really messed up above
#this takes a long time to run
tuneModels<-tune(svm,cf_stdbin~., data = svm.train, kernel = "linear",
                 ranges=list(cost=c(.01,.05,.1,.5,1,2,3,4,5,6,7,8,9,10)))
summary(tuneModels)
tuneModels$best.model #all models have 0 error
bestLin=tuneModels$best.model
table(predict=predict(bestLin, svm.train), truth=svm.train$cf_stdbin) #train error
(0)/nrow(svm.train) #train error rate 0
table(predict=predict(bestLin, svm.test), truth=svm.test$cf_stdbin) #test error
(0)/nrow(svm.test) #test error rate 0

#poly
svmp <- svm(cf_stdbin~., data = svm.train, kernel = "polynomial",
            cost = 0.01, scale = FALSE)
summary(svmp)
table(predict=predict(svmp, svm.train), truth=svm.train$cf_stdbin) #train error
(685+0)/nrow(svm.train) #train error rate of 0.0303
table(predict=predict(svmp, svm.test), truth=svm.test$cf_stdbin) #test error
(293+0)/nrow(svm.train) #test error rate of 0.129

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

#Linear
svmparl <- parallelSVM(cf_stdbin~., data = svm.train[,-1],numberCores = detectCores(), kernel = "linear",
                      scale = FALSE, type = "C-classification", samplingSize = 0.4, probability = TRUE, cost = .01,
                      cross = 10, seed = 1234)
summary(svmparl)
pred.train=predict(svmparl, svm.train) #train error
table(pred.train, truth=svm.train$cf_stdbin) #train error
(351)/nrow(svm.train) #train error rate of 0.0155
pred.test=predict(svmparl, svm.test) #test error
table(pred.test, truth=svm.test$cf_stdbin) #test error
(155)/nrow(svm.train) #test error rate of 0.0068

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

