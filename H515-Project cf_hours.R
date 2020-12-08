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

# 
# #setting up Standard Deviation
# cf_hours.mean = mean(scans$cf_hrs)
# cf_hours.std = sd(scans$cf_hrs)
# scans$cf_std <- with(scans, (abs(cf_hrs - cf_hours.mean))%/%cf_hours.std)
# summary(scans$cf_std)
# hist(as.numeric(scans$cf_std),
#      main="CF_STD", 
#      xlab="cf_std",
#      breaks=60)
# scans$cf_std <- factor(scans$cf_std)
# 
# #binary standard deviation
# scans$cf_stdbin <- with(scans, ifelse(cf_std==0,0,1))
# hist(scans$cf_stdbin,
#      main="CF_STDBIN", 
#      xlab="cf_stdbin",
#      breaks=2)
# scans$cf_stdbin <- factor(scans$cf_stdbin)

#For the cf_hrs analysis:
#removing oc_hours and of_hours as scans can be ordered for the future
#IE ordering a pizza for next week
#and what we are really interested in is how long it takes a scan to be finalized by the radiologist
#removing examdesc, as this is the long form of examcode and duplicative
#removing cd_hrs and df_hrs as these add up to the cf_hrs
scans.min <- scans[,-c(1,2,3,5,6,13)]
head(scans.min)

#splitting into test/train
set.seed(1234)
split = createDataPartition(scans.min$cf_hrs, p = 0.7, list = FALSE)
scans.min.train = scans.min[split,]
scans.min.test = scans.min[-split,]

#some basic regression
#note had to remove examcode/radiologist as it creates too many factors..
logscans = glm(cf_hrs ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect, data=scans.min.train)
summary(logscans)
pred<-predict(logscans, newdata=scans.min.test)
summary(pred)
SSE<-sum((pred-scans.min.test$cf_hrs)^2)
SST<-sum((mean(scans.min.train$cf_hrs)-scans.min.test$cf_hrs)^2)
OSR2<-1-SSE/SST
OSR2 #0.1111884

#Lasso/Ridge setup
#note - these take a long time to run, and have negative osr^2 
x.train=model.matrix(cf_hrs ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.train)
y.train=scans.min.train$cf_hrs
x.test=model.matrix(cf_hrs ~ shift+hr_cmpl+hr_dict+dow_cmpl+resdict+examcode+modality+priority+orgcode+eio+sect+radiologist,data=scans.min.test) 
y.test=scans.min.test$cf_hrs
all.lambdas <- c(exp(seq(15, -10, -.1)))

#Ridge use binomial for binary predictor otherwise don't
#ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas, family = "binomial")
ridge.cv=cv.glmnet(x = x.train, y = y.train, alpha=0, lambda=all.lambdas)
plot(ridge.cv)
best.lambda.ridge <- ridge.cv$lambda.min
ridge.mse.min <- ridge.cv$cvm[ridge.cv$lambda == ridge.cv$lambda.min]
ridge.mse.min #683.9811

ridge.tr=glmnet(x = x.train, y = y.train, alpha=0, lambda=best.lambda.ridge)
ridge.tr$beta # lots of values here
ridge.r2 = ridge.tr$dev.ratio
ridge.r2 #0.1084043
ridge.pred = predict(ridge.tr, newx=x.test, type="response")
SSE<-sum((ridge.pred-as.numeric(y.test))^2)
SST<-sum((mean(as.numeric(y.train))-as.numeric(y.test))^2)
ridge.OSR2<-1-SSE/SST
ridge.OSR2 #0.1092007
ridge.OSMSE<-SSE/nrow(scans.min.test)
ridge.OSMSE #416.8056

#Lasso use binomial for binary predictor otherwise don't
lasso.cv=cv.glmnet(x = x.train, y = y.train, alpha=1, lambda=all.lambdas)
plot(lasso.cv)
best.lambda.lasso <- lasso.cv$lambda.min
lasso.mse.min <- lasso.cv$cvm[lasso.cv$lambda == lasso.cv$lambda.min]
lasso.mse.min #699.7006

lasso.pred = predict(lasso.cv, newx=x.test, s = "lambda.min", type="response")
SSE<-sum((lasso.pred-as.numeric(y.test))^2)
SST<-sum((mean(as.numeric(y.train))-as.numeric(y.test))^2)
lasso.OSR2<-1-SSE/SST
lasso.OSR2 #0.06274808
lasso.OSMSE<-SSE/nrow(scans.min.test)
lasso.OSMSE #438.5409



#CART/Random Forest setup
#also negative for binary :/
#for binary use colum 18, for nonbinary use column 17
head(scans.min.train)

#non binary train/test
x.train = scans.min.train[,-c(1,6)]
x.test = scans.min.test[,-c(1,6)]
y.train = scans.min.train[,1]
y.test = scans.min.test[,1]

#CART
train.cart = train(x.train,y.train,method="rpart",
                   tuneGrid=data.frame(cp=seq(0, 0.02, 0.001)),trControl=trainControl(method="cv", number=10), minbucket=5)
train.cart$results[,"RMSE"]
train.cart$bestTune
mod.cart = train.cart$finalModel
pred.cart = predict(mod.cart, newdata=scans.min.test)
r2.cart = 1-sum((y.test - pred.cart)^2)/sum((y.test - mean(y.train))^2)
r2.cart #-0.08731799
error.cart = y.test - pred.cart
sqrt(mean(error.cart^2)) #OSRMSE 22.55564
sqrt(mean(abs(error.cart))) #OSMAE 2.559144

prp(mod.cart, varlen=0)
mod.cart

#these are baaad
train.cart$results


#Random Forest
#removing variables with more than 53 categories..
x.train = x.train[,-c(6,12)]
x.test = x.test[,-c(6,12)]
rf = randomForest(cf_hrs~shift+hr_cmpl+hr_dict+dow_cmpl+resdict+modality+priority+orgcode+eio+sect, data=scans.min.train, mtry = 3, nodesize=25, ntree = 80)
train.rf.oob = train(x.train, y.train, method="rf", tuneGrid=data.frame(mtry=1:10),
                     trControl=trainControl(method="oob"))

train.rf.oob$results

plot(train.rf.oob$results$mtry,train.rf.oob$results$Rsquared,
     xlab = "mtry",ylab = "Out-of-Bag R^2",type = "l")
#hey this plot is slightly less bad

#the train function stores the resulting final model
mod.rf = train.rf.oob$finalModel
mod.rf$mse

sqrt(tail(mod.rf$mse, 1)) #In Sample RMSE 25.13058
r2.is.rf = 1-sum((y.train - mod.rf$predicted)^2)/sum((y.train - mean(y.train))^2) #in sample R2?
r2.is.rf #0.1257353
error.is.rf = y.train - mod.rf$predicted
sqrt(mean(error.is.rf^2)) #ISRMSE double check 25.13058
sqrt(mean(abs(error.is.rf))) #ISMAE 2.084447

pred.rf = predict(mod.rf, newdata=x.test)
r2.rf = 1-sum((y.test - pred.rf)^2)/sum((y.test - mean(y.train))^2)
r2.rf #0.238487
error.rf = y.test - pred.rf
sqrt(mean(error.rf^2)) #OSRMSE 18.87624
sqrt(mean(abs(error.rf))) #OSMAE 2.008712
