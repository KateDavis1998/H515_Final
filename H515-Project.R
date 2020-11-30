#Import Packages
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
#install.packages("openxlsx")
library(openxlsx)

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
library(caret)
library(ROCR)
set.seed(1234)
split = createDataPartition(scans.min$cf_stdbin, p = 0.7, list = FALSE)
scans.min.train = scans.min[split,]
scans.min.test = scans.min[-split,]

#some basic regression
#note had to remove examcode as it creates too many factors..
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

