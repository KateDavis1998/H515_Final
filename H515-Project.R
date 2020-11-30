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
#shift, priority, resdict
scans$shift <- factor(scans$shift)
scans$priority <- factor(scans$priority)
scans$resdict <- factor(scans$resdict, labels=c("no", "yes"))

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

#binary standard deviation
scans$cf_stdbin <- with(scans, ifelse(cf_std==0,0,1))
hist(scans$cf_stdbin,
     main="CF_STDBIN", 
     xlab="cf_stdbin",
     breaks=2)
