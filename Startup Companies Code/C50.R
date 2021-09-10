# CS 513
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# C50 of All Variables

# Clear Environment
rm(list=ls())

# LOAD PACKAGES & LIBRARIES

## C5.0 Packages (Uncomment to Install)
# install.packages("C50", repos="http://R-Forge.R-project.org")
# install.packages("C50")

## C5.0 & Classification Libraries
library('C50')
library(e1071)
library(caret)

# EDIT DATASET
## Only include two-level categorical variables

## Import Dataset
filename <- file.choose()
Data = read.csv(filename, colClasses = c(rep("factor", 42)))

## Remove Columns of DisInterest
Data <- Data[c("Status","TeamSizeGrowth","TopCompanyExp","StartupExp",
               "SuccessfulStartupExp","Big5Partner","ConsultingExp",
               "ConsumerDataFocus", "SubscriptionBased","LocalGlobal",
               "BusinessModel","CapitalIntensive","CrowdsourcingBased",
               "CrowdfundingBased","B2BorB2C","GlobalExposure",
               "Fortune100Exp","Fortune500Exp","Fortune1000Exp",
               "PricingStrategy","HyperLocalisation",
               "LongtermFounderRelationship","RecessionSurvival")]

## Overview of Data
View(Data)
summary(Data)
prop.table(table(Data$Status))

# GENERATE C5.0 Tree
## Set seed
set.seed(111)

## Create training and test datasets
index<-sort(sample(nrow(Data),round(.25*nrow(Data))))
training<-Data[-index,]
test<-Data[index,]

## Grow Tree
C50_class <- C5.0(Status~.,data=training )
summary(C50_class )
plot(C50_class)

## Find Error Rate
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,1],C50=C50_predict)
wrong<- (test[,1]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,1])
c50_rate # = 0.16
