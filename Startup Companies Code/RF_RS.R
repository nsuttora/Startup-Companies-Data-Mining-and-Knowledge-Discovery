# CS 513 FINAL PROJECT
# Random Forest for Recession Survival Cases

# Clear Environment
rm(list=ls())

# LOAD PACKAGES & LIBRARIES

## Forest Packages (Uncomment to Install)
# install.packages("randomForest")

## Forest & Classification Libraries
library(randomForest)
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

## Change Not Applicatble to NA
Data[Data == "Not Applicable"] <- NA

## Remove Samples without RecessionSurvival Data
Data <- Data[-which(is.na(Data$RecessionSurvival)),]
View(Data)

## Change FortuneExp to Factor
Data$Fortune100Exp = as.factor(Data$Fortune100Exp)
levels(Data$Fortune100Exp)<-c('No','Yes')
Data$Fortune500Exp = as.factor(Data$Fortune100Exp)
levels(Data$Fortune100Exp)<-c('No','Yes')
Data$Fortune1000Exp = as.factor(Data$Fortune100Exp)
levels(Data$Fortune100Exp)<-c('No','Yes')

## Remove Rows with Missing Values
RFData <- na.omit(Data)

## Overview of RFData
View(RFData)
summary(RFData)
prop.table(table(RFData$Status))

# GENERATE FOREST
## Set seed
set.seed(111)

## Create training and test datasets
index <- sort(sample(nrow(RFData),round(.30*nrow(RFData))))
training <- RFData[-index,]
test <- RFData[index,]

## Grow Forest
Fit <- randomForest(Status~., data=training, importance=TRUE, ntree=1000)
importance(Fit)
varImpPlot(Fit)
Prediction <- predict(Fit, test)
table(actual = test[,1],Prediction)

## Find Error Rate
wrong <- (test[,1] != Prediction )
error_rate <- sum(wrong) / length(wrong)
error_rate 

# 0.2777778 All
# 0.2777778 Removed Crowdsourcing Based 44
# 0.2222222 Removed Local Global 40


