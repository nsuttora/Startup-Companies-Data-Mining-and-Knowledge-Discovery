# CS 513 FINAL PROJECT
# Classification and Regression Analysis (CART) for All Samples

# Clear Environment
rm(list=ls())

# LOAD PACKAGES & LIBRARIES

## Tree Packages (Uncomment to Install)
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("RColorBrewer")

## Tree Libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(rpart.plot)

## Classification Libraries
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

# Overview of Clean Dataset
View(Data)
summary(Data)
prop.table(table(Data$Status))

# GENERATE CART
## Set seed
set.seed(111)

# Create training and test datasets
index<-sort(sample(nrow(Data),round(.25*nrow(Data))))
training<-Data[-index,]
test<-Data[index,]

## Grow Tree
CART_class <- rpart(Status~., data=training)
rpart.plot(CART_class)
CART_predict2 <- predict(CART_class, test, type = "class") 
table(Actual = test[,1], CART = CART_predict2)
CART_predict <- predict(CART_class,test)
CART_predict <- predict(CART_class, test)
str(CART_predict)
CART_predict_cat <- ifelse(CART_predict[,1]<=.5,'Yes','No')

## Find Error Rates
table(Actual = test[,1], CART = CART_predict_cat)
CART_wrong <- sum(test[,1] != CART_predict_cat)
CART_error_rate <- CART_wrong / length(test[,1])
CART_error_rate # = 1
CART_predict2 <- predict(CART_class, test, type = "class")
CART_wrong2 <- sum(test[,1] != CART_predict2)
CART_error_rate2 <- CART_wrong2 / length(test[,1])
CART_error_rate2 # = 0.18

## Create Regular CART (Black & White)
prp(CART_class)

## Create Fancy CART (Color)
fancyRpartPlot(CART_class)

