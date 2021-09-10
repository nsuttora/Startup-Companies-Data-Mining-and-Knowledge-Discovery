# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

## k-Nearest Neighbor 



### Variables Used........
# age
# NumSeedInvestors
# NumAngelorVCInvestors
# NumFounders
# NumAdvisors
# SeniorLeadershipTeamSize
# NumFounderRecognition
# GooglePageRank
# NumDirectCompetitors
# EmployeesPerYear
# LastFundingRoundAmount
# FortuneExp


# start by clearing lists 
rm(list=ls())


library(e1071)
library(caTools)
library(class)
#Chose Cleaned_Data.csv
csvfile<-file.choose()
data<-  read.csv(csvfile)

View(data)

data <- na.omit(data)
data <- subset(data,select=-X)
data <- subset(data,select=-FocusFunctions)
data <- subset(data,select=-FoundingYear)

str(data)
library(dplyr)
data <- data %>% mutate_if(is.character,as.factor)
str(data)
data <- data %>% mutate_if(is.factor, as.numeric)
str(data)


# Splitting data into train
# and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")
# Feature Scaling
train_scale <- scale(train_cl[, 1:4])  
train_scale
test_scale <- scale(test_cl[, 1:4])  



############################ AGE -- kNN ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$Age, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.678571428571429"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 3)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.660714285714286"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$Age, k = 5)
misClassError <- mean(classifier_knn != test_cl$Age)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.589285714285714"
#########################
#########################




############################ NumSeedInvestors -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumSeedInvestors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.732142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.696428571428571"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumSeedInvestors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumSeedInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.642857142857143"
#########################
#########################



############################ NumAngelorVCInvestors -- kNN ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumAngelorVCInvestors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.696428571428571" 

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.732142857142857" 

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAngelorVCInvestors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumAngelorVCInvestors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.767857142857143"
#########################
#########################


############################ NumFounders -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumFounders, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.303571428571429"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounders, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumFounders)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.392857142857143"
#########################
#########################



############################ NumAdvisors -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumAdvisors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.25"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.303571428571429"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumAdvisors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumAdvisors)
print(paste('Accuracy =', 1-misClassError))
# "Accuracy = 0.339285714285714"
#########################
#########################



############################ SeniorLeadershipTeamSize -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$SeniorLeadershipTeamSize, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy = 0.125

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 3)
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy = 0.0714285714285714

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$SeniorLeadershipTeamSize, k = 5)
misClassError <- mean(classifier_knn != test_cl$SeniorLeadershipTeamSize)
print(paste('Accuracy =', 1-misClassError))
# Accuracy= 0.107142857142857
#########################
#########################



############################ NumFounderRecognition -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumFounderRecognition, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.160714285714286

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.160714285714286

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumFounderRecognition, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumFounderRecognition)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.232142857142857
#########################
#########################



############################ GooglePageRank -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$GooglePageRank, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0357142857142857

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 3)
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0535714285714286

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$GooglePageRank, k = 5)
misClassError <- mean(classifier_knn != test_cl$GooglePageRank)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.0892857142857143
#########################
#########################



############################ NumDirectCompetitors -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$NumDirectCompetitors, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.375"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 3)
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.375"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$NumDirectCompetitors, k = 5)
misClassError <- mean(classifier_knn != test_cl$NumDirectCompetitors)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"
#########################
#########################



############################ EmployeesPerYear -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$EmployeesPerYear, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 3)
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$EmployeesPerYear, k = 5)
misClassError <- mean(classifier_knn != test_cl$EmployeesPerYear)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.107142857142857"
#########################
#########################



############################ LastFundingRoundAmount -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$LastFundingRoundAmount, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#Accuracy = 0.196428571428571"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 3)
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.142857142857143"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$LastFundingRoundAmount, k = 5)
misClassError <- mean(classifier_knn != test_cl$LastFundingRoundAmount)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.178571428571429"
#########################
#########################



############################ FortuneExp -- kNN  ######################
# Fitting KNN Modelto training dataset
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 1)
classifier_knn
# Confusion Matrix
cm <- table(test_cl$FortuneExp, classifier_knn)
cm
# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.357142857142857"

# K = 3
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 3)
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.392857142857143"

# K = 5
classifier_knn <- knn(train = train_scale, test = test_scale, cl = train_cl$FortuneExp, k = 5)
misClassError <- mean(classifier_knn != test_cl$FortuneExp)
print(paste('Accuracy =', 1-misClassError))
#"Accuracy = 0.410714285714286"

#########################
#########################


