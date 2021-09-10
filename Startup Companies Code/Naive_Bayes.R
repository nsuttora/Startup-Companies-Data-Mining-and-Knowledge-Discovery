# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

## Naive Bayes

# used variables B2BorB2C, LocalGlobal, HighestEducation

rm(list=ls())

#install.packages('e1071', dependencies = TRUE)
 

library(e1071)
library(class) 



#Chose CleanedData.csv

csvfile<-file.choose()
cleanedData<-  read.csv(csvfile)


# don't use FocusFunctions and columns with no/very weak correlation and quantitative variables 
Data = cleanedData[, ! names(cleanedData) %in% c("FocusFunctions", "DataStructureFocus",
                                   "NumFounderRecognition", "NumSeedInvestors",
                                   "NumAngelorVCInvestors", "NumFounders",
                                   "TopCompanyExp", "ProductorService",
                                   "DataFocus", "StartupExp",
                                   "SuccessfulStartupExp", "Big5Partner",
                                   "BusinessModel", "CapitalIntensive",
                                   "GlobalExposure", "PricingStrategy",
                                   "HyperLocalisation", "Age", "FoundingYear", 
                                   "NumAdvisors", "SeniorLeadershipTeamSize",
                                   "GooglePageRank", "NumDirectCompetitors", 
                                   "EmployeesPerYear", "LastFundingRoundAmount", 
                                   "Fortune100Exp", "Fortune500Exp", "Fortune1000Exp", 
                                   "FortuneExp")]





#View(cleanedData)
#class(cleanedData)
#View(Data)
#class(Data)
#?prop.table
#prop.table


# train/test split (70% train, 30% test)
idx <- sort(sample(nrow(Data), as.integer(.70*nrow(Data))))
train <- Data[idx,]
test <- Data[-idx,]


#initial comparing of Status vs B2BorB2C
table(B2BorB2C=cleanedData$B2BorB2C,Status=cleanedData$Status)
prop.table(table(B2BorB2C=cleanedData$B2BorB2C,Status=cleanedData$Status)) 


## Naive Bayes classification using only one variable 
nBayes_B2BorB2C <- naiveBayes(Status ~B2BorB2C, data =train)
category_B2BorB2C<-predict(nBayes_B2BorB2C,test  )


## Compare the prediction to actual
data_B2BorB2C<-cbind(test,category_B2BorB2C)
#actual
table(B2BorB2C=test$B2BorB2C,Status=test$Status)
#predicted
table(B2BorB2C=test$B2BorB2C,NBayes=category_B2BorB2C)
#compared
table(NBayes=category_B2BorB2C,Status=test$Status)
prop.table(table(B2BorB2C=test$B2BorB2C,Status=test$Status))



## Naive Bayes classification using two variables 
nBayes_B2BorB2C_LocalGlobal <- naiveBayes(Status ~B2BorB2C+LocalGlobal, data =train)
category_B2BorB2C_LocalGlobal<-predict(nBayes_B2BorB2C_LocalGlobal,test  )


## Compare the prediction to actual for two variables
table(B2BorB2C=test$B2BorB2C,Status=test$Status)
#actual
ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,Status=test$Status)
#predicted
ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,NBayes=category_B2BorB2C_LocalGlobal)
#compared
ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,Status=test$Status,NBayes=category_B2BorB2C_LocalGlobal)
table(NBayes=category_B2BorB2C_LocalGlobal,Status=test$Status)



## Naive Bayes classification using 3 variables 
nBayes_B2BorB2C_LocalGlobal_HighestEducation <- naiveBayes(Status ~B2BorB2C+LocalGlobal+HighestEducation, data =train)
category_B2BorB2C_LocalGlobal_HighestEducation<-predict(nBayes_B2BorB2C_LocalGlobal_HighestEducation,test  )

#actual
ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,HighestEducation=test$HighestEducation,Status=test$Status,row.vars = 1:3)
prop.table(ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,HighestEducation=test$HighestEducation,Status=test$Status,row.vars = 1:3))

#predicted 
ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,HighestEducation=test$HighestEducation,
       NBayes=category_B2BorB2C_LocalGlobal_HighestEducation,row.vars = 1:3)
prop.table(
  ftable(B2BorB2C=test$B2BorB2C,LocalGlobal=test$LocalGlobal,HighestEducation=test$HighestEducation,
         NBayes=category_B2BorB2C_LocalGlobal_HighestEducation,row.vars = 1:3)
)
#compared
table(NBayes=category_B2BorB2C_LocalGlobal_HighestEducation,Status=test$Status)
prop.table(table(NBayes=category_B2BorB2C_LocalGlobal_HighestEducation,Status=test$Status))




#Testing all categories with NB and calculating error rate
nBayes_all <- naiveBayes(Status ~., data =train[,-1])

## Naive Bayes classification using all variables 

category_all<-predict(nBayes_all,test[,-1]  )
 

table(NBayes_all=category_all,Status=test$Status)

prop.table(table(NBayes_all=category_all,Status=test$Status))
NB_wrong<-sum(category_all!=test$Status)
NB_wrong
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

# 8 wrong -> 14 wrong
#error rate 0.1311475 -> 0.22
# total length of categories 61




