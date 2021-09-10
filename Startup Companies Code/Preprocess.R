# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# Dataset Clean-Up

# Clear Environment
rm(list=ls())

# Import Dataset (CAX_Startup_Data.csv)
filename <- file.choose()
RawData <- read.csv(filename, na.string = c("?"))
View(RawData)

# Remove Columns of DisInterest
Data <- RawData[-c(1,5,6,7,9,10,11,13,14,15,16,17,23,24,25,26,28,34,43,46,47,48,
                   49,50,51,52,53,54,56,57,59,61,62,63,64,65,66,67,71,73,74,75,
                   76,79,80,82,83,84,85,86,87,89,90,91,92,93,98,99,100,101,102,
                   103,104,105,106,107,108,109,110,111,112,113,114,115,116)]

# Rename Columns of Interest
colnames(Data) <- c("Status","FoundingYear","Age","FocusFunctions",
                    "TeamSizeGrowth","NumSeedInvestors","NumAngelorVCInvestors",
                    "NumFounders","NumAdvisors","SeniorLeadershipTeamSize", 
                    "TopCompanyExp","StartupExp","SuccessfulStartupExp",
                    "Big5Partner","ConsultingExp","ProductorService",
                    "DataFocus","ConsumerDataFocus","DataStructureFocus",
                    "SubscriptionBased","CloudPlatformBased","LocalGlobal",
                    "BusinessModel","CapitalIntensive","CrowdsourcingBased",
                    "CrowdfundingBased","B2BorB2C","GlobalExposure",
                    "HighestEducation","Fortune100Exp","Fortune500Exp",
                    "Fortune1000Exp","NumFounderRecognition","PricingStrategy",
                    "HyperLocalisation","LongtermFounderRelationship",
                    "GooglePageRank","NumDirectCompetitors","EmployeesPerYear",
                    "LastFundingRoundAmount","RecessionSurvival")

# Standardized NA
Data[Data == "No Info"] <- NA
Data[Data == "Not Applicable"] <- NA
Data[Data == "not applicable"] <- NA
Data[Data == ""] <- NA
Data[Data == "n"] <- NA
Data[Data == "N"] <- NA
Data[Data == "many"] <- NA
Data[Data == "\\"] <- NA

# Convert Character --> Integer
Data$NumSeedInvestors <- as.integer(Data$NumSeedInvestors, na.rm = TRUE)
Data$NumAngelorVCInvestors <- as.integer(Data$NumAngelorVCInvestors, na.rm = TRUE)
Data$NumFounderRecognition<-as.integer(Data$NumFounderRecognition, na.rm = TRUE)
Data$GooglePageRank <- as.integer(Data$GooglePageRank, na.rm = TRUE)
Data$NumDirectCompetitors <- as.integer(Data$NumDirectCompetitors, na.rm = TRUE)
Data$EmployeesPerYear <- as.integer(Data$EmployeesPerYear, na.rm = TRUE)
Data$LastFundingRoundAmount <- as.integer(Data$LastFundingRoundAmount, na.rm = TRUE)

#Populate NA with Mean of Column
Data[is.na(Data$NumAngelorVCInvestors),"NumAngelorVCInvestors"]<-mean(Data$NumAngelorVCInvestors,na.rm=TRUE) 
Data[is.na(Data$NumSeedInvestors),"NumSeedInvestors"]<-mean(Data$NumSeedInvestors,na.rm=TRUE) 
Data[is.na(Data$NumFounderRecognition),"NumFounderRecognition"]<-mean(Data$NumFounderRecognition,na.rm=TRUE) 
Data[is.na(Data$GooglePageRank),"GooglePageRank"]<-mean(Data$GooglePageRank,na.rm=TRUE) 
Data[is.na(Data$NumDirectCompetitors),"NumDirectCompetitors"]<-mean(Data$NumDirectCompetitors,na.rm=TRUE) 
Data[is.na(Data$EmployeesPerYear),"EmployeesPerYear"]<-mean(Data$EmployeesPerYear,na.rm=TRUE) 
Data[is.na(Data$LastFundingRoundAmount),"LastFundingRoundAmount"]<-mean(Data$LastFundingRoundAmount,na.rm=TRUE)

# Target Variable Class & Levels
Data$Status = as.factor(Data$Status)
levels(Data$Status)<-c('Failed','Success')

# FocusFunctions Column Class & Levels
Data$FocusFunctions <- tolower(Data$FocusFunctions)
Data$FocusFunctions = as.factor(Data$FocusFunctions)

# Convert Character --> Factor & Add Levels
Data$TeamSizeGrowth <- tolower(Data$TeamSizeGrowth)
Data$TeamSizeGrowth = as.factor(Data$TeamSizeGrowth)
levels(Data$TeamSizeGrowth)<-c('No','Yes')
Data$TopCompanyExp = as.factor(Data$TopCompanyExp)
levels(Data$TopCompanyExp)<-c('No','Yes')
Data$StartupExp = as.factor(Data$StartupExp)
levels(Data$StartupExp)<-c('No','Yes')
Data$SuccessfulStartupExp = as.factor(Data$SuccessfulStartupExp)
levels(Data$SuccessfulStartupExp)<-c('No','Yes')
Data$Big5Partner = as.factor(Data$Big5Partner)
levels(Data$Big5Partner)<-c('No','Yes')
Data$ConsultingExp = as.factor(Data$ConsultingExp)
levels(Data$ConsultingExp)<-c('No','Yes')
Data$ProductorService = as.factor(Data$ProductorService)
levels(Data$ProductorService)<-c('Both','Product', 'Service')
Data$DataFocus = as.factor(Data$DataFocus)
levels(Data$DataFocus)<-c('Both','no','Private','Public')
Data$ConsumerDataFocus = as.factor(Data$ConsumerDataFocus)
levels(Data$ConsumerDataFocus)<-c('No','Yes')
Data$DataStructureFocus = as.factor(Data$DataStructureFocus)
levels(Data$DataStructureFocus)<-c('Both','no', 'Structured','Unstructured')
Data$SubscriptionBased = as.factor(Data$SubscriptionBased)
levels(Data$SubscriptionBased)<-c('No','Yes')
Data$CloudPlatformBased <- tolower(Data$CloudPlatformBased)
Data$CloudPlatformBased = as.factor(Data$CloudPlatformBased)
levels(Data$CloudPlatformBased)<-c('both','cloud', 'none', 'platform')
Data$LocalGlobal <- tolower(Data$LocalGlobal)
Data$LocalGlobal = as.factor(Data$LocalGlobal)
levels(Data$LocalGlobal)<-c('global','local')
Data$BusinessModel = as.factor(Data$BusinessModel)
levels(Data$BusinessModel)<-c('Linear','Non-Linear')
Data$CapitalIntensive = as.factor(Data$CapitalIntensive)
levels(Data$CapitalIntensive)<-c('No','Yes')
Data$CrowdsourcingBased = as.factor(Data$CrowdsourcingBased)
levels(Data$CrowdsourcingBased)<-c('No','Yes')
Data$CrowdfundingBased = as.factor(Data$CrowdfundingBased)
levels(Data$CrowdfundingBased)<-c('No','Yes')
Data$B2BorB2C = as.factor(Data$B2BorB2C)
levels(Data$B2BorB2C)<-c('B2B','B2C')
Data$GlobalExposure = as.factor(Data$GlobalExposure)
levels(Data$GlobalExposure)<-c('No','Yes')
Data$HighestEducation = as.factor(Data$HighestEducation)
levels(Data$HighestEducation)<-c('Bachelors','Masters', 'PhD')
Data$PricingStrategy = as.factor(Data$PricingStrategy)
levels(Data$PricingStrategy)<-c('No','Yes')
Data$HyperLocalisation = as.factor(Data$HyperLocalisation)
levels(Data$HyperLocalisation)<-c('No','Yes')
Data$LongtermFounderRelationship = as.factor(Data$LongtermFounderRelationship)
levels(Data$LongtermFounderRelationship)<-c('No','Yes')
Data[is.na(Data$RecessionSurvival),"RecessionSurvival"]<-'Not Applicable'
Data$RecessionSurvival = as.factor(Data$RecessionSurvival)
levels(Data$RecessionSurvival)<-c('No','Not Applicable','Yes')

# Replace NA's of Yes/No Factor Columns with Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Data[is.na(Data$TeamSizeGrowth),"TeamSizeGrowth"]<-Mode(Data$TeamSizeGrowth)
Data[is.na(Data$LongtermFounderRelationship), "LongtermFounderRelationship"]<-Mode(Data$LongtermFounderRelationship)
Data[is.na(Data$RecessionSurvival), "RecessionSurvival"]<-Mode(Data$RecessionSurvival)
Data[is.na(Data$DataStructureFocus), "DataStructureFocus"]<-Mode(Data$DataStructureFocus)
Data[is.na(Data$LocalGlobal), "LocalGlobal"]<-Mode(Data$LocalGlobal)
Data[is.na(Data$BusinessModel), "BusinessModel"]<-Mode(Data$BusinessModel)
Data[is.na(Data$CapitalIntensive), "CapitalIntensive"]<-Mode(Data$CapitalIntensive)
Data[is.na(Data$GlobalExposure), "GlobalExposure"]<-Mode(Data$GlobalExposure)
Data[is.na(Data$HighestEducation), "HighestEducation"]<-Mode(Data$HighestEducation)
Data[is.na(Data$PricingStrategy), "PricingStrategy"]<-Mode(Data$PricingStrategy)
Data[is.na(Data$HyperLocalisation), "HyperLocalisation"]<-Mode(Data$HyperLocalisation)
Data[is.na(Data$Fortune100Exp), "Fortune100Exp"]<-Mode(Data$Fortune100Exp)
Data[is.na(Data$Fortune500Exp), "Fortune500Exp"]<-Mode(Data$Fortune500Exp)
Data[is.na(Data$Fortune1000Exp), "Fortune1000Exp"]<-Mode(Data$Fortune1000Exp)

#Get Combined Score for Fortune Company Experience 
Data$Fortune100Exp <- as.integer(Data$Fortune100Exp)
Data$Fortune500Exp <- as.integer(Data$Fortune500Exp)
Data$Fortune1000Exp <- as.integer(Data$Fortune1000Exp)
Data$FortuneExp <- Data$Fortune100Exp + Data$Fortune500Exp + Data$Fortune1000Exp

View(Data)
summary(Data)

# save cleaned data (change path accordingly)
write.csv(Data, "/CS513/Final/Cleaned_Data.csv", row.names = FALSE)