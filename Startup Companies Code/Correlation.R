# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# Dataset Correlation

# Clear Environment
rm(list=ls())

# Import Dataset (Cleaned_Data.csv)
filename <- file.choose()
Data = read.csv(filename, colClasses = c(rep("factor", 42)))

#1 = Failed, 2 = Success
Data$Status = as.integer(Data$Status)

# EDA Analysis #
table(Status = Data$Status, FoundingYear = Data$FoundingYear)
cor(as.integer(Data$FoundingYear), Data$Status)
# 0.1909444

table(Status = Data$Status, Age = Data$Age)
cor(as.integer(Data$Age), Data$Status)
# -0.1768085

table(Status = Data$Status, TeamSizeGrowth = Data$TeamSizeGrowth)
cor(as.integer(Data$TeamSizeGrowth), Data$Status)
# 0.238631

table(Status = Data$Status, NumSeedInvestors = Data$NumSeedInvestors)
cor(as.integer(Data$NumSeedInvestors), Data$Status)
# 0.03404118

table(Status = Data$Status, NumAngelorVCInvestors = Data$NumAngelorVCInvestors)
cor(as.integer(Data$NumAngelorVCInvestors), Data$Status)
# 0.05007803

table(Status = Data$Status, NumFounders = Data$NumFounders)
cor(as.integer(Data$NumFounders), Data$Status)
# 0.02735846

table(Status = Data$Status, NumAdvisors = Data$NumAdvisors)
cor(as.integer(Data$NumAdvisors), Data$Status)
# 0.1841515

table(Status = Data$Status, SeniorLeadershipTeamSize = Data$SeniorLeadershipTeamSize)
cor(as.integer(Data$SeniorLeadershipTeamSize), Data$Status)
# 0.215692

table(Status = Data$Status, TopCompanyExp = Data$TopCompanyExp)
cor(as.integer(Data$TopCompanyExp), Data$Status)
# 0.09679584

table(Status = Data$Status, StartupExp = Data$StartupExp)
cor(as.integer(Data$StartupExp), Data$Status)
# 0.09679584

table(Status = Data$Status, SuccessfulStartupExp = Data$SuccessfulStartupExp)
cor(as.integer(Data$SuccessfulStartupExp), Data$Status)
# 0.02047792

table(Status = Data$Status, Big5Partner = Data$Big5Partner)
cor(as.integer(Data$Big5Partner), Data$Status)
# 0.08710295

table(Status = Data$Status, ConsultingExp = Data$ConsultingExp)
cor(as.integer(Data$ConsultingExp), Data$Status)
# -0.191745

table(Status = Data$Status, ProductorService = Data$ProductorService)
cor(as.integer(Data$ProductorService), Data$Status)
# -0.09075112

table(Status = Data$Status, DataFocus = Data$DataFocus)
cor(as.integer(Data$DataFocus), Data$Status)
# 0.08567326

table(Status = Data$Status, ConsumerDataFocus = Data$ConsumerDataFocus)
cor(as.integer(Data$ConsumerDataFocus), Data$Status)
# 0.169483

table(Status = Data$Status, DataStructureFocus = Data$DataStructureFocus)
cor(as.integer(Data$DataStructureFocus), Data$Status)
# 0.007592873

table(Status = Data$Status, SubscriptionBased = Data$SubscriptionBased)
cor(as.integer(Data$SubscriptionBased), Data$Status)
# 0.1010671

table(Status = Data$Status, CloudPlatformBased = Data$CloudPlatformBased)
cor(as.integer(Data$CloudPlatformBased), Data$Status)
# -0.1032455

table(Status = Data$Status, LocalGlobal = Data$LocalGlobal)
cor(as.integer(Data$LocalGlobal), Data$Status)
# -0.3267174

table(Status = Data$Status, BusinessModel = Data$BusinessModel)
cor(as.integer(Data$BusinessModel), Data$Status)
# 0.0257441

table(Status = Data$Status, CapitalIntensive = Data$CapitalIntensive)
cor(as.integer(Data$CapitalIntensive), Data$Status)
# -0.0197335

table(Status = Data$Status, CrowdsourcingBased = Data$CrowdsourcingBased)
cor(as.integer(Data$CrowdsourcingBased), Data$Status)
# -0.1161668

table(Status = Data$Status, CrowdfundingBased = Data$CrowdfundingBased)
cor(as.integer(Data$CrowdfundingBased), Data$Status)
# 0.1186821

table(Status = Data$Status, B2BorB2C = Data$B2BorB2C)
cor(as.integer(Data$B2BorB2C), Data$Status)
# -0.2997825

table(Status = Data$Status, GlobalExposure = Data$GlobalExposure)
cor(as.integer(Data$GlobalExposure), Data$Status)
# 0.01746811

table(Status = Data$Status, HighestEducation = Data$HighestEducation)
cor(as.integer(Data$HighestEducation), Data$Status)
# 0.1470116

table(Status = Data$Status, Fortune100Exp = Data$Fortune100Exp)
cor(as.integer(Data$Fortune100Exp), Data$Status)
# 0.1599661

table(Status = Data$Status, Fortune500Exp = Data$Fortune500Exp)
cor(as.integer(Data$Fortune500Exp), Data$Status)
# 0.1576591

table(Status = Data$Status, NumFounderRecognition = Data$NumFounderRecognition)
cor(as.integer(Data$NumFounderRecognition), Data$Status)
# 0.001728026

table(Status = Data$Status, PricingStrategy = Data$PricingStrategy)
cor(as.integer(Data$PricingStrategy), Data$Status)
# -0.06275487

table(Status = Data$Status, HyperLocalisation = Data$HyperLocalisation)
cor(as.integer(Data$HyperLocalisation), Data$Status)
# -0.070683

table(Status = Data$Status, LongtermFounderRelationship = Data$LongtermFounderRelationship)
cor(as.integer(Data$LongtermFounderRelationship), Data$Status)
# 0.2276838

table(Status = Data$Status, GooglePageRank = Data$GooglePageRank)
cor(as.integer(Data$GooglePageRank), Data$Status)
# -0.02039728

table(Status = Data$Status, NumDirectCompetitors = Data$NumDirectCompetitors)
cor(as.integer(Data$NumDirectCompetitors), Data$Status)
# 0.06059326

table(Status = Data$Status, EmployeesPerYear = Data$EmployeesPerYear)
cor(as.integer(Data$EmployeesPerYear), Data$Status)
# 0.3364192

table(Status = Data$Status, LastFundingRoundAmount = Data$LastFundingRoundAmount)
cor(as.integer(Data$LastFundingRoundAmount), Data$Status)
# -0.1672832

table(Status = Data$Status, RecessionSurvival = Data$RecessionSurvival)
cor(as.integer(Data$RecessionSurvival), Data$Status)
# 0.2831691

table(Status = Data$Status, FortuneExp = Data$FortuneExp)
cor(as.integer(Data$FortuneExp), Data$Status)
# 0.2045905