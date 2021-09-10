# CS 513 
# Final Project 
# Julia Nelson, Taylor Niedzielski, Sonia Patel, and Noah Suttora

# ANN on Cleaned Dataset (Noah Suttora)

# clear Environment
rm(list=ls())

# Import Dataset (Cleaned_Data.csv)
filename <- file.choose()
Data = read.csv(filename, colClasses = c(rep("factor", 42)))

# don't use FocusFunctions and columns with no/very weak correlation
Data = Data[, ! names(Data) %in% c("FocusFunctions", "DataStructureFocus",
                                   "NumFounderRecognition", "NumSeedInvestors",
                                   "NumAngelorVCInvestors", "NumFounders",
                                   "TopCompanyExp", "ProductorService",
                                   "DataFocus", "StartupExp",
                                   "SuccessfulStartupExp", "Big5Partner",
                                   "BusinessModel", "CapitalIntensive",
                                   "GlobalExposure", "PricingStrategy",
                                   "HyperLocalisation")]

# convert all columns to numeric
Data <- data.frame(lapply(Data, as.numeric))

# train/test split (70% train, 30% test)
idx <- sort(sample(nrow(Data), as.integer(.70*nrow(Data))))
train <- Data[idx,]
test <- Data[-idx,]

# neural network package
library("neuralnet")
library("NeuralNetTools")


# (1) neural network with 5 hidden layers
net_Data <- neuralnet(Status~., train, hidden=5, threshold=0.01)

# plot neural network
plot(net_Data)

# analyze neural network
names <- c('FY','Age','TSG','NA','SLTS','CE','CDF','SB','CPB','LG',
           'CSB','CFB','B2B/C','Edu','F100','F500','F1000','LFR','GPR','NDC',
           'EPY','LFRA','RS','FE')
garson(net_Data, x_lab=names)
garson(net_Data, bar_plot = FALSE)

# predict without Status column
ann <- compute(net_Data, test[,-1])
ann_cat <- ifelse(ann$net.result<1.2, 1, 2)

# confusion matrix
conf_mat = table(Actual=test$Status, prediction=ann_cat)
conf_mat

accuracy <- sum(diag(conf_mat))/nrow(test) * 100
accuracy


# (2) neural network with 10 hidden layers
net_Data2 <- neuralnet(Status~., train, hidden=10, threshold=0.01)

# plot neural network
plot(net_Data2)
garson(net_Data2, x_lab=names)
garson(net_Data2, bar_plot = FALSE)

# predict without Status column
ann2 <- compute(net_Data2, test[,-1])
ann_cat2 <- ifelse(ann2$net.result<1.2, 1, 2)

# confusion matrix
conf_mat2 = table(Actual=test$Status, prediction=ann_cat2)
conf_mat2

accuracy2 <- sum(diag(conf_mat2))/nrow(test) * 100
accuracy2