####################################################################################################
### Project  : ICT Industry 4.0s
### Script   : DCS(function).R
### Contents : Document Classifier System Functions
### Date     : 2019. 09.23 
####################################################################################################

####################################################################################################
# 0.Setting up environment
####################################################################################################
# remove the rdata history
rm(list=ls())

# Load the packages that are needed in experiments
pkgs <- c("caret", "dplyr", "rpart", "adabag", "randomForest", "e1071")
sapply(pkgs, require, character.only = TRUE)

# load the text preprocessing functions
source("DCS_function.R", encoding = "UTF-8")

# rdata that contains result
load("DCS_result.RData")

####################################################################################################
# 1.Experiment  : Train 0.8 // Test : 0.2
####################################################################################################
# Dataset
View(df) 

# Result Table
Acc_Table 

# Show the best result : Stack_XGB, Random Forest and MDL show the highest accuracy
Acc_Table[which.max(Acc_Table)]  

# Show the confusion matrix
Confusion_Matrix

####################################################################################################
# 2.Load the text data and predict
####################################################################################################
# Root folder path
root_folder  <- "./문서"

# Folder list(Group by each document type)
folder_list  <- list.files(root_folder)

# Document Prediction Function
Doc_predict(folder_list)
