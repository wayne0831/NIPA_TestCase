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

# Dataframe which contains information of each text data's TF-IDF
load("DCS.RData")

####################################################################################################
# 1.Experiment with all classes : Train 0.8 // Test : 0.2
####################################################################################################
# contain all claases' data and results of several algorithms
load("DCS_AllClasses.Rdata")

# Dataset
View(dtm.all) 

# Result Table
Error_Table 

# Show the best result
#Stack_XGB shows the least error rate
Error_Table[which.min(Error_Table)]  

# Show the confusion matrix
cfusion.s.xgb  #'회의록' class can be removed

####################################################################################################
# 2.Experiment without '회의록' class : Train 0.8 // Test : 0.2
####################################################################################################
# remove the rdata history
rm(list=ls())

load("DCS_ClassRemoved.Rdata")

# Dataset
View(dtm.all) 

# Result Table
Error_Table2 

# Show the best result
#Stack_XGB shows the least error rate
Error_Table2[which.min(Error_Table2)] 

# Show the confusion matrix
cfusion.s.xgb 

####################################################################################################
# 3.Load the text data and predict
####################################################################################################
# remove the rdata history
rm(list=ls())

# load the text preprocessing functions
source("DCS_function.R", encoding = "UTF-8")

# load the best algorithm : Stak_XGB
load("DCS_AllClasses.Rdata")

# Root folder path
root_folder  <- "./문서"

# Folder list(Group by each document type)
folder_list  <- list.files(root_folder)

# train data frame
df.tmp <- data.frame()
for (i in 1:NROW(folder_list)){
  folder_path <- paste0(root_folder, "/", folder_list[i], "/")
  tmp         <- get_folder_info(folder_path)
  df.tmp      <- rbind(df.tmp, tmp)}

df.tmp$doc_id <- 1:NROW(df.tmp)
df.tmp        <- df.tmp %>% select(doc_id, Path, Type, Name, Extension, text)

# Create the document term matrix and Remove the doc_id column
df.tmp        <- create_dtm(df.tmp)
df.tmp        <- df.tmp[, -1]

# rearrange the column of df.tmp to equalize with dtm.all
order <- which(colnames(df.tmp) == colnames(dtm.all[,-1])[1])
for(i in 2:40){ order <- cbind(order, which(colnames(df.tmp) == colnames(dtm.all[,-1])[i])) }

# set df.tmp as test data
test <- df.tmp[, order]

# load the result of Stack_XGB 
test_st     <- data.frame(Type        = test$Type,
                          tr_predict  = predict(dt_fit, newdata = test),
                          rf_predict  = predict(rf_fit2, newdata = test),
                          xgb_predict = predict(xgb_fit, newdata = test),
                          svm_predict = predict(tuned.svm, newdata = test),
                          mdl_predict = predict(mdl_fit, newdata = test),
                          bag_predict = predict(bag_fit, newdata = test))

# see the prediction
predict(xgb_st, test_st) 

# see the actual value
test_st$Type 
