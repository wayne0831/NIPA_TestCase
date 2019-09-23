####################################################################################################
### Project  : ICT ��?? Industry 4.0s(��??/?ؾ?) ???????߻???
### Script   : DCS.R
### Contents : Document Classifier System
####################################################################################################

####################################################################################################
### Setting up environment
####################################################################################################
# Remove previous data
rm(list=ls())

####################################################################################################
# setting
####################################################################################################
pkgs <- c("caret", "dplyr", "rpart", "adabag", "randomForest", "e1071", "NbClust")

sapply(pkgs, require, character.only = TRUE)
load("DCS.RData")

# data preprocessing(remove doc_id column)
df        <- dtm.all[, -1]

# remove '회의록' class
df$Type   <- as.character(df$Type)
df        <- df[df$Type != '회의록', ]
df$Type   <- as.factor(df$Type)

set.seed(13)
idx   <- createDataPartition(df$Type, p = 0.8, list = F)
train <- df[idx, ]
test  <- df[-idx, ]

####################################################################################################
# train the model - 5 cross validation 
####################################################################################################
## Decision Tree
dt_fit       <- train(Type~., method = 'rpart', data = train)
test.tr      <- predict(dt_fit, newdata = test)
cfusion.e.tr <- table(test.tr, test$Type) 
error.e.tr   <- sum(test.tr != test$Type) / (nrow(df) - length(idx))

## Random Forest
fit_Control <- trainControl(method = 'cv', number = 5)
df$Type     <- as.factor(df$Type)

# grid search
rf_fit2      <- train(Type~., data = train, method = 'rf', ntree = 500, 
                      trControl = fit_Control, tuneGrid = expand.grid(mtry = 5:15), verbose = F)
test.rf      <- predict(rf_fit2, newdata = test)
cfusion.e.rf <- table(test.rf, test$Type) 
error.e.rf   <- sum(test.rf != test$Type) / (nrow(df) - length(idx))

## XGBoost
tune_grid     <- expand.grid(nrounds = seq(50, 100, 10), eta = seq(0.05, 0.5, 0.05),
                             max_depth = c(3:6), gamma = seq(0, 0.5, 0.05),
                             colsample_bytree = 1, min_child_weight = 1, subsample =1)

train_control <- trainControl(method = 'cv', number = 5, verboseIter = FALSE, allowParallel = TRUE)

xgb_fit       <- train(x = select(train, -Type), y = train$Type, trControl = train_control, 
                       #tuneGrid = tune_grid, 
                       method = 'xgbTree')

test.xgb      <- predict(xgb_fit, newdata = test)
cfusion.e.xgb <- table(test.xgb, test$Type) 
error.e.xgb   <- sum(test.xgb != test$Type) / (nrow(df) - length(idx)) 

## SVM
tune.svm        <- tune.svm(Type ~ ., data = train,
                            gamma = seq(0.02, 0.16, 0.02),
                            cost  = seq(300, 600, 50))                            # train w a range of C & gamma
tuned.svm       <- svm(Type ~ ., data = train, cross = 5,
                       gamma = tune.svm$best.parameters[[1]],
                       cost  = tune.svm$best.parameters[[2]])                     # tuned svm model on training set
cfusion.r.svm.u <- table(tuned.svm$fitted, train$Type)                    # confusion matrix on training set
error.r.svm.u   <- sum(tuned.svm$fitted != train$Type) / length(idx) # error rate on training set
test.svm.u      <- predict(tuned.svm, newdata = test)                  # test
cfusion.e.svm.u <- table(test.svm.u, test$Type)                         # confusion matrix on test set
error.e.svm.u   <- sum(test.svm.u != test$Type) / (nrow(df) - length(idx))     # error rate on test set

## MDL
mdl_fit       <- train(x = select(train, -Type), y = train$Type, trControl = train_control, 
                       method = 'multinom')
test.mdl      <- predict(mdl_fit, newdata = test)
cfusion.e.mdl <- table(test.mdl, test$Type) 
error.e.mdl   <- sum(test.mdl != test$Type) / (nrow(df) - length(idx))


## bagging
bag_fit       <- train(x = select(train, -Type), y = train$Type, trControl = train_control, 
                       method = 'treebag')
test.bag <- predict(bag_fit, newdata = test)
cfusion.e.bag <- table(test.bag, test$Type) 
error.e.bag   <- sum(test.bag != test$Type) / (nrow(df) - length(idx))

## stacking
train_st <- data.frame(Type = train$Type,
                       tr_predict = predict(dt_fit, newdata = train),
                       rf_predict = predict(rf_fit2, newdata = train),
                       xgb_predict = predict(xgb_fit, newdata = train),
                       svm_predict = predict(tuned.svm, newdata = train),
                       mdl_predict = predict(mdl_fit, newdata = train),
                       bag_predict = predict(bag_fit, newdata = train))
tr_st <- train(Type~., method = 'rpart', data = train_st)
rf_st <- train(Type~., data = train_st, method = 'rf', ntree = 100, 
               trControl = fit_Control, tuneGrid = expand.grid(mtry = 5:15), verbose = F)
xgb_st <- train(Type ~ ., data = train_st, trControl = train_control, 
                #tuneGrid = grid_default, 
                method = 'xgbTree')
svm_st <- svm(Type ~ ., data = train_st, cross = 5,
              gamma = tune.svm$best.parameters[[1]],
              cost  = tune.svm$best.parameters[[2]]) 
mdl_st <- train(x = select(train_st, -Type), y = train_st$Type, trControl = train_control, 
                method = 'multinom')
bag_st <- train(x = select(train_st, -Type), y = train_st$Type, trControl = train_control, 
                method = 'treebag')
test_st     <- data.frame(Type = test$Type,
                          tr_predict = predict(dt_fit, newdata = test),
                          rf_predict = predict(rf_fit2, newdata = test),
                          xgb_predict = predict(xgb_fit, newdata = test),
                          svm_predict = predict(tuned.svm, newdata = test),
                          mdl_predict = predict(mdl_fit, newdata = test),
                          bag_predict = predict(bag_fit, newdata = test)) 

test.s.tr    <- predict(tr_st, newdata = test_st)
cfusion.s.tr <- table(test.s.tr, test$Type) 
error.s.tr   <- sum(test.s.tr != test$Type) / (nrow(df) - length(idx))

test.s.rf    <- predict(rf_st, newdata = test_st)
cfusion.s.rf <- table(test.s.rf, test$Type) 
error.s.rf   <- sum(test.s.rf != test$Type) / (nrow(df) - length(idx))

test.s.xgb    <- predict(xgb_st, newdata = test_st)
cfusion.s.xgb <- table(test.s.xgb, test$Type) 
error.s.xgb   <- sum(test.s.xgb != test$Type) / (nrow(df) - length(idx))

test.s.svm    <- predict(svm_st, newdata = test_st)
cfusion.s.svm <- table(test.s.svm, test$Type) 
error.s.svm   <- sum(test.s.svm != test$Type) / (nrow(df) - length(idx))

test.s.mdl    <- predict(mdl_st, newdata = test_st)
cfusion.s.mdl <- table(test.s.mdl, test$Type) 
error.s.mdl   <- sum(test.s.mdl != test$Type) / (nrow(df) - length(idx))

test.s.bag    <- predict(bag_st, newdata = test_st)
cfusion.s.bag <- table(test.s.bag, test$Type) 
error.s.bag   <- sum(test.s.bag != test$Type) / (nrow(df) - length(idx))



best.res      <- 1 - min(error.e.tr, error.e.rf, error.e.xgb, error.e.svm.u, error.e.mdl, error.e.bag,
                         error.s.tr, error.s.rf, error.s.xgb, error.s.svm, error.s.mdl, error.s.bag)
best.res
test.result   <- data.frame(Type = test$Type,
                            tr_predict = predict(dt_fit, newdata = test),
                            rf_predict = predict(rf_fit2, newdata = test),
                            xgb_predict = predict(xgb_fit, newdata = test),
                            svm_predict = predict(tuned.svm, newdata = test),
                            mdl_predict = predict(mdl_fit, newdata = test),
                            bag_predict = predict(bag_fit, newdata = test), 
                            tr.s_predict = predict(tr_st, newdata = test_st),
                            rf.s_predict = predict(rf_st, newdata = test_st),
                            xgb.s_predict = predict(xgb_st, newdata = test_st),
                            svm.s_predict = predict(svm_st, newdata = test_st),
                            mdl.s_predict = predict(mdl_st, newdata = test_st),
                            bag.s_predict = predict(bag_st, newdata = test_st))

Confusion_Matrix  <- cfusion.s.xgb

Acc_Table         <- t(sort(data.frame("Decision Tree" = 1 - error.e.tr,
                                "Random Forest" = 1 - error.e.rf,
                                "XGBoost"       = 1 - error.e.xgb,
                                "SVM"           = 1 - error.e.svm.u,
                                "MDL"           = 1 - error.e.mdl,
                                "Bagging"       = 1 - error.e.bag,
                                "Stacking_Tree" = 1 - error.s.tr,
                                "Stacking_RF"   = 1 - error.s.rf,
                                "Stacking_XGB"  = 1 - error.s.xgb,
                                "Stacking_SVM"  = 1 - error.s.svm,
                                "Stacking_MDL"  = 1 - error.s.mdl,
                                "Stacking_Bag"  = 1 - error.s.bag), decreasing = TRUE))

colnames(Acc_Table) <- "Accuracy" 


Doc_predict <- function(folder_list = folder_list){
  
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
  
  result <- list(Prediction = predict(xgb_st, test_st),
                 Actual     = test_st$Type)
  
  return(result)
}

