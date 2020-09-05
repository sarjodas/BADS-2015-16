#set current directory for all the function call
setwd("C:/Users/ipart/Desktop/TU Berlin/Business Analytics and Data Science/Assignment_BADS_WS1617/Final")

#Load all the libraries that are required to train and run the model
source("loadlibraries.R", local = TRUE)
load.library()
setwd("C:/Users/ipart/Desktop/TU Berlin/Business Analytics and Data Science/Assignment_BADS_WS1617/Final")

#parllellism
nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


#Fetch the known dataset
source("Load_known.R", local = TRUE)
#load known data
whole.data <- data.frame(get.data.dataset())

#Imputation function call
source("Impute_known.R", local = TRUE)
#imputing some features
whole.data <- impute.data(whole.data)

#Patritioning the whole dataset for cross validation
set.seed(123)
idx.train <- createDataPartition(y = whole.data$return_customer, p = 0.90, list = FALSE)
train <- whole.data[idx.train, ]
test <- whole.data[-idx.train, ]


set.seed(123)
model.control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 10, # number of folds in cross validation
  classProbs = TRUE, # Return class probabilities
  summaryFunction = twoClassSummary, # twoClassSummary returns AUC
  allowParallel = TRUE # Enable parallelization if available
)



rf.parms <- expand.grid(mtry = 1:10)
#Train random forest with 10-fold cv
#train the model on 24 features under all the boundary conditions and control parameters.
rf.caret <- train(return_customer ~ newsletter + delivery + model + 
                    audiobook_download_count + used_items + account_creation_date  + 
                    deliverydate_estimated + deliverydate_actual + cost_shipping + 
                    weight + coupon + paperback_count + ebook_count + other_count + 
                    imported_count + referrer + audiobook_count + advertising_code + 
                    points_redeemed + goods_value + order_date + remitted_items + item_count, 
                  data = train,  
                  method = "rf", ntree = 50, tuneGrid = rf.parms, 
                  metric = "ROC", trControl = model.control)

#prediction and performance result over test dataset
#test.pred will contain values between 0 and 1.
#cross validate the model
test.pred <- predict(rf.caret, newdata = test, type = "prob")[,2]
#Calculate and plot ROC
rf.roc <- roc(test$return_customer, test.pred)
plot.roc(rf.roc)

auc(test$return_customer, as.numeric(test.pred))
#Area under the curve: 0.6071

#Calculate the categorical results over the cut-off value
test$predicted.response = cut(test.pred, breaks = c(0, 0.23, 1), labels = c("X1", "X2"))
confusionMatrix(test$predicted.response, test$return_customer)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
#           0 3267  711
#           1  413  195
# 
# Accuracy : 0.7549          
# 95% CI : (0.7422, 0.7673)
# No Information Rate : 0.8024          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.1176          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.8878          
# Specificity : 0.2152          
# Pos Pred Value : 0.8213          
# Neg Pred Value : 0.3207          
# Prevalence : 0.8024          
# Detection Rate : 0.7124          
# Detection Prevalence : 0.8674          
# Balanced Accuracy : 0.5515          
# 
# 'Positive' Class : X1  


#Stop parallelism
stopCluster(cl)
