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

#data_balanced_both uses ovun.sample of ROSE package to create a over and under sampled data by using a "Both" method of ROSE package
data_balanced_both <- ovun.sample(return_customer~ newsletter + cost_shipping + remitted_items +
                                    delivery + item_count
                                  + account_creation_date
                                  + coupon
                                  + referrer
                                  + goods_value + other_count + order_date + deliverydate_estimated +
                                    deliverydate_actual + schoolbook_count + paperback_count +   audiobook_count +
                                    book_count + audiobook_download_count +  email_domain + imported_count + postcode_invoice +
                                    ebook_count + used_items +model, 
                                    data = train, method = "both", p = 0.3,
                                    N=46696, seed = 123)$data
#Area under the curve: 0.6468

#data.rose uses ROSE function to create a over and under sampled synthetic dataset
data.rose <- ROSE(return_customer~ newsletter + cost_shipping + remitted_items +
                    delivery + item_count
                  + coupon
                  + referrer
                  + goods_value + other_count +
                     schoolbook_count + paperback_count +   audiobook_count +
                    book_count + audiobook_download_count +   imported_count + postcode_invoice +
                    ebook_count + used_items +model, 
                    data = train, p = 0.3, seed = 123)$data
#Area under the curve: 0.6309

#set specific controlling parameters for XGB
model.control <- trainControl(
  method = "cv", number = 10, classProbs = TRUE,
  summaryFunction = twoClassSummary, allowParallel = TRUE,
  returnData = TRUE
)


xgb.prams <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = 5,
                         eta = c(0.01, 0.05, 0.1, 0.15),
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)

#train the model on 24 features under all the boundary conditions and control parameters.
xgb <- train(return_customer~ newsletter + cost_shipping + remitted_items +
               delivery + item_count
             + account_creation_date
             + coupon
             + referrer
             + goods_value + other_count + order_date + deliverydate_estimated +
               deliverydate_actual + schoolbook_count + paperback_count +   audiobook_count +
               book_count + audiobook_download_count +  email_domain + imported_count + postcode_invoice +
               ebook_count + used_items +model, 
                     data = train, method = "xgbTree", tuneGrid = xgb.prams, 
                     metric = "ROC", trControl = model.control)


#prediction and performance result over test dataset
#xgb.test will contain values between 0 and 1.
xgb.test <- predict(xgb, newdata = test, type = "prob")[,2]
auc(test$return_customer, xgb.test)
#Area under the curve: 0.6511

#Calculate and plot ROC
rf.roc <- roc(test$return_customer, xgb.test)
plot.roc(rf.roc)

#Calculate the categorical results over the cut-off value
test$predicted.response = cut(xgb.test, breaks = c(0, 0.23, 1), labels = c("X1", "X2"))
confusionMatrix(test$predicted.response, test$return_customer)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0     1
#          0  3330  597
#          1   881  380
# 
# Accuracy : 0.7151          
# 95% CI : (0.7026, 0.7274)
# No Information Rate : 0.8117          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.1617          
# Mcnemar's Test P-Value : 1.823e-13       
# 
# Sensitivity : 0.7908          
# Specificity : 0.3889          
# Pos Pred Value : 0.8480          
# Neg Pred Value : 0.3013          
# Prevalence : 0.8117          
# Detection Rate : 0.6419          
# Detection Prevalence : 0.7569          
# Balanced Accuracy : 0.5899          
# 
# 'Positive' Class : X1 




#Fetch the known dataset
source("Load_class.R", local = TRUE)
#load known data
class.data <- data.frame(get.read.dataset())

#Imputation function call
source("Impute_class.R", local = TRUE)
#imputing some features
class.data <- impute.class(class.data)

#prediction
class.pred <- predict(xgb, newdata = class.data, type = "prob") [,2]

#Calculate the categorical results over the cut-off value
class.data$return_customer = cut(class.pred, breaks = c(0, 0.23, 1), labels = c(0, 1))
class.data$return_customer = as.integer(as.character(class.data$return_customer))

#Total Number of Predicted return customers: 3156
#Total Number of Predicted non-return customers: 9815

#exporting the results onto the new dataset file.
export <- class.data[, c("ID", "return_customer")]
View(export)
#Writing predictions to a file
write.table(export, file = "Prediction.csv", sep = ",", row.names = FALSE)

#Stop parallelism
stopCluster(cl)

