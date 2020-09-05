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

#set specific controlling parameters for neural networks
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 10, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the ouput training object
)
 
# Define a search grid of values to test 
nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,30,2))
#Train neural network nn with 10-fold cv
#train the model on 24 features under all the boundary conditions and control parameters.
nn <- train(return_customer~ newsletter + cost_shipping + remitted_items +
              delivery + item_count
            + account_creation_date
            + coupon
            + referrer
            + goods_value + other_count + order_date + deliverydate_estimated +
              deliverydate_actual + schoolbook_count + paperback_count +   audiobook_count +
              book_count + audiobook_download_count +  email_domain + imported_count + postcode_invoice +
              ebook_count + used_items + model, data = train,
            method = "nnet", maxit = 100, trace = FALSE, # options for nnet function
            tuneGrid = nn.parms, # parameters to be tested
            metric = "ROC", 
            trControl = model.control)
# Analyze the cross-validation results
yhat.nn   <- predict(nn, newdata = test, type = "prob")[,2]
# # Obtain ROC curve and calculate AUC 
nn.roc <-roc(test$return_customer, yhat.nn)
auc(nn.roc)
#Area under the curve: 0.6039
#plot ROC curve
plot.roc(nn.roc)

test$predicted.response = cut(yhat.nn, breaks = c(0, 0.23, 1), labels = c("X1", "X2"))
confusionMatrix(test$predicted.response, test$return_customer)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
#         0 2353  427
#         1 1858  550
# 
# Accuracy : 0.5596          
# 95% CI : (0.5459, 0.5731)
# No Information Rate : 0.8117          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0779          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.5588          
# Specificity : 0.5629          
# Pos Pred Value : 0.8464          
# Neg Pred Value : 0.2284          
# Prevalence : 0.8117          
# Detection Rate : 0.4535          
# Detection Prevalence : 0.5359          
# Balanced Accuracy : 0.5609          
# 
# 'Positive' Class : X1  



