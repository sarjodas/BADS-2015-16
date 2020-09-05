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

#set specific controlling parameters for XGB
ctrl  <- trainControl(method = "cv", number = 10, 
                      classProbs = TRUE,  savePredictions = "final", 
                      summaryFunction = twoClassSummary, allowParallel = TRUE, returnData = TRUE)

#initialise boundary conditions for neural networks, XGB and random forest
nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,30,2))
xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2,4),
                         eta = c(0.01, 0.05, 0.1, 0.15),
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)
rf.parms <- expand.grid(mtry = 1:10)

#create a model list which needs to be performed as an ensemble model
modelList <- list(caretModelSpec(method = "rf", ntree = 80, tuneGrid = rf.parms, metric = "ROC"),
                  caretModelSpec(method = "xgbTree", tuneGrid = xgb.parms, metric = "ROC"))

#train the model on 24 features under all the boundary conditions and control parameters
models <- caretList(return_customer ~ newsletter + delivery + model + 
                      audiobook_download_count + used_items + account_creation_date  + 
                      deliverydate_estimated + deliverydate_actual + cost_shipping + 
                      weight + coupon + paperback_count + ebook_count + other_count + 
                      imported_count + referrer + audiobook_count + advertising_code + 
                      points_redeemed + goods_value + order_date + remitted_items + item_count, 
                    data = train, trControl = ctrl, tuneList = modelList, 
                    continue_on_fail = FALSE)


#Using glm over the base classifier
ens.stack <- caretStack(models, method='glm')

#prediction and performance result over test dataset
#ens.stack.pred will contain values between 0 and 1.
ens.stack.pred <- predict(ens.stack, newdata = test, type = "prob")

auc(test$return_customer, ens.stack.pred)
#Area under the curve: 0.6456
 #For ROC plotting
r.stack <- roc(test$return_customer, ens.stack.pred)
plot(r.stack)

test$predicted.response = cut(ens.stack.pred, breaks = c(0, 0.23, 1), labels = c("X1", "X2"))
confusionMatrix(test$predicted.response, test$return_customer)
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
#           0 3456  645
#           1  755  332
# 
# Accuracy : 0.7301          
# 95% CI : (0.7178, 0.7422)
# No Information Rate : 0.8117          
# P-Value [Acc > NIR] : 1.000000        
# 
# Kappa : 0.1539          
# Mcnemar's Test P-Value : 0.003578        
#                                           
#             Sensitivity : 0.8207          
#             Specificity : 0.3398          
#          Pos Pred Value : 0.8427          
#          Neg Pred Value : 0.3054          
#              Prevalence : 0.8117          
#          Detection Rate : 0.6662          
#    Detection Prevalence : 0.7905          
#       Balanced Accuracy : 0.5803          
#                                           
#        'Positive' Class : X1              
#                               




stopCluster(cl)
