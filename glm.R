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


#Use glm over 24 most relevant features over the method: Binomial 
lr <- glm(return_customer ~ newsletter + delivery + model + 
            audiobook_download_count + used_items + account_creation_date  + 
            deliverydate_estimated + deliverydate_actual + cost_shipping + 
            weight + coupon + paperback_count + ebook_count + other_count + 
            imported_count + referrer + audiobook_count + advertising_code + 
            points_redeemed + goods_value + order_date + remitted_items + item_count, 
          data = train, family = binomial(link="logit"))


#prediction and performance result over test dataset
pred.lr <- predict(lr, newdata = test, type = "response")
auc(test$return_customer, pred.lr)
#Area under the curve: 0.6196

test$predicted.response = cut(pred.lr, breaks = c(0, 0.23, 1), labels = c("X1", "X2"))
confusionMatrix(test$predicted.response, test$return_customer)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
#         0 3386  646
#         1  825  331
# 
# Accuracy : 0.7165         
# 95% CI : (0.704, 0.7287)
# No Information Rate : 0.8117         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.1335         
# Mcnemar's Test P-Value : 3.467e-06      
#                                          
#             Sensitivity : 0.8041         
#             Specificity : 0.3388         
#          Pos Pred Value : 0.8398         
#          Neg Pred Value : 0.2863         
#              Prevalence : 0.8117         
#          Detection Rate : 0.6527         
#    Detection Prevalence : 0.7772         
#       Balanced Accuracy : 0.5714         
#                                          
#        'Positive' Class : X1 



summary(pred.lr)
class.lr <- ifelse(pred.lr > 0.5, 0, 1)
summary(factor(class.lr))
sum(class.lr == final$return_customer)
accuracy.lr <- sum(class.lr == final$return_customer) / length(class.lr)
summary(accuracy.lr)
