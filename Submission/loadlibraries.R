load.library <- function(){
  #library
  if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc") # load the package
  if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") # load the package
  if(!require("e1071")) install.packages("e1071"); library("e1071") # load the package
  if(!require("randomForest")) install.packages("randomForest"); library("randomForest") # load the package
  if(!require("caret")) install.packages("caret"); library("caret") # load the package
  if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
  if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
  if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package
  if(!require("xgboost")) install.packages("xgboost"); library("xgboost") # load the package
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # load the package
  if(!require("ROSE")) install.packages("ROSE"); library("ROSE") # load the package
  if(!require("caretEnsemble")) install.packages("caretEnsemble"); library("caretEnsemble") # load the package
}