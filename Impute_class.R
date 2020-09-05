#loading the basic library of HMISC

library(Hmisc)

#this function is used to impute the cleaned data set in order to use with various prediction models.
#we are changing the class of the certain feature which are used in the prediction model
impute.class <- function(class.data){
  class.data$account_creation_date = as.Date(class.data$account_creation_date, format = "%Y/%m/%d")
  class.data$deliverydate_actual = as.Date(class.data$deliverydate_actual, format = "%Y/%m/%d")
  class.data$deliverydate_estimated = as.Date(class.data$deliverydate_estimated, format = "%Y/%m/%d")
  class.data$order_date = as.Date(class.data$order_date, format = "%Y/%m/%d")
  
  class.data$form_of_address= as.character(class.data$form_of_address)
  class.data$email_domain= as.character(class.data$email_domain)
  class.data$payment= as.character(class.data$payment)
  class.data$advertising_code= as.numeric(class.data$advertising_code)
  class.data$postcode_delivery= as.character(class.data$postcode_delivery)
  class.data$postcode_invoice= as.numeric(class.data$postcode_invoice)
  
  return(class.data)
}

