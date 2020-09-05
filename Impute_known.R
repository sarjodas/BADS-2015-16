#loading the basic library of HMISC

library(Hmisc)
#this function is used to impute the cleaned data set in order to use with various prediction models. 
#we are changing the class of the certain feature which are used in the prediction model

impute.data <- function(whole.data){
  whole.data$account_creation_date = as.Date(whole.data$account_creation_date, format = "%Y/%m/%d")
  whole.data$deliverydate_actual = as.Date(whole.data$deliverydate_actual, format = "%Y/%m/%d")
  whole.data$deliverydate_estimated = as.Date(whole.data$deliverydate_estimated, format = "%Y/%m/%d")
  whole.data$order_date = as.Date(whole.data$order_date, format = "%Y/%m/%d")
  whole.data$return_customer = factor(whole.data$return_customer)
  
  whole.data$form_of_address= as.character(whole.data$form_of_address)
  #replace all NA values in form of address column with other. 
  whole.data$form_of_address[is.na(whole.data$form_of_address)] <- "Other"
  
  whole.data$email_domain= as.character(whole.data$email_domain)
  whole.data$payment= as.character(whole.data$payment)
  #change class of advertising code from factor to character
  whole.data$advertising_code = as.character(whole.data$advertising_code)
  
  #replace all NA values in advertising code column with other.
  whole.data$advertising_code[whole.data$advertising_code==""] <- "Other"
  # 
  whole.data$advertising_code[whole.data$advertising_code=="AC" | whole.data$advertising_code=="AD" |
                                whole.data$advertising_code=="AS" | whole.data$advertising_code=="AY" |
                                whole.data$advertising_code=="BN" | whole.data$advertising_code=="BU" |
                                whole.data$advertising_code=="BW"] <- "Rare"
  # 
  #whole.data$advertising_code = as.numeric(factor(whole.data$advertising_code))
  # 
  # 
  whole.data$postcode_delivery= as.character(whole.data$postcode_delivery)
  # 
  #this is used in order to standardize all the factor variables in our data set so that they can be considered the same way by the prediction model. 
  feature.names=names(whole.data)
  for (f in feature.names) {
    if (class(whole.data[[f]])=="factor") {
      levels <- unique(c(whole.data[[f]]))
      whole.data[[f]] <- factor(whole.data[[f]],
                                labels=make.names(levels))
    }
  }
  
  return(whole.data)
  
}

