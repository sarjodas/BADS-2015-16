#loading the basic library of HMISC

library(Hmisc)

#creating a helper function which will be used to load the known data set

get.data.dataset <-function() {
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # load the package
  
  #loading the known data set in a vector named known 
  
  known<- read.csv("assignment_BADS_WS1617_known.csv", header = TRUE, sep = ",")
  
  #for cleaning account creation date on the basis of order date 
  known$account_creation_date <- as.character(known$account_creation_date) #change class from factor to charcter
  known$order_date <- as.character(known$order_date) #change class from factor to charcter
  
  #replacing NA's with order date values, the reason being we noticed that in most of the rows 
  #the account creation date is equal to the order date.
  known$account_creation_date[is.na(known$account_creation_date)] <-  (known$order_date[is.na(known$account_creation_date)]) 
  #no more NA's in account creation date after running the above line. 
  
  known$account_creation_date <- as.factor(known$account_creation_date) #convert back to factor
  known$order_date <- as.factor(known$order_date) #convert back to factor
  
  
  #for cleaning delivery date actualas it has a lot of 0000/00/00 values as well as outliers on the basis of order date
  known$order_date <- as.Date(known$order_date) #change class from factor to Date
  known$deliverydate_actual <- as.Date(known$deliverydate_actual) #change class from factor to Date
  
  #after running the above line when we run the summary(known$deliverydate_actual) we noticed that all the 0000/00/00 values changed to NA. 
  
  #if the audiobook_download_count is more than zero and deliverydate_actual value for the same row is NA replace NA with order date. 
  #this is done as the audiobook_download_count tells that the package is in the form of software which can be downloaded and 
  #hence the delivery date can be equal to oprder date.
  
  known$deliverydate_actual <- with(known, as.Date(ifelse((audiobook_download_count>0), 
                                                          order_date , deliverydate_actual), origin = "1970-01-01"))
  #same logic appplies to audiobook_count
  known$deliverydate_actual <- with(known, as.Date(ifelse((audiobook_count>0), 
                                                          order_date , deliverydate_actual), origin = "1970-01-01"))
  
  #same logic appplies to ebook_count
  known$deliverydate_actual <- with(known, as.Date(ifelse((ebook_count>0), 
                                                          order_date , deliverydate_actual), origin = "1970-01-01"))
  
  known$deliverydate_actual <- with(known, as.Date(ifelse(is.na(deliverydate_actual), order_date + days(4), deliverydate_actual), origin = "1970-01-01"))
  
  
  #to deal with outliers we subtracted the deliverdate_actual column with order date column and stored that result in a vectore named date_diff
  
  date_diff<-difftime(known$deliverydate_actual, known$order_date, units = "days")
  #change class to numeric in order to read the difference in a mathamatical form.  
  date_diff <- as.numeric(date_diff) #change class to numeric
  # Find the quartile values and the inter-quantile-range IQR
  #in the lower.quartile vector we are storing the result of the second column of the summary of date_diff vector. 
  #This is done because the second column of the summary always gives the lower quartile value. 
  lower.quartile <- as.numeric(summary(date_diff)[2])
  #in the upper.quartile vector we are storing the result of the fifth column of the summary of date_diff vector. 
  #This is done because the fifth column of the summary always gives the upper quartile value.
  upper.quartile <- as.numeric(summary(date_diff)[5])
  #here we are calculating the inter quartile range in vector IQR. The difference between upper quartile and lower quartile is known as inter quartile range. 
  IQR <- upper.quartile - lower.quartile
  # Calculate upper bound value as this is important in determining the outlier values which we have to replace. 
  upper.bound <- ceiling(upper.quartile + 1.5*IQR)
  #the values which are greater than the upper bound vector are converted to NA. 
  date_diff[date_diff>upper.bound] <-NA 
  #all NA values in date_diff vector shall be compared with deliverydate_actual column 
  #corresponding matched values in the deliverydate_actual column should be replaced by the order date + the upper bound value. 
  #for example if the order date is 10/10/2013 and upper bound for this value is 10 and diff_date value is NA than the deliverydate_actual value corresponding to this row will be 20/10/2013.
  known$deliverydate_actual <- with(known, as.Date(ifelse(is.na(date_diff), order_date + days(upper.bound), deliverydate_actual), origin = "1970-01-01")) #replace outliers with order date + 2
  
  #for cleaning delivery date estimated on the basis of order date based on a similar logic as used in deliverydate_actual. 
  known$deliverydate_estimated <- as.Date(known$deliverydate_estimated) #change class from factor to Date
  #running the above command showed that there are no NA values in deliverydate_estimated column. 
  
  ##to deal with outliers we subtracted the deliverdate_estimated column with order date column and stored that result in a vectore named clean_date_diff
  clean_date_diff<-difftime(known$deliverydate_estimated, known$order_date, units = "days")
  
  #change class to numeric in order to read the difference in a mathamatical form.
  clean_date_diff <- as.numeric(clean_date_diff) 
  
  # Find the quartile values and the inter-quantile-range IQR
  #in the lower.quartile vector we are storing the result of the second column of the summary of date_diff vector. 
  #This is done because the second column of the summary always gives the lower quartile value.
  lower.quartile.est <- as.numeric(summary(clean_date_diff)[2])
  
  #in the upper.quartile vector we are storing the result of the fifth column of the summary of date_diff vector. 
  #This is done because the fifth column of the summaralways gives the upper quartile value.
  upper.quartile.est <- as.numeric(summary(clean_date_diff)[5])
  
  #here we are calculating the inter quartile range in vector IQR. The difference between upper quartile and lower quartile is known as inter quartile range.
  IQR.est <- upper.quartile.est - lower.quartile.est
  
  #Calculate upper bound value as this is important in determining the outlier values which we have to replace.
  upper.bound.est <- ceiling(upper.quartile + 1.5*IQR.est)
  
  #the values which are greater than the upper bound vector are converted to NA.
  clean_date_diff[clean_date_diff>upper.bound.est] <-NA
  
  #the any value in clean_date_diff vector is less than zero we change them to NA.
  clean_date_diff[clean_date_diff<0] <-NA
  
  #all NA values in date_diff vector shall be compared with deliverydate_estimate column 
  #corresponding matched values in the deliverydate_estimate column should be replaced by the order date + the upper bound value. 
  #for example if the order date is 10/10/2013 and upper bound for this value is 5 and diff_date value is NA than the deliverydate_estimate value corresponding to this row will be 15/10/2013.
  known$deliverydate_estimated <- with(known, as.Date(ifelse(is.na(clean_date_diff), order_date + days(upper.bound.est), deliverydate_estimated), origin = "1970-01-01"))
  
  
  
  # Find the quartile values and the inter-quantile-range IQR
  #in the lower.quartile vector we are storing the result of the second column of the summary of weight. 
  #This is done because the second column of the summary always gives the lower quartile value.
  lower.quartile.weight <- as.numeric(summary(known$weight)[2])
  
  #in the upper.quartile vector we are storing the result of the fifth column of the summary of weight. 
  #This is done because the fifth column of the summaralways gives the upper quartile value.
  upper.quartile.weight <- as.numeric(summary(known$weight)[5])
  
  #here we are calculating the inter quartile range in vector IQR. The difference between upper quartile and lower quartile is known as inter quartile range.
  IQR.weight <- upper.quartile.weight - lower.quartile.weight
  #Calculate upper bound value as this is important in determining the outlier values which we have to replace.
  upper.bound.weight <- ceiling(upper.quartile + 1.5*IQR.weight)
  
  #replace the outliers with upper bound weight vector
  known$weight[known$weight>upper.bound.weight] <-upper.bound.weight
  
  #replace all NA in weight column with mean of weight column 
  known[is.na(known[,23]), 23] <- mean(known[,23], na.rm = TRUE)  
  
  #replace all NA in item_count column with mean of item_count column 
  known[is.na(known[,16]), 16] <- mean(known[,16], na.rm = TRUE)   #item_count
  

  return(known)
  
}

