if (!require('earth')) install.packages("earth");
library("earth")
#Load all the libraries that are required to train and run the model
source("loadlibraries.R", local = TRUE)
load.library()
#Fetch the known dataset
source("Load_known.R", local = TRUE)
#load known data
whole.data <- data.frame(get.data.dataset())
marsModel <- earth(whole.data$return_customer ~., data=whole.data) # build model
ev <- evimp (marsModel) # estimate variable importance
plot(ev)
