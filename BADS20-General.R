####Business Analytics and Data Science Assignment####
#Read the data
known <- read.csv("BADS_WS1920_known.csv",header = TRUE)
library("ggplot2")
####Data preparation####
#Dimensions of the dataset
dim(known)
#Data structure
str(known)
#Summary statistics 
summary(known)
#Some comments on the data:
#Convert all the dates into Date variables (%Y-%M-%D)
str(known$order_date)
#Check fo the difference between the order date and the delivery date (potentially a big factor for returning a product)

#Missing values in delivery dates
#What to do with item_id?
#Transform the variable item_size into a meaningful variable, possibly create more sub-variables differentiating
#between size, footsize and others factors
#Check the relevance of the variable item_color
#Some comments on the variable item_price: price 0 could refer to a discount or a giftcard, moreover, the max value
#is not common, it has the same item_id, so it is relevant for the analysis.
#Check the relevance of the variable user_id, also check for the minimal values(!) 

#Plot user_state against the return variable
