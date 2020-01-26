####Business Analytics and Data Science Assignment####
#Name: Daniel Bustillo Mac Lean
#Matriculation number: 609728
#Email: bustilld@hu-berlin.de
#Read the data
library(readr)
known <- read_csv("BADS_WS1920_known.csv")
unknown <- read_csv("BADS_WS1920_unknown.csv")
library("ggplot2")
library("caret")
library("rpart")

#Sampling the data to increase the speed of calculations
#known <- known[sample(nrow(known), 30000, replace = FALSE),]
#unknown <- unknown[sample(nrow(unknown), 30000, replace = FALSE),]
####Data preparation####
#Tweaks
par(mar=c(1,1,1,1))
options("scipen"=100, "digits"= 4)
#Dimensions of the dataset
dim(known)
#Data structure
str(known)
#Summary statistics 
summary(known)
#Some comments on the data:
#Transform the return variable into a binary variable
class(known$return)
known$return <- factor(known$return,levels = c(0,1),labels= c("item kept", "item returned"))
#Convert all the dates into Date variables (%Y-%M-%D)
known$order_date <- as.Date.factor(known$order_date)
known$delivery_date <- as.Date.factor(known$delivery_date)
known$user_dob <- as.Date.factor(known$user_dob)


#Check for the difference between the order date and the delivery date (potentially a big factor for returning a product)
known$difference <- as.Date(known$delivery_date)-as.Date(known$order_date)
known$difference <- as.integer(known$difference)
known$difference[known$difference<0]<-NA
boxplot(known$difference)
summary(known$difference)

#Missing values in the data
sapply(known, function(x) (sum(is.na(x))/nrow(known))*100)
#Around 10% of the customers did not input a delivery date for their orders
#Around 8 percent of the customers did not input their date of birth
#Moreover, 10% of the created variable Difference is missing, due to missing delivery dates or unreal data entries (delivery in 1994)
#Convert the variable item_id to a character type variable
known$item_id <- as.character(known$item_id)
#(Transform the variable item_size into a meaningful variable, possibly create more sub-variables differentiating
#between size, footsize and others factors)
str(known$item_size)
table(known$item_size)
known$item_size <- as.factor(known$item_size)
summary(known$item_price)
#Check the relevance of the variable item_color
table(known$item_color)
#Some comments on the variable item_price: price 0 could refer to a discount or a giftcard, moreover, the max value
#is not common, it has the same item_id, so it is relevant for the analysis.
hist(known$item_price,breaks = 100,col = "blue",xlim = c(0,400))

known$log_price <- log(known$item_price)
hist(known$log_price,breaks = 100,col = "blue")
#By log-transforming the item_price variable we get a new variable that is more balanced and not skewed
#Check the relevance of the variable user_id
#Some 
known$user_id <- as.character(known$user_id)


#### Train the models####
idx.train <- createDataPartition(y = known$return, p = 0.6, list = FALSE) # Draw a random, stratified sample including p percent of the data
train <- known[idx.train, ] # training set
test <-  known[-idx.train, ] # test set (drop all observations with train indeces)

lr_train <- glm(return~.,   data = train, family = binomial(link = "logit"))
dt_train <- rpart(return ~ ., data = train, cp = 0, maxdepth=10)
dt_large_train <- rpart(return ~ ., data = train, method = "class", cp=0., minsplit=0, minbucket=0, maxsurrogate=0, xval=3)



#Weight of Evidence
library(klaR)
classCounts <- table(known$return, known$item_size)
classProb <- classCounts / rowSums(classCounts)
likelihood_ratio <- classProb[2,] / classProb[1,]
ORG_TYPE_WOE <- log(likelihood_ratio)









####Data cleaning on the unknown dataset####
unknown$order_date <- as.Date.factor(unknown$order_date)
unknown$delivery_date <- as.Date.factor(unknown$delivery_date)
unknown$user_dob <- as.Date.factor(unknown$user_dob)

unknown$difference <- as.Date(unknown$delivery_date)-as.Date(unknown$order_date)
unknown$difference <- as.integer(unknown$difference)
unknown$difference[unknown$difference<0]<-NA
summary(unknown$difference)

unknown$item_id <- as.character(unknown$item_id)
unknown$item_size <- as.factor(unknown$item_size)

unknown$log_price <- log(unknown$item_price)

#!!!!!Different levels on item_size, transform! 
#Check the same in the other variables!


#further comments on data transformations
#Convert the DOB variable to AGE
#Create a new variable "Number of purchased products on the same day 
#Dummy Variable of day of purchase and day of registration
#####building the model#####

#Logit regression
logit <- glm("return ~difference + user_state + item_price + item_size", data = known, family = binomial(link="logit"))
summary(logit)
#As expected, preliminary results show that the price, the difference between the order date and the
#delivery date, and the item_size can be potentially decisive in the customer's decision to return a product
logit2 <- glm("return ~difference + item_color + item_price + user_state " , data = known, family = binomial(link="logit"))
summary(logit2)
#Further analysis shows that the item color is also an important factor for returning a product

pred_unknown <- predict(logit, newdata = unknown, type = "response")

prediction <- data.frame("order_item_id" = unknown$order_item_id, "return" = pred_unknown)


#Decision trees



