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

# known$order_date <- as.Date.factor(known$order_date)
# known$delivery_date <- as.Date.factor(known$delivery_date)
# known$user_dob <- as.Date.factor(known$user_dob)
#Create a variable Age
known$age <- as.numeric(Sys.Date()-known$user_dob)
known$age <- known$age/365
known$age <- round(known$age)
hist(known$age)
summary(known$age)
table(known$age)
#While it seems that age could be a deciding factor, there are far too many outliers
#there appears to be many people over a 100 years old, that could  be explained by the default value of the 
#user_dob in the formulary or because of privacy issues

#Check for the difference between the order date and the delivery date (potentially a big factor for returning a product)
known$difference <- as.Date(known$delivery_date)-as.Date(known$order_date)
known$difference <- as.integer(known$difference)
known$difference[known$difference<0]<-NA
summary(known$difference)

#Missing values in the data
sapply(known, function(x) (sum(is.na(x))/nrow(known))*100)
#Around 10% of the customers did not input a delivery date for their orders
#Around 8 percent of the customers did not input their date of birth
#Moreover, 10% of the created variable Difference is missing, due to missing delivery dates or unreal data entries (delivery in 1994)
#Convert the variable item_id to a character type variable

#Analizing the variable item_size
str(known$item_size)
str(unique(known$item_size))
known$item_size <- as.factor(known$item_size)
table(known$item_size)
levels(known$item_size)
known$item_color <- as.factor(known$item_color)

known$item_id <- as.numeric(known$item_id)
known$user_state <- as.factor(known$user_state)
known$user_id <- as.factor(known$user_id)
known$user_title <- as.factor(known$user_title)
levels(known$user_title)
table(known$user_title)
#WOE
classCounts <- table(known$return, known$item_size)
classProb <- classCounts / rowSums(classCounts)
likelihood_ratio <- classProb[2,] / classProb[1,]
item_size_WOE <- log(likelihood_ratio)
str(item_size_WOE)

classCounts1 <- table(known$return, known$item_color)
classProb1 <- classCounts1 / rowSums(classCounts1)
likelihood_ratio1 <- classProb1[2,] / classProb1[1,]
item_color_WOE <- log(likelihood_ratio1)
str(item_color_WOE)



# Dummy Variable
known$same_day_order <- ifelse(known$order_date==known$user_reg_date,yes = 1,no=0)


#Some comments on the variable item_price: price 0 could refer to a discount or a giftcard, moreover, the max value
#is not common, it has the same item_id, so it is relevant for the analysis.
hist(known$item_price,breaks = 100,col = "blue",xlim = c(0,400))

known$log_price <- log(known$item_price)
hist(known$log_price,breaks = 100,col = "blue")
#By log-transforming the item_price variable we get a new variable that is more balanced and not skewed

# Drop some variables that could cause problems
drops <- c("order_date","delivery_date","item_price","user_dob","user_reg_date")

known[,drops]<-NULL
#### Train the models####
idx.train <- createDataPartition(y = known$return, p = 0.6, list = FALSE) # Draw a random, stratified sample including p percent of the data
train <- known[idx.train, ] # training set
test <-  known[-idx.train, ] # test set (drop all observations with train indeces)


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

unknown$same_day_order <- ifelse(unknown$order_date==unknown$user_reg_date,yes = 1,no=0)

unknown$age <- as.numeric(Sys.Date()-unknown$user_dob)
unknown$age <- unknown$age/365
unknown$age <- round(unknown$age)
unknown[,drops]<-NULL


table(unknown$item_size)
table(known$item_size)

#!!!!!Different levels on item_size, transform! 
#Check the same in the other variables!


#further comments on data transformations
#Convert the DOB variable to AGE
#Create a new variable "Number of purchased products on the same day 
#Dummy Variable of day of purchase and day of registration

#####building the model#####

#Logit regression
logit <- glm("return ~user_state + log_price + item_size", data = train, family = binomial(link="logit"))
summary(logit)
#As expected, preliminary results show that the price, the difference between the order date and the
#delivery date, and the item_size can be potentially decisive in the customer's decision to return a product
logit2 <- glm("return ~difference + item_color + item_price + user_state " , data = known, family = binomial(link="logit"))
summary(logit2)
#Further analysis shows that the item color is also an important factor for returning a product

#pred_unknown <- predict(logit, newdata = test, type = "response")

prediction <- data.frame("order_item_id" = unknown$order_item_id, "return" = pred_unknown)


#Decision trees

dt_train <- rpart(return ~ ., data = train, cp = 0, maxdepth=10)
dt_large_train <- rpart(return ~ ., data = train, method = "class", cp=0., minsplit=0, minbucket=0, maxsurrogate=0, xval=3)
pred_unknowndt <- predict(dt_large_train,newdata = unknown, type = "prob")


#####Gradient boosting with xgboost#####

library(mlr)
# Prepare the mlr task
# Xgboost doesn't take categorical variables as input
known_dummy <- mlr::createDummyFeatures(known, target="return")
train <- known_dummy[idx.train, ] # training set
test <-  known_dummy[-idx.train, ] # test set (drop all observations with train indeces)

task <- makeClassifTask(data = train, target = "return", positive = "1")

library("xgboost")
xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", # prediction type needs to be specified for the learner
                           par.vals = list("verbose" = 0,
                                           "early_stopping_rounds"=20)) # early stopping when no improvement for k iterations
xgb.learner

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.05), 
  makeIntegerParam("nrounds", lower=80, upper=400), 
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 1)
)

# How dense should the parameters be selected from the ranges?
tuneControl <- makeTuneControlRandom(maxit=100, tune.threshold = FALSE)

# We do 3-fold cross-validation, given the small data more folds might be better
rdesc <- makeResampleDesc(method = "RepCV", rep = 3, folds=2, stratify = TRUE)

#To make sure we are not spending too much time, let's parallelise it
library("parallelMap")
parallelStartSocket(10, level = "mlr.tuneParams")
set.seed(123) # Set seed for the local random number generator, e.g. the CV samples
# Set seed for the clusters. Select a random number generator for parallel computing
# and set the seed for the registered cluster
library("parallel")
RNGkind("L'Ecuyer-CMRG")  
clusterSetRNGStream(iseed = 1234567)
#careful, takes time
# Tune parameters as before
xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)

parallelStop()

# Extract optimal parameter values after tuning 
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

# Train the model on the full training data (not only a CV-fold)
model_library <- list()
model_library[["xgb"]] <- mlr::train(xgb.learner, task = task)

# Also train gradient boosting with a better set of hyperparameters found by extensive grid search
xgb.learner <- setHyperPars(xgb.learner, par.vals = list("eta"=0.03,"nrounds"=300, "max_depth"=4, "verbose" = 0))
model_library[["xgb_gridsearch"]] <- mlr::train(xgb.learner, task = task)

# Train randome forest (ideally with parameters from before)
rf.learner <- makeLearner("classif.randomForest", 
                          predict.type = "prob", # prediction type needs to be specified for the learner 
                          par.vals = list(
                            "mtry" = 2, "sampsize" = 250, "ntree" = 1000,
                            "replace" = TRUE, "importance" = FALSE))
model_library[["rf"]] <- mlr::train(rf.learner, task = task)

# Make prediction on test data
pred <- sapply(model_library, predict, newdata = test, simplify=FALSE)

# Calculate AUC performance on test set 
auc <- sapply(pred, mlr::performance, measures = mlr::auc)
# Compare the gradient boosting performance to last week's random forest
auc
# Gradient boosted trees are currently the most popular models in competitions, combining high performance and speed
# A worse performance could mean that our parameter tuning is not comprehensive enough or that the model
# just does't do well on this specific dataset

#### Heterogeneous Ensembles ####

# Collect the predictions of all models 
pred$xgb
pred_matrix <- sapply(pred, function(x) x$data$prob.1)
head(pred_matrix)
# Combine the predictions 
# This is where more complex combination algorithms exist
pred_ensemble <- rowMeans(pred_matrix[, c("xgb","rf")])
cbind(head(pred_matrix), head(pred_ensemble))
# Compare the performance of the combined predictions to individual predictions
auc
ModelMetrics::auc(actual=test$TARGET, predicted=pred_ensemble)

