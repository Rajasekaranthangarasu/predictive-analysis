### House Price prediction@Kaggle: https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# Problem: How do home features add up to its price tag?

# 0. clear the workspace
rm(list=ls())
# 1. Read in the data
training<-read.csv("data/train.csv", header = TRUE, sep = ",")
testing<-read.csv("data/test.csv", header = TRUE, sep = ",")
str(training)

colSums(is.na(training))
colSums(is.na(testing))
# Recoding variables with NA values that are actually data points as highlighted by
## reference: http://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns-data-frame-data-tab
### recoding the training data
table(training$Alley)
training[c("Alley")][is.na(training[c("Alley")])] <- "NoAlleyAccess"
table(training$Alley)

table(training$BsmtQual)
training[c("BsmtQual")][is.na(training[c("BsmtQual")])] <- "NoBasement"
table(training$BsmtQual)

table(training$BsmtCond)
training[c("BsmtCond")][is.na(training[c("BsmtCond")])] <- "NoBasement"
table(training$BsmtQual)

table(training$BsmtExposure)
training[c("BsmtExposure")][is.na(training[c("BsmtExposure")])] <- "NoBasement"
table(training$BsmtExposure)

table(training$BsmtFinType1)
training[c("BsmtFinType1")][is.na(training[c("BsmtFinType1")])] <- "NoBasement"
table(training$BsmtFinType1)

table(training$BsmtFinType2)
training[c("BsmtFinType2")][is.na(training[c("BsmtFinType2")])] <- "NoBasement"
table(training$BsmtFinType2)

table(training$GarageType)
training[c("GarageType")][is.na(training[c("GarageType")])] <- "NoGarage"
table(training$GarageType)

table(training$GarageFinish)
training[c("GarageFinish")][is.na(training[c("GarageFinish")])] <- "NoGarage"
table(training$GarageFinish)

table(training$GarageQual)
training[c("GarageQual")][is.na(training[c("GarageQual")])] <- "NoGarage"
table(training$GarageQual)

table(training$GarageCond)
training[c("GarageCond")][is.na(training[c("GarageCond")])] <- "NoGarage"
table(training$GarageCond)

table(training$PoolQC)
training[c("PoolQC")][is.na(training[c("PoolQC")])] <- "NoPool"
table(training$PoolQC)

table(training$Fence)
training[c("Fence")][is.na(training[c("Fence")])] <- "NoFence"
table(training$Fence)

table(training$MiscFeature)
training[c("MiscFeature")][is.na(training[c("MiscFeature")])] <- "None"
table(training$MiscFeature)

table(training$FireplaceQu)
training[c("FireplaceQu")][is.na(training[c("FireplaceQu")])] <- "NoFireplace"
table(training$FireplaceQu)

sum(is.na(training))
colSums(is.na(training)) # all categorical NA recoded accordingly as they were not missing according to the data dictionary

### recoding the testing data

table(testing$Alley)
testing[c("Alley")][is.na(testing[c("Alley")])] <- "NoAlleyAccess"
table(testing$Alley)

table(testing$BsmtQual)
testing[c("BsmtQual")][is.na(testing[c("BsmtQual")])] <- "NoBasement"
table(testing$BsmtQual)

table(testing$BsmtCond)
testing[c("BsmtCond")][is.na(testing[c("BsmtCond")])] <- "NoBasement"
table(testing$BsmtQual)

table(testing$BsmtExposure)
testing[c("BsmtExposure")][is.na(testing[c("BsmtExposure")])] <- "NoBasement"
table(testing$BsmtExposure)

table(testing$BsmtFinType1)
testing[c("BsmtFinType1")][is.na(testing[c("BsmtFinType1")])] <- "NoBasement"
table(testing$BsmtFinType1)

table(testing$BsmtFinType2)
testing[c("BsmtFinType2")][is.na(testing[c("BsmtFinType2")])] <- "NoBasement"
table(testing$BsmtFinType2)

table(testing$GarageType)
testing[c("GarageType")][is.na(testing[c("GarageType")])] <- "NoGarage"
table(testing$GarageType)

table(testing$GarageFinish)
testing[c("GarageFinish")][is.na(testing[c("GarageFinish")])] <- "NoGarage"
table(testing$GarageFinish)

table(testing$GarageCond)
testing[c("GarageCond")][is.na(testing[c("GarageCond")])] <- "NoGarage"
table(testing$GarageCond)

table(testing$GarageQual)
testing[c("GarageQual")][is.na(testing[c("GarageQual")])] <- "NoGarage"
table(testing$GarageQual)

table(testing$PoolQC)
testing[c("PoolQC")][is.na(testing[c("PoolQC")])] <- "NoPool"
table(testing$PoolQC)

table(testing$Fence)
testing[c("Fence")][is.na(testing[c("Fence")])] <- "NoFence"
table(training$Fence)

table(testing$MiscFeature)
testing[c("MiscFeature")][is.na(testing[c("MiscFeature")])] <- "None"
table(testing$MiscFeature)

table(testing$FireplaceQu)
testing[c("FireplaceQu")][is.na(testing[c("FireplaceQu")])] <- "NoFireplace"
table(testing$FireplaceQu)

colSums(is.na(testing))
sum(is.na(testing))

# 2. Summarize Data
# Checking the number of rows and cols
dim(training)
dim(testing)
## Quick peek at the structure of the dataset
str(training) ## The response variable is SalesPrice that needs to be predicted based on the predictors
str(testing)

# Check for missing data in training
sum(is.na(training)) # 358 values are missing in training data
sum(is.na(testing)) # 357 values are missing in testing data
## check for missing data column/attribute wise
colSums(is.na(training))
colSums(is.na(testing))



# Data Preprocessing steps
# Check for zero variance predictors
library(caret) # load the caret library for it has the nearZeroVar()
nzv_cols <- nearZeroVar(training)
nzv_cols
names(training[nzv_cols])

nzv_cols1 <- nearZeroVar(testing)
#str(nzv_cols1)
names(testing[nzv_cols1])
if(length(nzv_cols) > 0) 
  train <- training[, -nzv_cols] 

if(length(nzv_cols1) > 0) 
  test <- testing[, -nzv_cols1] 
## Missing data treatment: There are two types of missing data.a) MCAR (Missing Completetly At Random) & (b) MNAR (Missing Not At Random)
## Usually, MCAR is the desirable scenario in case of missing data. For this analysis I will assume that MCAR is at play. 
## Assuming data is MCAR, too much missing data can be a problem too. Usually a safe maximum threshold is 5% of the total for large datasets. If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out. We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function
# Check for missing data in training
sum(is.na(train))
sum(is.na(test))
colSums(is.na(train))
colSums(is.na(test))

## Visualizing the missing data pattern using the VIM package
library(VIM)
aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot <- aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
### So, the predictors such as PoolQC, MiscFeature, Alley and Fence have greater than 80% of missing data in both training & test data. It would be useful to drop these variables rather than imputing them. 
## Some great references
# http://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame
# http://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name

# Drop the predictors with high missing values identified above
#dropHigMisCols<- c("PoolQC","MiscFeature","Alley","Fence")
#train<-train [, !names(train) %in% dropHigMisCols ]
#test<- test[, !names(test) %in% dropHigMisCols]

# Missing value treatment for the remaining predictors in training and testing dataset
str(train)

# Missing value treatment using the mice library
library(mice)
tempData <- mice(train,m=5,maxit=50,meth='pmm',seed=500)
## Note on the parameters. m=5 refers to the number of imputed datasets. Five is the default value.
## meth='pmm' refers to the imputation method. In this case we are using predictive mean matching as imputation method. Other imputation methods can be used, type methods(mice) for a list of the available imputation methods.
summary(tempData)
train.complete<-complete(tempData,1) # where 1 referes to the first dataset out of five generated above
sum(is.na(train.complete)) # 9 values missing in the categorical predictors
colSums(is.na(train.complete))

# Missing value treatment for the test data
tempData <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
test.complete<-complete(tempData,1) # where 1 referes to the first dataset out of five generated above
sum(is.na(test.complete)) # 24 values missing in the categorical predictors
colSums(is.na(test.complete))
str(test.complete)
### Check for missing data after preprocessing in both train and test dataset
sum(is.na(train))
sum(is.na(test))

### Check for high correlated predictors 
# Now correlation works only for numeric data. Therefore, I will subset the numeric predictors from the train & test data and save it to a new data frame

train.complete.cont<-train.complete[,c(1:2,4:5,12:15,20,27:29,33:40,42:43,46,48:49,53:54,56:57,60)]
str(train.complete.cont)
train.complete.cat<-train.complete[,-c(1:2,4:5,12:15,20,27:29,33:40,42:43,46,48:49,53:54,56:57,60)]
str(train.complete.cat)

test.complete.cont<-test.complete[,c(2, 4:5,13:16,21,28:31,35:42,44:45,48,50:51,55:56,58:59)]
str(test.complete.cont)
test.complete.cat<-test.complete[,-c(2, 4:5,13:16,21,28:31,35:42,44:45,48,50:51,55:56,58:59)]
str(test.complete.cat)


# Checking for highly correlations in continuous variables of the train data
train.cont.cor<-cor(train.complete.cont)
train.cont.higCor<- findCorrelation(train.cont.cor, cutoff = 0.80)
str(train.cont.higCor)
names(train[train.cont.higCor]) # There are four predictors with more than 80% correlation and these are "YearRemodAdd", "OverallCond","BsmtQual","Foundation". They should be removed from the train data
train<-train[, -train.cont.higCor] # remove the high correlated predictors from the continuous predictors in the train data
#train.cont<-train.cont[, -train.cont.higCor]

# similarly check for high correlated predictors in in continuous variables of the test data
test.cont.cor<-cor(test.complete.cont)
test.cont.higCor<- findCorrelation(test.cont.cor, cutoff = 0.80)
#str(test.cont.higCor)
names(test[test.cont.higCor]) # There are two predictors with more than 80% correlation and these are "Foundation" "LotShape". They should be removed from the test data
test<- test[, -test.cont.higCor] ## remove the high correlated predictors from the continuous predictors in the test data
#test.cont<-test.cont[,-test.cont.higCor]

dim(train)
dim(test)
