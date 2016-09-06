### House Price prediction@Kaggle: https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# Problem: How do home features add up to its price tag?

# 0. clear the workspace
rm(list=ls())
# 1. Read in the data
training<-read.csv("data/train.csv", header = TRUE, sep = ",")
testing<-read.csv("data/test.csv", header = TRUE, sep = ",")
# 2. Summarize Data
# Checking the number of rows and cols
dim(training)
dim(testing)
## Quick peek at the structure of the dataset
str(training) ## The response variable is SalesPrice that needs to be predicted based on the predictors
str(testing)
# Check for missing data in training
sum(is.na(training)) # 6965 values are missing in training data
sum(is.na(testing)) # 7000 values are missing in testing data
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
str(nzv_cols1)
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
dropHigMisCols<- c("PoolQC","MiscFeature","Alley","Fence")
train<-train [, !names(train) %in% dropHigMisCols ]
test<- test[, !names(test) %in% dropHigMisCols]

# Missing value treatment for the remaining predictors in training and testing dataset
str(train)

# Show the missing values in train continuous predictors 
summary(train.cont)
sum(is.na(train.cont))

# Imputation with mean/median/mode 
# Missing value treatment by mean for continuous variables in the train data
train$LotFrontage[is.na(train$LotFrontage)] <-mean(train$LotFrontage, na.rm=T)
train$MasVnrArea[is.na(train$MasVnrArea)] <-mean(train$MasVnrArea, na.rm=T)
train$GarageYrBlt[is.na(train$GarageYrBlt)] <-mean(train$GarageYrBlt, na.rm = T)

# Missing value treatment by mode for categorical variables in the train data
train$MasVnrType[is.na(train$MasVnrType)] <-mode(train$MasVnrType)
train$BsmtQual[is.na(train$BsmtQual)] <-mode(train$BsmtQual)
train$BsmtExposure[is.na(train$BsmtExposure)] <-mode(train$BsmtExposure)
train$BsmtFinType1[is.na(train$BsmtFinType1)] <-mode(train$BsmtFinType1)
train$Electrical[is.na(train$Electrical)] <-mode(train$Electrical)
train$FireplaceQu[is.na(train$FireplaceQu)] <-mode(train$FireplaceQu)
train$GarageType[is.na(train$GarageType)] <-mode(train$GarageType)
train$GarageFinish[is.na(train$GarageFinish)] <-mode(train$GarageFinish)
sum(is.na(train))

# Missing value treatment by mean for continuous variables in the test data
test$LotFrontage[is.na(test$LotFrontage)] <-mean(test$LotFrontage, na.rm=T)
test$MasVnrArea[is.na(test$MasVnrArea)] <-mean(test$MasVnrArea, na.rm=T)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <-mean(test$BsmtFinSF1, na.rm=T)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <-mean(test$BsmtFinSF2, na.rm=T)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <-mean(test$BsmtUnfSF, na.rm=T)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <-mean(test$TotalBsmtSF, na.rm=T)
test$BsmtFullBath[is.na(test$BsmtFullBath)] <-mean(test$BsmtFullBath, na.rm=T)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <-mean(test$BsmtHalfBath, na.rm=T)
test$GarageYrBlt[is.na(test$GarageYrBlt)] <-mean(test$GarageYrBlt, na.rm=T)
test$GarageCars[is.na(test$GarageCars)] <-mean(test$GarageCars, na.rm=T)
test$GarageArea[is.na(test$GarageArea)] <-mean(test$GarageArea, na.rm=T)

# Missing value treatment by mode for categorical variables in the test data
test$MSZoning[is.na(test$MSZoning)]<-mode(test$MSZoning)
test$MSZoning[is.na(test$MSZoning)]<-mode(test$MSZoning)
#test$Utilities [is.na(test$Utilities )]<-mode(test$Utilities )
test$Exterior1st [is.na(test$Exterior1st )]<-mode(test$Exterior1st )
test$Exterior2nd [is.na(test$Exterior2nd )]<-mode(test$Exterior2nd )
test$MasVnrType [is.na(test$MasVnrType)]<-mode(test$MasVnrType )
#test$BsmtCond[is.na(test$BsmtCond)]<-mode(test$BsmtCond)
test$BsmtQual[is.na(test$BsmtQual)]<-mode(test$BsmtQual)
test$BsmtExposure[is.na(test$BsmtExposure)]<-mode(test$BsmtExposure)
test$BsmtFinType1[is.na(test$BsmtFinType1 )]<-mode(test$BsmtFinType1 )
#test$BsmtFinType2[is.na(test$BsmtFinType2)]<-mode(test$BsmtFinType2)
test$KitchenQual[is.na(test$KitchenQual)]<-mode(test$KitchenQual)
#test$Functional[is.na(test$Functional)]<-mode(test$Functional)
test$FireplaceQu[is.na(test$FireplaceQu)]<-mode(test$FireplaceQu)
test$GarageType[is.na(test$GarageType)]<-mode(test$GarageType)
test$GarageFinish[is.na(test$GarageFinish)]<-mode(test$GarageFinish)
test$GarageQual[is.na(test$GarageQual)]<-mode(test$GarageQual)
test$SaleType[is.na(test$SaleType)]<-mode(test$SaleType)
#test$GarageCond[is.na(test$GarageCond)]<-mode(test$GarageCond)

### Check for missing data after preprocessing in both train and test dataset
sum(is.na(train))
sum(is.na(test))

### Check for high correlated predictors 
# Now correlation works only for numeric data. Therefore, I will subset the numeric predictors from the train & test data and save it to a new data frame
train.cont<-train[-c(3,6:11,16:19,21:26,30:32,41,44:45,47,50,55:56)]
str(train.cont)
train.cat<-train[c(3,6:11,16:19,21:26,30:32,41,44:45,47,50,55:56)]
test.cont<-test[-c(3,6:12,17:20,22:27,32:34,43,46:47,49,52:53,58:59)]
str(test.cont)
test.cat<-test[c(3,6:12,17:20,22:27,32:34,43,46:47,49,52:53,58:59)]

# Checking for highly correlations in continuous variables of the train data
train.cont.cor<-cor(train.cont)
train.cont.higCor<- findCorrelation(train.cont.cor, cutoff = 0.80)
str(train.cont.higCor)
names(train[train.cont.higCor]) # There are three predictors with more than 80% correlation and these are "YearRemodAdd" "OverallCond"  "BsmtQual". They should be removed from the train data
train<-train[, -train.cont.higCor] # remove the high correlated predictors from the continuous predictors in the train data
train.cont<-train.cont[, -train.cont.higCor]

# similarly check for high correlated predictors in in continuous variables of the test data
test.cont.cor<-cor(test.cont)
test.cont.higCor<- findCorrelation(test.cont.cor, cutoff = 0.80)
str(test.cont.higCor)
names(test[test.cont.higCor]) # There are two predictors with more than 80% correlation and these are "BsmtQual"  "BsmtUnfSF". They should be removed from the test data
test<- test[, -test.cont.higCor] ## remove the high correlated predictors from the continuous predictors in the test data
test.cont<-test.cont[,-test.cont.higCor]