# G.Johnson
# C3T3 - 25-January-2022

# load libraries

library(readr)
library(caret)
library(corrplot)

ExistingProducts <- read_csv("existingproductattributes2017.csv")
NewProducts <- read_csv("newproductattributes2017.csv")

# Know the dataset
str(ExistingProducts)
summary(ExistingProducts)
attributes(ExistingProducts)
names(ExistingProducts)

# dummify the data to convert all "chr" classes
ExistingProductsDummy <- dummyVars(" ~ .", data = ExistingProducts)
readyData <- data.frame(predict(ExistingProductsDummy, newdata = ExistingProducts))

# dummify the NewProducts data
NewProductsDummy <- dummyVars(" ~ .", data = NewProducts)
readyData2 <- data.frame(predict(NewProductsDummy, newdata = NewProducts))

# Check datatypes of dataframe
str(readyData) # all datatypes are now num
str(readyData2)
summary(readyData) # remove features containing NA's
summary(readyData2)
readyData$BestSellersRank <- NULL
readyData2$BestSellersRank <- NULL

# Remove other irrelevant features
readyData$ProductNum <- NULL
readyData$ProfitMargin <- NULL
readyData2$ProductNum <- NULL
readyData2$ProfitMargin <- NULL

# Correlation
corrData <- cor(readyData)
corrData

corrData2 <- cor(readyData2)
corrData2

# Visualize the correlation matrix with a heat map
corrplot(corrData)
corrplot(corrData2)

# Remove features that aren't needed
readyData$ProductTypeAccessories <- NULL
readyData$ProductTypeDisplay <- NULL
readyData$ProductTypeExtendedWarranty <- NULL
readyData$ProductTypeGameConsole <- NULL
readyData$ProductTypePrinter <- NULL
readyData$ProductTypePrinterSupplies <- NULL
readyData$ProductTypeSoftware <- NULL
readyData$ProductTypeTablet <- NULL
readyData2$ProductTypeAccessories <- NULL
readyData2$ProductTypeDisplay <- NULL
readyData2$ProductTypeExtendedWarranty <- NULL
readyData2$ProductTypeGameConsole <- NULL
readyData2$ProductTypePrinter <- NULL
readyData2$ProductTypePrinterSupplies <- NULL
readyData2$ProductTypeSoftware <- NULL
readyData2$ProductTypeTablet <- NULL

# Develop Multiple Regression Models
set.seed(123)

trainSize <- round(nrow(readyData)*0.7)
testSize <- nrow(readyData)-trainSize

trainSize #see how many instances are in the set
testSize #see how many instances are in the set

#Create training and test sets
training_indices <- sample(seq_len(nrow(readyData)), size = trainSize)
trainSet <- readyData[training_indices,]
testSet <- readyData[-training_indices,]

#Create Linear Regression (parametric) models with volume as the dep. variable
LRmodel1 <- lm(Volume~ x5StarReviews, trainSet)
LRmodel2 <- lm(Volume~ x4StarReviews, trainSet)
LRmodel3 <- lm(Volume~ x3StarReviews, trainSet)
LRmodel4 <- lm(Volume~ x2StarReviews, trainSet)
LRmodel5 <- lm(Volume~ x1StarReviews, trainSet)
LRmodel6 <- lm(Volume~ PositiveServiceReview, trainSet)
LRmodel7 <- lm(Volume~ NegativeServiceReview, trainSet)
LRmodel8 <- lm(Volume~ Recommendproduct, trainSet)

summary(LRmodel1)
#Residual standard error: 8.839e-13 on 54 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 2.184e+32 on 1 and 54 DF,  p-value: < 2.2e-16

## essentially perfect fit: summary may be unreliable

summary(LRmodel2)
#Residual standard error: 680.6 on 54 degrees of freedom
#Multiple R-squared:  0.8534,	Adjusted R-squared:  0.8507 
#F-statistic: 314.4 on 1 and 54 DF,  p-value: < 2.2e-16

summary(LRmodel3)
#Residual standard error: 1054 on 54 degrees of freedom
#Multiple R-squared:  0.6485,	Adjusted R-squared:  0.642 
#F-statistic: 99.64 on 1 and 54 DF,  p-value: 7.294e-14

summary(LRmodel4)
#Residual standard error: 1572 on 54 degrees of freedom
#Multiple R-squared:  0.2183,	Adjusted R-squared:  0.2038 
#F-statistic: 15.08 on 1 and 54 DF,  p-value: 0.0002828

summary(LRmodel5)
#Residual standard error: 1727 on 54 degrees of freedom
#Multiple R-squared:  0.05613,	Adjusted R-squared:  0.03865 
#F-statistic: 3.211 on 1 and 54 DF,  p-value: 0.07873

summary(LRmodel6)
#Residual standard error: 1385 on 54 degrees of freedom
#Multiple R-squared:  0.3933,	Adjusted R-squared:  0.382 
#F-statistic:    35 on 1 and 54 DF,  p-value: 2.325e-07

summary(LRmodel7)
#Residual standard error: 1709 on 54 degrees of freedom
#Multiple R-squared:  0.07583,	Adjusted R-squared:  0.05872 
#F-statistic: 4.431 on 1 and 54 DF,  p-value: 0.03996

summary(LRmodel8)
#Residual standard error: 1746 on 54 degrees of freedom
#Multiple R-squared:  0.03572,	Adjusted R-squared:  0.01786 
#F-statistic:     2 on 1 and 54 DF,  p-value: 0.163

# Use 3 non-parametric algos; SVM, Random Forest, Gradient Boosting

#define an 80%/20% train/test split of the dataset
inTraining <- createDataPartition(readyData$Volume, p = .80, list = FALSE)
training <- readyData[inTraining,]
testing <- readyData[-intraining,]

#3 fold cross validation because the dataset is too small
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

#load svm/rf/gb libraries
library(LiblineaR)
library(kernlab)
library(e1071)
library(party)
library(randomForest)
library(Rborist)
library(extraTrees)
library(RRF)
library(quantregForest)
library(xgboost)
library(h2o)
h2o.init()
library(gbm)

#train Support Vector Machine (SVM) - L2 Regularized Support Vector Machine (dual) with Linear Kernel
system.time({
  svmFit1 <- train(Volume~., data = training, method = "svmLinear3", trControl = fitControl, tuneLength = 5)
}) #31 Warnings -In LiblineaR::LiblineaR(data = as.matrix(x), target = y,  ... :
#No value provided for svr_eps. Using default of 0.1

#train Support Vector Machine (SVM) - Support Vector Machines with Boundrange String Kernel
#system.time({
#  svmFit2 <- train(Volume~., data = training, method = "svmBoundrangeString", trControl = fitControl, tuneLength = 5)
#}) #Error: 'x' should be a character matrix with a single column for string kernel methods

#train Support Vector Machine (SVM) - Support Vector Machines with Exponential String Kernel
#system.time({
#  svmFit3 <- train(Volume~., data = training, method = "svmExpoString", trControl = fitControl, tuneLength = 5)
#}) #Error: 'x' should be a character matrix with a single column for string kernel methods

#train Support Vector Machine (SVM) - Support Vector Machines with Linear Kernel
system.time({
  svmFit4 <- train(Volume~., data = training, method = "svmLinear", trControl = fitControl, tuneLength = 5)
}) #Warning message: In .local(x, ...) : Variable(s) `' constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Linear Kernel
system.time({
  svmFit5 <- train(Volume~., data = training, method = "svmLinear2", trControl = fitControl, tuneLength = 5)
}) #Warning messages:
#1: In svm.default(x = as.matrix(x), y = y, kernel = "linear", cost = param$cost,  :
#  Variable(s) ‘ProductTypeLaptop’ and ‘ProductTypeNetbook’ constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Polynomial Kernel
system.time({
  svmFit6 <- train(Volume~., data = training, method = "svmPoly", trControl = fitControl, tuneLength = 5)
}) #Warning messages:
#1: In .local(x, ...) : Variable(s) `' constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Radial Basis Function Kernel
system.time({
  svmFit7 <- train(Volume~., data = training, method = "svmRadial", trControl = fitControl, tuneLength = 5)
}) #Warning messages:
#1: In .local(x, ...) : Variable(s) `' constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Radial Basis Function Kernel
system.time({
  svmFit8 <- train(Volume~., data = training, method = "svmRadialCost", trControl = fitControl, tuneLength = 5)
}) #Warning messages:
#1: In .local(x, ...) : Variable(s) `' constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Radial Basis Function Kernel
system.time({
  svmFit9 <- train(Volume~., data = training, method = "svmRadialSigma", trControl = fitControl, tuneLength = 5)
}) #Warning messages:
#1: In .local(x, ...) : Variable(s) `' constant. Cannot scale data.

#train Support Vector Machine (SVM) - Support Vector Machines with Spectrum String Kernel
#system.time({
#  svmFit10 <- train(Volume~., data = training, method = "svmSpectrumString", trControl = fitControl, tuneLength = 5)
#}) #Error: 'x' should be a character matrix with a single column for string kernel methods



######



#train Random Forest (Regression) - Conditional Inference Random Forest
system.time({
  rfFit1 <- train(Volume~., data = training, method = "cforest", trControl = fitControl, tuneLength = 5)
}) 

#train Random Forest (Regression) - Parallel Random Forest
system.time({
  rfFit2 <- train(Volume~., data = training, method = "parRF", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Quantile Random Forest
system.time({
  rfFit3 <- train(Volume~., data = training, method = "qrf", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Random Forest - ranger
system.time({
  rfFit4 <- train(Volume~., data = training, method = "ranger", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Random Forest - Rborist
system.time({
  rfFit5 <- train(Volume~., data = training, method = "Rborist", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Random Forest - rf
system.time({
  rfFit6 <- train(Volume~., data = training, method = "rf", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Random Forest by Randomization
system.time({
  rfFit7 <- train(Volume~., data = training, method = "extraTrees", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Random Forest Rule-Based Model
system.time({
  rfFit8 <- train(Volume~., data = training, method = "rfRules", trControl = fitControl, tuneLength = 5)
})

#1913 rules (length<=2) were extracted from the first 500 trees.
#3198 rules (length<=3) were extracted from the first 500 trees.
#4496 rules (length<=4) were extracted from the first 500 trees.
#[1] "6 paths are ignored."
#5444 rules (length<=5) were extracted from the first 500 trees.
#[1] "7 paths are ignored."
#6137 rules (length<=6) were extracted from the first 500 trees.
#[1] "8 paths are ignored."
#1788 rules (length<=2) were extracted from the first 500 trees.
#2861 rules (length<=3) were extracted from the first 500 trees.
#[1] "1 paths are ignored."
#4099 rules (length<=4) were extracted from the first 500 trees.
#[1] "10 paths are ignored."
#5362 rules (length<=5) were extracted from the first 500 trees.
#[1] "24 paths are ignored."
#6330 rules (length<=6) were extracted from the first 500 trees.
#[1] "100 paths are ignored."
#1792 rules (length<=2) were extracted from the first 500 trees.
#2845 rules (length<=3) were extracted from the first 500 trees.
#[1] "2 paths are ignored."
#4112 rules (length<=4) were extracted from the first 500 trees.
#[1] "23 paths are ignored."
#5173 rules (length<=5) were extracted from the first 500 trees.
#[1] "93 paths are ignored."
#6110 rules (length<=6) were extracted from the first 500 trees.
#[1] "181 paths are ignored."
#6637 rules (length<=5) were extracted from the first 500 trees.
#[1] "1 paths are ignored."

#train Random Forest (Regression) - Regularized Random Forest
system.time({
  rfFit9 <- train(Volume~., data = training, method = "RRF", trControl = fitControl, tuneLength = 5)
})

#train Random Forest (Regression) - Regularized Random Forest
system.time({
  rfFit10 <- train(Volume~., data = training, method = "RRFglobal", trControl = fitControl, tuneLength = 5)
})



####



#train Gradient Boosting (Regression) - eXtreme Gradient Boosting - xgbDART
system.time({
  gbFit1 <- train(Volume~., data = training, method = "xgbDART", trControl = fitControl, tuneLength = 5)
})

#train Gradient Boosting (Regression) - eXtreme Gradient Boosting - xgbLinear
system.time({
  gbFit2 <- train(Volume~., data = training, method = "xgbLinear", trControl = fitControl, tuneLength = 5)
})

#train Gradient Boosting (Regression) - eXtreme Gradient Boosting - xgbTree
system.time({
  gbFit3 <- train(Volume~., data = training, method = "xgbTree", trControl = fitControl, tuneLength = 5)
})

#train Gradient Boosting (Regression) - Gradient Boosting Machines - gbm_h2o
system.time({
  gbFit4 <- train(Volume~., data = training, method = "gbm_h2o", trControl = fitControl, tuneLength = 5)
})

#train Gradient Boosting (Regression) - Stochastic Gradient Boosting - gbm
system.time({
  gbFit5 <- train(Volume~., data = training, method = "gbm", trControl = fitControl, tuneLength = 5)
})

rf1Preds <- predict(rfFit1, readyData)
postResample(rf1Preds, readyData$Volume)
#RMSE     Rsquared          MAE 
#1155.1422463    0.4562222  259.7421317 

rf2Preds <- predict(rfFit2, readyData)
postResample(rf2Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#721.9208630   0.8588789 127.0879433 

rf3Preds <- predict(rfFit3, readyData)
postResample(rf3Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#466.2490751   0.9402386  56.8500000 

rf4Preds <- predict(rfFit4, readyData)
postResample(rf4Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#766.2352554   0.8474154 137.0202767 

rf5Preds <- predict(rfFit5, readyData)
postResample(rf5Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#710.3484669   0.8793112 115.9875613 

rf6Preds <- predict(rfFit6, readyData)
postResample(rf6Preds, readyData$volume)
#Error in table(obs, pred) : all arguments must have the same length

rf7Preds <- predict(rfFit7, readyData)
postResample(rf7Preds, readyData$Volume)
#RMSE   Rsquared        MAE 
#481.101544   0.938029  62.384167 

rf8Preds <- predict(rfFit8, readyData)
postResample(rf8Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#1490.674747    0.157945  518.500000 

rf9Preds <- predict(rfFit9, readyData)
postResample(rf9Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#725.6949488   0.8805287 114.0548067 

rf10Preds <- predict(rfFit10, readyData)
postResample(rf10Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#708.4984589   0.8870569 111.4142400 

svm1Preds <- predict(svmFit1, readyData)
postResample(svm1Preds, readyData$Volume)
#RMSE  Rsquared       MAE 
#2.4740855 0.9999993 1.0789045 

svm4Preds <- predict(svmFit4, readyData)
postResample(svm4Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE   Rsquared        MAE 
#77.6562273  0.9983401 58.2531808 

svm5Preds <- predict(svmFit5, readyData)
postResample(svm5Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE   Rsquared        MAE 
#77.6562273  0.9983401 58.2531808 

svm6Preds <- predict(svmFit6, readyData)
postResample(svm6Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE   Rsquared        MAE 
#79.2310418  0.9982634 58.2435271 

svm7Preds <- predict(svmFit7, readyData)
postResample(svm7Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE     Rsquared          MAE 
#1151.9196045    0.4529533  241.2161693 

svm8Preds <- predict(svmFit8, readyData)
postResample(svm8Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE     Rsquared          MAE 
#1239.3028913    0.3832064  271.2319552 

svm9Preds <- predict(svmFit9, readyData)
postResample(svm9Preds, readyData$Volume) # Warning: Cannot scale data
#RMSE    Rsquared         MAE 
#956.3930971   0.6983057 199.4517745 

gb1Preds <- predict(gbFit1, readyData)
postResample(gb1Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#636.9291755   0.8872643  93.6459681 

gb2Preds <- predict(gbFit2, readyData)
postResample(gb2Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#466.1523702   0.9404661  54.6226673 

gb3Preds <- predict(gbFit3, readyData)
postResample(gb3Preds, readyData$Volume)
#RMSE    Rsquared         MAE 
#471.4096992   0.9378098  62.0573590 

gb4Preds <- predict(gbFit4, readyData)
postResample(gb4Preds, readyData$Volume)
#RMSE     Rsquared          MAE 
#1118.5764949    0.5046637  275.2336107 

gb5Preds <- predict(gbFit5, readyData)
postResample(gb5Preds, readyData$Volume)
#RMSE     Rsquared          MAE 
#1204.2027589    0.4026253  308.7556001 


### Build prediction model using svmLinear2

finalPredrf <- predict(rfFit3, readyData2)
#postResample(finalPredrf, readyData2$Volume)  Can't predict against 0
#RMSE        Rsquared       MAE 
#1598.8269     NA         688.8333 
finalPredrf

finalPredsvm <- predict(svmFit5, readyData2)
#postResample(finalPredsvm, readyData2$Volume)  Can't predict against 0
#RMSE        Rsquared       MAE 
#1593.8819      NA         697.3725 

finalPredgb <- predict(gbFit2, readyData2)
#postResample(finalPredgb, readyData2$Volume)  Can't predict against 0
#RMSE       Rsquared      MAE 
#1597.532      NA        684.918 



# Add predictions to the new products data set

output <- NewProducts
output$predictionsrf <- finalPredrf
#output$predictionssvm <- finalPredsvm # contains negative values and not a good prediction
output$predictionsgb <- finalPredgb



write.csv(output, file="C3.T3output.csv", row.names = TRUE)

