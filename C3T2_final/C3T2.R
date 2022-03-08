G.JOHNSON
C3T2

#Load libraries
library(readr) # read CSV
library(plyr) # used to change values
library(caret) # predictions
library(dplyr) # needed for explore
library(corrplot) # needed to plot correlations
library(explore) # runs EDA
library(gbm) # Required GBM model for Classification
library(C50) # C5.0 model for Classification
library(pls) # Partial Least Squares model for Classification
library(randomForest) # Random Forest
#library(forcats)

CompleteResponses<- read_csv("CompleteResponses.csv") # load datafrome
SurveyIncomplete<- read.csv("SurveyIncomplete.csv")

##What is in the Dataframe
attributes(CompleteResponses) # List attributes in the DF
summary(CompleteResponses) # min, max, mean, median, and quartiles of attributes
summary(SurveyIncomplete) # min, max, mean, median, and quartiles of attributes
str(CompleteResponses) # Displays the structure of data set
names(CompleteResponses) # Names attributes in data set

##Print out the instances within a particular column in the data set
CompleteResponses$salary
CompleteResponses$age
CompleteResponses$elevel
CompleteResponses$car
CompleteResponses$zipcode
CompleteResponses$credit
CompleteResponses$brand
summary(SurveyIncomplete$brand)


#Correlation
#corData<- cor(CompleteResponses)
#corData
#corrplot(corData)

##Change datatypes
#CompleteResponses$salary<-as.numeric(CompleteResponses$salary)
#CompleteResponses$age<-as.integer(CompleteResponses$age)
#CompleteResponses$elevel<-as.ordered(CompleteResponses$elevel)
#CompleteResponses$car<-as.factor(CompleteResponses$car)
#CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
#CompleteResponses$credit<-as.numeric(CompleteResponses$credit)
CompleteResponses$brand<- as.factor(CompleteResponses$brand)
SurveyIncomplete$brand<- as.factor(SurveyIncomplete$brand)
#SurveyIncomplete$salary<-as.numeric(SurveyIncomplete$salary)
#SurveyIncomplete$age<-as.integer(SurveyIncomplete$age)
#SurveyIncomplete$elevel<-as.ordered(SurveyIncomplete$elevel)
#SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
#SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
#SurveyIncomplete$credit<-as.numeric(SurveyIncomplete$credit)

#Check again for missing values or NA
summary(CompleteResponses)
is.na(CompleteResponses)
is.na(SurveyIncomplete$brand)

attributes(SurveyIncomplete) # List attributes in the DF
summary(SurveyIncomplete) # min, max, mean, median, and quartiles of attributes
str(SurveyIncomplete) # Displays the structure of data set
names(SurveyIncomplete)

#Plotting
hist(CompleteResponses$salary)
hist(CompleteResponses$age)
hist(CompleteResponses$credit)


#explore(CompleteResponses) # EDA




#caret model - Automatic Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = CompleteResponses
#Y Value = brand

#load library and set seed
#library(caret)
set.seed(998)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

# create test set with SurveyIncomplete
#inTraining2 <- createDataPartition(SurveyIncomplete$brand, p = .75, list = FALSE)
#training2 <- SurveyIncomplete[inTraining2,]
#testing2 <- SurveyIncomplete[inTraining2,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
##10 fold cross validation (SurveyIncomplete)
#fitControl2 <- trainControl(method = "reapeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry for Random Forest
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))
rfGrid2 <- expand.grid(mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

#train Random Forest manual tune model
system.time({
rfFit1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid)
})

system.time({
  rfFit2 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid2)
})

#train Stochastic Gradient Boosting model with a tuneLength = 5 
system.time({
gbmFit1 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 5)
})

system.time({
  gbmFit2 <- train(brand~., data = training, method = "gbm", trControl=fitControl, tuneLength = 10)
})

#train C5.0 - type Classification model with a tuneLength = 5 
system.time({
c50Fit1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 5)
})

system.time({
  c50Fit2 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 10)
})

#train Partial Least Squares model with a tuneLength = 5 
system.time({
  plsFit1 <- train(brand~., data = training, method = "pls", trControl=fitControl, tuneLength = 5)
})

system.time({
  plsFit2 <- train(brand~., data = training, method = "pls", trControl=fitControl, tuneLength = 10)
})
#training results Random Forest
rfFit1
#mtry  Accuracy   Kappa      
#1     0.6217673  0.000000000
#2     0.6224409  0.002387732
#3     0.7333010  0.364650410
#4     0.8370171  0.644023831
#5     0.8820043  0.748660675
rfFit2

## Models Random Forest
#rfFit1$finalModel$tuneValue
#rfFit1$finalModel$ntree
summary(rfFit1)
summary(rfFit2)
plot(rfFit1)
plot(rfFit2)

#Checking variable importance for RF 
varImp(object=rfFit1)
#Overall
#salary   100.0000
#age       42.6088
#credit    21.3013
#elevel.L   1.5952
#elevel.C   1.5519
#elevel^4   1.4897
#elevel.Q   1.0576
#zipcode1   0.5513
#zipcode4   0.5480
#zipcode5   0.4919
#zipcode7   0.4835
#zipcode3   0.4813
#zipcode6   0.4129
#zipcode2   0.4109
#zipcode8   0.2749
#car15      0.2422
#car7       0.2354
#car2       0.2001
#car12      0.1977
#car17      0.1940

#Plotting Varianle importance for Random Forest 
plot(varImp(object=rfFit1),main="RF - Variable Importance")

#training results GBM
gbmFit1
#Resampling results across tuning parameters:
#  interaction.depth  n.trees  Accuracy   Kappa    
#1                   50      0.7335697  0.4380106
#1                  100      0.7323573  0.4347585
#1                  150      0.7334319  0.4357171
#1                  200      0.7326245  0.4331208
#1                  250      0.7335681  0.4347680
#2                   50      0.8259855  0.6358195
#2                  100      0.8852422  0.7608264
#2                  150      0.9098914  0.8101426
#2                  200      0.9158184  0.8222405
#2                  250      0.9213385  0.8335891
#3                   50      0.8803918  0.7531545
#3                  100      0.9003261  0.7918427
#3                  150      0.9189152  0.8291238
#3                  200      0.9216075  0.8344763
#3                  250      0.9238954  0.8391614
#4                   50      0.8948103  0.7815234
#4                  100      0.9218764  0.8349556
#4                  150      0.9226843  0.8367792
#4                  200      0.9234916  0.8383722
#4                  250      0.9234907  0.8383238
#5                   50      0.9209346  0.8332500
#5                  100      0.9222792  0.8356719
#5                  150      0.9237595  0.8387056
#5                  200      0.9240294  0.8393923
#5                  250      0.9225484  0.8361224

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was held constant at a
#value of 10
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 200, interaction.depth = 5, shrinkage = 0.1 and n.minobsinnode = 10
gbmFit2
#interaction.depth  n.trees  Accuracy   Kappa    
#1                  50      0.7331695  0.4366935
#1                 100      0.7324960  0.4335955
#1                 150      0.7323618  0.4322159
#1                 200      0.7328991  0.4327702
#1                 250      0.7334374  0.4336017
#1                 300      0.7343804  0.4349647
#1                 350      0.7359966  0.4382353
#1                 400      0.7349194  0.4359343
#1                 450      0.7346496  0.4352357
#1                 500      0.7346496  0.4349644
#2                  50      0.8192433  0.6222820
#2                 100      0.8828155  0.7563346
#2                 150      0.9129848  0.8165088
#2                 200      0.9209317  0.8326124
#2                 250      0.9236253  0.8382583
#2                 300      0.9253761  0.8419248
#2                 350      0.9251074  0.8413242
#2                 400      0.9264542  0.8442183
#2                 450      0.9273969  0.8460416
#2                 500      0.9264541  0.8440646
#3                  50      0.8733902  0.7387499
#3                 100      0.9106969  0.8123856
#3                 150      0.9232243  0.8379313
#3                 200      0.9247049  0.8409015
#3                 250      0.9259163  0.8433038
#3                 300      0.9259172  0.8433777
#3                 350      0.9273976  0.8464015
#3                 400      0.9247040  0.8407577
#3                 450      0.9236266  0.8383136
#3                 500      0.9234909  0.8381548
#4                  50      0.8973615  0.7861847
#4                 100      0.9237648  0.8393668
#4                 150      0.9244358  0.8403836
#4                 200      0.9248394  0.8412791
#4                 250      0.9251098  0.8416284
#4                 300      0.9243003  0.8398641
#4                 350      0.9230879  0.8371434
#4                 400      0.9233585  0.8378334
#4                 450      0.9214724  0.8338513
#4                 500      0.9201265  0.8308114
#5                  50      0.9251102  0.8419990
#5                 100      0.9255125  0.8423848
#5                 150      0.9260517  0.8434322
#5                 200      0.9253783  0.8419255
#5                 250      0.9230901  0.8370205
#5                 300      0.9226841  0.8361470
#5                 350      0.9209339  0.8324492
#5                 400      0.9207993  0.8320169
#5                 450      0.9194525  0.8291540
#5                 500      0.9191819  0.8285517
#6                  50      0.9264546  0.8444493
#6                 100      0.9251084  0.8415438
#6                 150      0.9259177  0.8431927
#6                 200      0.9240315  0.8391020
#6                 250      0.9238972  0.8388479
#6                 300      0.9220108  0.8348738
#6                 350      0.9213369  0.8333680
#6                 400      0.9199891  0.8303104
#6                 450      0.9209323  0.8324069
#6                 500      0.9198561  0.8301283
#7                  50      0.9261854  0.8438821
#7                 100      0.9229513  0.8368144
#7                 150      0.9233571  0.8375152
#7                 200      0.9228178  0.8364285
#7                 250      0.9213373  0.8333018
#7                 300      0.9212031  0.8328057
#7                 350      0.9194505  0.8291998
#7                 400      0.9198554  0.8299196
#7                 450      0.9193166  0.8289297
#7                 500      0.9189129  0.8280502
#8                  50      0.9255108  0.8424541
#8                 100      0.9247037  0.8406133
#8                 150      0.9238972  0.8387521
#8                 200      0.9222807  0.8351676
#8                 250      0.9210679  0.8326592
#8                 300      0.9202610  0.8309585
#8                 350      0.9198566  0.8300293
#8                 400      0.9193179  0.8288288
#8                 450      0.9159508  0.8215807
#8                 500      0.9183763  0.8269647
#9                  50      0.9240304  0.8391207
#9                 100      0.9229538  0.8365361
#9                 150      0.9221443  0.8348421
#9                 200      0.9210672  0.8327242
#9                 250      0.9189127  0.8281238
#9                 300      0.9183742  0.8269386
#9                 350      0.9166227  0.8231055
#9                 400      0.9158153  0.8214968
#9                 450      0.9156806  0.8211809
#9                 500      0.9141990  0.8180829
#10                  50      0.9240302  0.8391480
#10                 100      0.9214704  0.8337137
#10                 150      0.9222798  0.8354833
#10                 200      0.9216078  0.8339935
#10                 250      0.9198568  0.8302101
#10                 300      0.9197230  0.8298093
#10                 350      0.9183749  0.8268412
#10                 400      0.9174308  0.8247183
#10                 450      0.9171623  0.8242540
#10                 500      0.9166243  0.8231293


## Models
#gbmFit1$finalModel$tuneValue
#gbmFit1$finalModel$ntree
summary(gbmFit1)
summary(gbmFit2)
plot(gbmFit1)
plot(gbmFit2)

#Checking variable importance for RF 
varImp(object=gbmFit1)
#Overall
#salary   100.00000
#age       91.68487
#credit     2.63822
#elevel.L   0.53319
#elevel.C   0.24682
#elevel^4   0.22945
#elevel.Q   0.14342
#zipcode8   0.13739
#zipcode7   0.12760
#zipcode4   0.12639
#car11      0.12222
#car7       0.10316
#car12      0.09843
#car9       0.09485
#car4       0.09445
#car15      0.08825
#car10      0.07029
#zipcode3   0.06778
#car6       0.05782
#zipcode2   0.05494

#Plotting Varianle importance for Random Forest 
plot(varImp(object=gbmFit1),main="GBM - Variable Importance")

#training results C5.0
c50Fit1
#Resampling results across tuning parameters:
#  model  winnow  trials  Accuracy   Kappa    
#rules  FALSE    1      0.8273226  0.6562566
#rules  FALSE   10      0.9205253  0.8306355
#rules  FALSE   20      0.9220076  0.8345492
#rules  FALSE   30      0.9218733  0.8344520
#rules  FALSE   40      0.9218733  0.8344520
#rules   TRUE    1      0.8273226  0.6501318
#rules   TRUE   10      0.9201219  0.8295399
#rules   TRUE   20      0.9224119  0.8353088
#rules   TRUE   30      0.9230868  0.8369892
#rules   TRUE   40      0.9232216  0.8372652
#tree   FALSE    1      0.8273226  0.6562566
#tree   FALSE   10      0.9225463  0.8356694
#tree   FALSE   20      0.9242965  0.8395343
#tree   FALSE   30      0.9216031  0.8341819
#tree   FALSE   40      0.9228156  0.8368201
#tree    TRUE    1      0.8273226  0.6501318
#tree    TRUE   10      0.9236243  0.8377465
#tree    TRUE   20      0.9261838  0.8434209
#tree    TRUE   30      0.9247033  0.8406855
#tree    TRUE   40      0.9249725  0.8412727
#
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were trials = 20, model = tree and winnow = TRUE.
c50Fit2
#Resampling results across tuning parameters:
#  
#  model  winnow  trials  Accuracy   Kappa    
#rules  FALSE    1      0.8597624  0.7118880
#rules  FALSE   10      0.9198514  0.8292171
#rules  FALSE   20      0.9210632  0.8323592
#rules  FALSE   30      0.9209285  0.8320264
#rules  FALSE   40      0.9205247  0.8311753
#rules  FALSE   50      0.9207939  0.8318227
#rules  FALSE   60      0.9210630  0.8323566
#rules  FALSE   70      0.9210630  0.8323922
#rules  FALSE   80      0.9210630  0.8323922
#rules  FALSE   90      0.9210630  0.8323922
#rules   TRUE    1      0.8510159  0.6973969
#rules   TRUE   10      0.9199883  0.8295408
#rules   TRUE   20      0.9206609  0.8315773
#rules   TRUE   30      0.9201216  0.8304069
#rules   TRUE   40      0.9201215  0.8303460
#rules   TRUE   50      0.9203906  0.8309934
#rules   TRUE   60      0.9206598  0.8315273
#rules   TRUE   70      0.9206598  0.8315630
#rules   TRUE   80      0.9206598  0.8315630
#rules   TRUE   90      0.9206598  0.8315630
#tree   FALSE    1      0.8598968  0.7122632
#tree   FALSE   10      0.9195817  0.8291200
#tree   FALSE   20      0.9186386  0.8278745
#tree   FALSE   30      0.9190424  0.8287321
#tree   FALSE   40      0.9195811  0.8299435
#tree   FALSE   50      0.9193114  0.8294274
#tree   FALSE   60      0.9194460  0.8297519
#tree   FALSE   70      0.9193114  0.8294537
#tree   FALSE   80      0.9191768  0.8291550
#tree   FALSE   90      0.9189076  0.8285839
#tree    TRUE    1      0.8519582  0.6973596
#tree    TRUE   10      0.9211989  0.8329102
#tree    TRUE   20      0.9220075  0.8350053
#tree    TRUE   30      0.9217384  0.8346489
#tree    TRUE   40      0.9221421  0.8355721
#tree    TRUE   50      0.9222767  0.8358463
#tree    TRUE   60      0.9222767  0.8358463
#tree    TRUE   70      0.9222767  0.8358463
#tree    TRUE   80      0.9222767  0.8358463
#tree    TRUE   90      0.9222767  0.8358463


## Models
#c50Fit1$finalModel$tuneValue
#c50Fit1$finalModel$ntree
summary(c50Fit1)
summary(c50Fit2)
plot(c50Fit1)
plot(c50Fit2)

#Checking variable importance for RF 
varImp(object=c50Fit1)
#Overall
#age       100.00
#salary    100.00
#credit     36.85
#car6       24.18
#zipcode7   22.01
#car13      21.35
#car17      18.87
#car15      18.37
#car7       13.62
#zipcode4    6.88
#car3        0.00
#car20       0.00
#zipcode2    0.00
#car5        0.00
#car12       0.00
#elevel.C    0.00
#car10       0.00
#car2        0.00
#car19       0.00
#zipcode6    0.00

#Plotting Varianle importance for Random Forest 
plot(varImp(object=c50Fit1),main="C5.0 - Variable Importance")

#training results Partial Least Squares
plsFit1
#Resampling results across tuning parameters:
#  
#  ncomp  Accuracy   Kappa      
#1      0.5792143  -0.05131197
#2      0.5394672  -0.11097607
#3      0.5237084  -0.14908685
#4      0.5392009  -0.11103400
#5      0.5429723  -0.09955973
#
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was ncomp = 1
plsFit2
#Resampling results across tuning parameters:
#  
#  ncomp  Accuracy   Kappa      
#1      0.5786727  -0.05060905
#2      0.5404081  -0.11009834
#3      0.5241109  -0.14906830
#4      0.5266694  -0.14239497
#5      0.5280142  -0.14028117
#
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was ncomp = 1.

## Models
#plsFit1$finalModel$tuneValue
#plsFit1$finalModel$ntree
summary(plsFit1)
summary(plsFit2)
plot(plsFit1)
plot(plsFit2)

#Checking variable importance for RF 
varImp(object=plsFit1)
#Overall
#salary   1.000e+02
#credit   1.592e+01
#age      3.490e-03
#zipcode8 1.026e-04
#car11    5.950e-05
#car8     5.649e-05
#elevel^4 5.272e-05
#car3     5.184e-05
#car6     4.680e-05
#car9     4.680e-05
#zipcode6 4.556e-05
#zipcode7 4.246e-05
#car18    4.028e-05
#zipcode5 3.770e-05
#zipcode2 3.092e-05
#car17    3.056e-05
#car13    2.775e-05
#elevel.Q 2.750e-05
#elevel.L 2.622e-05
#elevel.C 2.588e-05

#Plotting Varianle importance for Random Forest 
plot(varImp(object=plsFit1),main="Partial Least Squares - Variable Importance")

##Predict with Random Forest
testPredrf1<- predict(rfFit1, testing)
postResample(testPredrf1, testing$brand)
#Accuracy     Kappa 
#0.8799515 0.7451652 
testPredrf2<- predict(rfFit2, testing)
postResample(testPredrf2, testing$brand)
#Accuracy     Kappa 
#0.9143088 0.8184389 

##Predict with GBM
testPredgbm1<- predict(gbmFit1, testing)
postResample(testPredgbm1, testing$brand)
#Accuracy     Kappa 
#0.9240097 0.8394613 
testPredgbm2<- predict(gbmFit2, testing)
postResample(testPredgbm2, testing$brand)
#Accuracy     Kappa 
#0.9244139 0.8402157 

##Predict with C5.0
testPredc501<- predict(c50Fit1, testing) 
postResample(testPredc501, testing$brand)
#Accuracy     Kappa 
#0.9195635 0.8307364 
testPredc502<- predict(c50Fit2, testing) 
postResample(testPredc502, testing$brand)
#Accuracy     Kappa 
#0.9179466 0.8271194 

##Predict with PLS
testPredpls1<- predict(plsFit1, testing)
postResample(testPredpls1, testing$brand)
#Accuracy       Kappa 
#0.58286176 -0.03578476 
testPredpls2<- predict(plsFit2, testing)
postResample(testPredpls2, testing$brand)
#Accuracy       Kappa 
#0.58286176 -0.03578476 

#Prediction on Survey Incomplete
FinalPredgbm <- predict(gbmFit1, SurveyIncomplete)
postResample(FinalPredgbm, SurveyIncomplete$brand)
#Accuracy      Kappa 
#0.39580000 0.01133512 
FinalPredgbm2 <- predict(gbmFit2, SurveyIncomplete)
postResample(FinalPredgbm2, SurveyIncomplete$brand)
#Accuracy      Kappa 
#0.39500000 0.01191735 
# See Predicted totals # add with CompletedSurvey to see total value 15,000
summary(FinalPredgbm)
#0    1 
#1930 3070 
summary(CompleteResponses$brand)
#0    1 
#3744  6154 

##Totals
# Acer: 5674   Sony: 9224
