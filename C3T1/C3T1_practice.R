library(readr)
cars<- read_csv("cars.csv")

#Getting to know the data set.
attributes(cars)#Lists your attributes within the data set.
summary(cars) #Prints the min, max, mean, median, and quartiles of each attribute.
str(cars) #Displays the structure of your data set.
names(cars) #Names your attributes within your data set.
cars$`speed of car` #Will print out the instances within that particular column in your data set.
cars$'distance of car'
#Plotting
#Histogram plot
hist(cars$'speed of car',
     main="Speed of Cars",
     xlab="Speed",
     col="darkmagenta") 
hist(cars$`distance of car`,
     main="Distance of Cars",
     xlab="Distance",
     col="darkgreen")
#Scatter plot
plot(cars$`speed of car`,cars$`distance of car`,
     main="Distance by Speed",
     ylab="Distance of cars",
     xlab="Speed of cars",
     col="darkred")
#Normal Quantile plot - to see if the data is normally distributed
qqnorm(cars$`speed of car`,
       main="Quantile Plot to Visualize How the Data is Distributed",
       col="hotpink")
#How to change datatypes in R
#DatasetName$ColumnName<-as.tpeofdata(DatasetName$ColumnName)
cars$`name of car`<- as.factor(cars$`name of car`)
class(cars$`name of car`) # Check type
str(cars)
is.character(cars$"name of car")
is.factor(cars$"name of car")
is.logical(cars$"name of car")
is.numeric(cars$"name of car")
is.integer(cars$"name of car")
sapply(cars, class)
cars$"name of car"
#Rename attributes/columns in the dataset
names(cars)<-c("Make","Speed","Distance")
#Check for missing values
summary(cars) #Counts how many NA's are present
is.na(cars) #Show the NA's through logical data (True if it's Missing, False if not)

###Remove any observations containing missing data. 
##(If the missing data is less than 10% of the total data and only after comparing 
##the min/max of all the features both with and without the missing data.)

#na.omit(DatasetName$ColumnName#Drops any rows with missing values and omits them forever.
#na.exclude(DatasetName$ColumnName)#Drops any rows with missing values, but keeps track of where they were.

#Replace the missing values with the mean, which is common technique, but something 
#to use with care with as it can skew the data.
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)

#Creating Testing and Training Sets

set.seed(123) #using 123 is a common value, use the same seed number throughout modeling

#Split the data into training and test sets for modeling
#Common splitting is 70/30, training/test respectively.  Also 80/20 is a common split
trainSize<-round(nrow(cars)*0.7)
testSize<-nrow(cars)-trainSize
#Verify how many instances will be in each set
trainSize
testSize

#Create the training and test sets; sets should be in randomized order to create the most optimal model
training_indices<-sample(seq_len(nrow(cars)),size=trainSize)
trainSet<-cars[training_indices,]
testSet<-cars[-training_indices,]

#Linear Regression Model using the linear model function

cars_lm<- lm(Distance~ Speed, trainSet)
summary(cars_lm) #Multiple R-squared- How well the regression line fits the data (1 means it’s a perfect fit).
#p-value - Tells you how much the Independent Variable/Predictor affects the Dependent Variable/Response/. 
#A p-value of more than 0.05 means the Independent Variable has no effect on the Dependent Variable; 
#less than 0.05 means the relationship is statistically significant.

#Predictions - predict the distance through the speed of the cars

cars_pred<- predict(cars_lm,testSet)
cars_pred #view predictions

