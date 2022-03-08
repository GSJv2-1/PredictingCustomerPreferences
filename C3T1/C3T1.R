install.packages("readr")
library(readr)

library(plyr) #Used to change values in Species numeric

IrisDF<- read_csv("iris.csv")
attributes(IrisDF)
summary(IrisDF)
str(IrisDF)
names(IrisDF)

revalue(IrisDF$Species, c("setosa" = "1"))->IrisDF$Species
revalue(IrisDF$Species, c("versicolor" = "2"))->IrisDF$Species
revalue(IrisDF$Species, c("virginica" = "3"))->IrisDF$Species

IrisDF$Species<- as.numeric(IrisDF$Species)

head(IrisDF$Species)

class(IrisDF$Species)
summary(IrisDF$Species)


hist(IrisDF$Species)
plot(IrisDF$Sepal.Length)
qqnorm(IrisDF$Species)
qqnorm(IrisDF$Sepal.Length)
qqnorm(IrisDF$Sepal.Width)
qqnorm(IrisDF$Petal.Length)
qqnorm(IrisDF$Petal.Width)


set.seed(123)

trainSize<-round(nrow(IrisDF)*0.2)
testSize<-nrow(IrisDF)-trainSet
trainSize
testSize

trainSet<-IrisDF[training_indices,]
testSet<-IrisDF[-training_indices,]

LinearModel<-lm(Petal.Width~ Petal.Length, trainSet)
summary(LinearModel)

predictions<-predict(LinearModel,testSet)
predictions
