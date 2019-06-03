#install.packages("randomForest") #odkomentowaæ aby zainstalowaæ pakiet
library(randomForest)


# Badamy parameter Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienn¹ Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))


print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(22000) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.90*nrow(d3), replace = FALSE) # stosunek zbioru trenuj9cego do do waliduj9cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); 
TrainSet$Dalc <- factor(TrainSet$Dalc); 


withoutClassWtAndSampleSize <- 0
withClassWtAndSampleSize <- 0
withClassWt <- 0
withSampleSize <- 0

for(j in 1:20){
  
  #model without classWT and Samplesize
  model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE)
  predValid <- predict(model1, ValidSet, type = "class")
  withoutClassWtAndSampleSize <- withoutClassWtAndSampleSize + mean(predValid == ValidSet$Dalc)
  
  #model with classWt and SampleSize
  model2 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE, classwt = c(8E1,80E1,100E1,200E1,8000E1), sampsize = c(180,40,12,6,5))
  predValid <- predict(model2, ValidSet, type = "class")
  withClassWtAndSampleSize <- withClassWtAndSampleSize + mean(predValid == ValidSet$Dalc)
  
  #model with classWt and without SampleSize
  model3 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE, classwt = c(8E1,80E1,100E1,200E1,8000E1))
  predValid <- predict(model3, ValidSet, type = "class")
  withClassWt <- withClassWt + mean(predValid == ValidSet$Dalc)
  
  #model with SampleSize and without ClassWt
  model4 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE, classwt = c(8E1,80E1,100E1,200E1,8000E1), sampsize = c(180,40,12,6,5))
  predValid <- predict(model4, ValidSet, type = "class")
  withSampleSize <- withSampleSize + mean(predValid == ValidSet$Dalc)
  
}

withoutClassWtAndSampleSize <- withoutClassWtAndSampleSize/20
withClassWtAndSampleSize <- withClassWtAndSampleSize/20
withClassWt <- withClassWt/20
withSampleSize <- withSampleSize/20

x <- c(withoutClassWtAndSampleSize, withClassWtAndSampleSize, withClassWt, withSampleSize)

#wykres s³upkowy
barplot(x, main="Influence of Workday \nAlcohol Consumtion", ylab="Accuracy", las=3,
        names.arg=c("not both", "both", "classWt", "sampsize"))

#importance of variables
importance(model1)     #model without classWT and Samplesize  
varImpPlot(model1)

importance(model2)     #model with classWt and SampleSize 
varImpPlot(model2)

importance(model3)     #model with classWt and without SampleSize 
varImpPlot(model3)

importance(model4)     #model with SampleSize and without ClassWt 
varImpPlot(model4)

