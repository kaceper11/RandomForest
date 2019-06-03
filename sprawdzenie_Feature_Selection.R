#install.packages("randomForest") #odkomentać aby zainstalować pakiet
library(randomForest)

# Badamy parametr Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienną Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))


print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(29790) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.7*nrow(d3), replace = FALSE) # stosunek zbioru trenującego do do walidującego
levels(train$Dalc); 
train$Dalc <- factor(train$Dalc); 
TrainSet <- d3[train,]
ValidSet <- d3[-train,]



c <- ncol(TrainSet) #zliczamy zmienne kolumny
#Intializing the vector which will contain the p-values of all variables
pvalues <- numeric(c)
# Getting the p-values

for (i in 1:c)
{
  fit <- lm(TrainSet$Dalc ~ TrainSet[,i])  #sprawdzamy korelacjo?= przewidywanej zmiennej z konkretno?= zmienno?=
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}

#ord stores the column number in order of increasing p-value
ord <- order(pvalues) 
a <- (1:16)*0

tempTrainSet <- TrainSet
tempValidSet <- ValidSet
tempOrd <- ord

for(k in 1:10){
for(j in (1:16)*3){
  
  #Getting the column numbers for top 10 features with the predictor salerprice
  tempOrd <- ord[0:j]   #wybieramy liczbę najlepszych kolumn
  Dalc <- TrainSet[,'Dalc']
  tempTrainSet <- TrainSet[,tempOrd]
  tempTrainSet <- cbind(tempTrainSet,Dalc)
  Dalc <- ValidSet[,'Dalc']
  tempValidSet <- ValidSet[,tempOrd]
  tempValidSet <- cbind(tempValidSet,Dalc)
  
  summary(tempTrainSet)
  summary(tempValidSet)
  
  tempTrainSet$Dalc <- factor(tempTrainSet$Dalc); 
  tempValidSet$Dalc <- factor(tempValidSet$Dalc); 

  
  model1 <- randomForest(Dalc ~ ., data = tempTrainSet, importance = TRUE)
  model1
  
  predValid <- predict(model1, tempValidSet, type = "class")
  a[j/3] <- a[j/3] + mean(predValid == tempValidSet$Dalc)
  
}
}
a <- a/10
a

plot((1:16)*3,a, main="Evaluation of top features selection",
     ylab="Accuracy", xlab="Number of used (top) features")

   