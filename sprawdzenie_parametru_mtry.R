#install.packages("randomForest") #odkomenta?? aby zainstalowa?? pakiet
library(randomForest)


# Badamy parametr Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienn?? Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))


print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(29790) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj9cego do do waliduj9cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); 
TrainSet$Dalc <- factor(TrainSet$Dalc); 


mTry_factor=c()
a <- (1:20)*0
y <- (1:20)*0
i=1
j=1

for(j in 1:5){
  i = 1
  for (i in 1:20) {
    model3 <- randomForest(Dalc ~ ., data = TrainSet, mtry = i, importance = TRUE)
    predValid <- predict(model3, ValidSet, type = "class")
    y[i] <- y[i] + mean(predValid == ValidSet$Dalc)
  }
}
y <- y/5

plot(1:20,y, main="Evaluation of mtry parameter",
     ylab="Accuracy", xlab="mtry value")

#Najlepszy wynik dla mtry = 3