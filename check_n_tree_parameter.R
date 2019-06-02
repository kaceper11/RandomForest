#install.packages("randomForest") #odkomentuj aby zainstalowaæ pakiet
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

set.seed(29790) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj¹cego do do waliduj¹cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); 
TrainSet$Dalc <- factor(TrainSet$Dalc); 

nTree_factor=c()
a <- (1:10)*0
y <- (1:10)*0
x <- (1:10) * 100
i=1
j=1

for(j in 1:20){
  i = 1
  for (i in 1:10) {
    model3 <- randomForest(Dalc ~ ., data = TrainSet, ntree = x[i], importance = TRUE)
    predValid <- predict(model3, ValidSet, type = "class")
    y[i] <- y[i] + mean(predValid == ValidSet$Dalc)
  }
}

y <- y/20

plot(x,y, main="Evaluation of ntree parameter",
     ylab="Accuracy", xlab="ntree value")

#Najlepszy wynik dla ntree = 500