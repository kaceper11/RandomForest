#install.packages("randomForest") #odkomentuj aby zainstalowa?? pakiet
library(randomForest)


# Badamy parameter Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienn9 Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))


print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(22000) # ustawianie ziarna doboru

a <- (1:20)*0
y <- (1:10)*0
x <- (1:10)*0.1
i=0.1
j=1

for(j in 1:5){
  i = 0.1
  for (i in (1:10)*0.1) {
    train <- sample(nrow(d3), i*nrow(d3), replace = FALSE) # stosunek zbioru trenuj9cego do do waliduj9cego
    TrainSet <- d3[train,]
    ValidSet <- d3[-train,]
    summary(TrainSet)
    summary(ValidSet)
    
    levels(TrainSet$Dalc); 
    TrainSet$Dalc <- factor(TrainSet$Dalc); 
    
    
    mTry_factor=c()
    model3 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE)
    predValid <- predict(model3, ValidSet, type = "class")
    y[i*10] <- y[i*10] + mean(predValid == ValidSet$Dalc)
  }
}

y <- y/5

plot(x,y, main="Evaluation of ratio parameter",
     ylab="Accuracy", xlab="ratio value")

#Najgorszy wynik dla ratio = 0.1