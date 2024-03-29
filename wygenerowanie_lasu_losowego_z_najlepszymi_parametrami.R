#install.packages("randomForest") #odkomentować aby zainstalować pakiet
library(randomForest)

# Badamy parameter Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienną Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))



print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(22000) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.9*nrow(d3), replace = FALSE) # stosunek zbioru trenuj9cego do do waliduj9cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); # klasy które mamy przewidzieć, 1 - małe spożycie %, 5 - duże
TrainSet$Dalc <- factor(TrainSet$Dalc); # zmieniamy wartości z ciągłych na dysktetne żeby msc skorzystać z klasyfikacji, w przeciwnym wypadku byłaby regresja


model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE)
model1

importance(model1)        
varImpPlot(model1)     