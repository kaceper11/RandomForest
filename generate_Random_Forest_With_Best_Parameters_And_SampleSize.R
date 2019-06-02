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
train <- sample(nrow(d3), 1*nrow(d3), replace = FALSE) # stosunek zbioru trenuj¹cego do do waliduj¹cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); # klasy który mamy przewidzieæ, 1 - ma³e spo¿ycie %, 5 - du¿e
TrainSet$Dalc <- factor(TrainSet$Dalc); # zmieniamy wartoœci z ci¹g³ych na dysktetne ¿eby móc skorzystaæ z klasyfikacji, w przeciwnym wypadku by³aby regresja


model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 3, ntree = 500, importance = TRUE, sampsize = c(180,40,12,6,5))
model1

importance(model1)        
varImpPlot(model1)     