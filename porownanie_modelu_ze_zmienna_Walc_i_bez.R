#install.packages("randomForest") #odkomentować aby zainstalować pakiet
library(randomForest)


# Badamy parameter Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1W <-d1
d2W <-d2
d1 <- d1[-c(28:29)] # wycinamy zmienną Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))
d3W=merge(d1W,d2W,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))


set.seed(29790) # ustawianie ziarna doboru


#Tworzymy dla osobne modele dla przypadku z zmienn?? walc oraz bez niej
#Celem tego testu jest sprawdzenie co si?? stanie jak dodamy silnie skorelowan?? zmienn??
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj?cego do do waliduj?cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]

trainW <- sample(nrow(d3W), 0.70*nrow(d3W), replace = FALSE) # stosunek zbioru trenującego do do walidującego
TrainSetW <- d3W[trainW,]
ValidSetW <- d3W[-trainW,]


TrainSet$Dalc <- factor(TrainSet$Dalc); 
TrainSetW$Dalc <- factor(TrainSetW$Dalc); 

ValidSet$Walc <- factor(ValidSet$Walc); 
ValidSetW$Walc <- factor(ValidSetW$Walc); 


withoutWalc <- 0
withWalc <- 0


for(j in 1:20){
  
  #model excluded
  model3 <- randomForest(Dalc ~ ., data = TrainSet, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  withoutWalc <- withoutWalc + mean(predValid == ValidSet$Dalc)
  
  #model included
  model4 <- randomForest(Dalc ~ ., data = TrainSetW, importance = TRUE)
  predValidW <- predict(model4, ValidSetW, type = "class")
  withWalc <- withWalc + mean(predValidW == ValidSetW$Dalc)
  
}

withoutWalc <- withoutWalc / 20
withWalc <- withWalc / 20
x <- c(withoutWalc, withWalc)

#wykres s??upkowy
barplot(x, main="Influence of Workday \nAlcohol Consumtion", ylab="Accuracy",
        names.arg=c("Excluded", "Included"))

#importance of variables
importance(model3)     #walc excluded
varImpPlot(model3)

importance(model4)     #walc included
varImpPlot(model4)