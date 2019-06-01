#Wzorzec: https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
#Feature selection: https://www.kaggle.com/nikhilesh87/easy-feature-selection-for-beginners-in-r
#Wniosek: Feature selection bierze najlepsze zmienne, a gdy chcemy uciąć wszystkie inne
#to model się pogarsza i dostajemy większy błąd predykcji, => wincyj zmiennych => lepiej

#install.packages("randomForest") #odkomentuj aby zainstalować pakiet
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

set.seed(29790) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # sotsunek zbioru trenującego do do walidującego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); # klasy który mamy przewidzieć, 1 - małe spożycie %, 5 - duże
TrainSet$Dalc <- factor(TrainSet$Dalc); # zmieniamy wartości z ciągłych na dysktetne żeby móc skorzystać z klasyfikacji, w przeciwnym wypadku byłaby regresja




##Tworzenie drzewka
# Create a Random Forest model with default parameters
model1 <- randomForest(Dalc ~ ., data = TrainSet, importance = TRUE)
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie można model boostować poprzez dobór wag, czyli parametry classwt i sampsize ale średnio mi to pomogło, 
#można zrobić pętle co by sprawdzała śrędnią, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod moe się wywalać jeśli ustawimy więcej (sampsize) dla danej klasy niż jestliczy dla niej zbór ternujący
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Dalc)

# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class") #class and prob
# Checking classification accuracy
mean(predValid == ValidSet$Dalc) #dokładność jak dobrze nasz model przewiduje na niezależym zbierze niż trenujący                  
table(predValid,ValidSet$Dalc)

#importance(model1)     #wykres ważności zmiennych, dodamy to w pkcie 5   
#varImpPlot(model1)  

##########################################################################
##1a)
# mtry evaluation
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

##########################################################################
##1b)
# ntree evaluation
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



##########################################################################
##Plan działania (robimy wszystko dla zmiennej Dalc - daily alcohal cons., w sumie Walc możemy sobie darować, albo zrobić kopiuj wklej)
#1. Sprawdzić każdy parametr w pętli, jaka wartość nalepsza, czyli
# a) mtry (DONE)  -> najelszy wynik (1,2)
# b) ntree (DONE) -> najelszy wynik (400,500,600) trudno powiedzieć ale w tych okolicach
################################TO DO##############################
# c) classwt (dać proównanie do braku tego parametru)
# d) samplesize (dać proównanie do braku tego parametru)
#2. Sprawdzić jaki stosunek zbioru trenującego do walidującego daje najlepszy rezultat
#3. Sprawdzić ile najlepszych zmiennych daje najlepszy rezultat
#4. Sprawdzić jaki seed daje najlepsze wyniki

#5. Jeśli już dobierzemy najlepsze parametry to można je wykorzystać, puścić
#jeszcze raz model z nimi, podać dokładność i narysować wykres ważności zmiennych (importance)
#6. Dodatkowo można też dodać zmienną Dalc

##Jak to zrobić
##Trzeba zrobić loop w loopie, sprawdzamy przedział wartości, wszystko powararzamy N razy, 
#sumujemy do zmiennej i obliczmy średnią
#W przypadku 1. wystarczy tylko zmienić dodawać parametry w metodzie, w poostałych trzeba budować też 
#cały model od nowa

#TIP: Polecam zakomentować moje pomiary, 1a i 1b to trochę zajmuje ich liczenie!!!!!!!

##########################################################################
##########################################################################
####Wybór parametrów  (pkt 1)
model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 5, ntree = 500, importance = TRUE)
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie można model boostować poprzez dobór wag, czyli parametry classwt i sampsize ale średnio mi to pomogło, 
#można zrobić pętle co by sprawdzała śrędnią, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod moe się wywalać jeśli ustawimy więcej (sampsize) dla danej klasy niż jestliczy dla niej zbór ternujący

##########################################################################
##########################################################################
####Wybór najistotniejszych zmiennych (pkt 2)
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # sotsunek zbioru trenującego do do walidującego

##########################################################################
##########################################################################
####Wybór najistotniejszych zmiennych (pkt 3) - feature selection
#No. of cols in data frame
c <- ncol(TrainSet) #zliczamy zmienne kolumny
#Intializing the vector which will contain the p-values of all variables
pvalues <- numeric(c)
# Getting the p-values
for (i in 1:c)
{
  fit <- lm(TrainSet$Dalc ~ TrainSet[,i])  #sprawdzamy korelację przewidywanej zmiennej z konkretną zmienną
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}

#ord stores the column number in order of increasing p-value
ord <- order(pvalues) 
#Getting the column numbers for top 10 features with the predictor salerprice
ord <- ord[0:25]   #wybieramy liczbę najlepszych kolumn
Dalc <- TrainSet[,'Dalc']
TrainSet <- TrainSet[,ord]
TrainSet <- cbind(TrainSet,Dalc)
Dalc <- ValidSet[,'Dalc']
ValidSet <- ValidSet[,ord]
ValidSet <- cbind(ValidSet,Dalc)
##########################################################################
##########################################################################
####Wybór seeda (pkt 4)
set.seed(29790) # ustawianie ziarna doboru


##########################################################################
##########################################################################
####Dodanie tych dodatkowych parametrów i generowanie nowego drzewa i wyliczeniem ważności (pkt 5)
#odkomentować, daje daje fajne wyniki
#importance(model1)     #wykres ważności zmiennych   
#varImpPlot(model1)  

##########################################################################
##########################################################################
####Dodanie zmiennej walc (pkt 6)
#usunąć poniższy kod przy tworzeniu modelu
d1 <- d1[-c(28:29)] # wycinamy zmienną Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]
