#Wzorzec: https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
#Feature selection: https://www.kaggle.com/nikhilesh87/easy-feature-selection-for-beginners-in-r
#Wniosek: Feature selection bierze najlepsze zmienne, a gdy chcemy uci�� wszystkie inne
#to model si� pogarsza i dostajemy wi�kszy b��d predykcji, => wi�cyj zmiennych => lepiej

#install.packages("randomForest") #odkomentuj aby zainstalowa� pakiet
library(randomForest)


# Badamy parameter Dalc, 
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
d1 <- d1[-c(28:29)] # wycinamy zmienn� Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet", "Dalc"))



print(nrow(d3)) # 382 students

str(d3)
summary(d3)

set.seed(29790) # ustawianie ziarna doboru
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj�cego do do waliduj�cego
TrainSet <- d3[train,]
ValidSet <- d3[-train,]
summary(TrainSet)
summary(ValidSet)

levels(TrainSet$Dalc); # klasy kt�ry mamy przewidzie�, 1 - ma�e spo�ycie %, 5 - du�e
TrainSet$Dalc <- factor(TrainSet$Dalc); # zmieniamy warto�ci z ci�g�ych na dysktetne �eby m�c skorzysta� z klasyfikacji, w przeciwnym wypadku by�aby regresja




##Tworzenie drzewka
# Create a Random Forest model with default parameters
model1 <- randomForest(Dalc ~ ., data = TrainSet, importance = TRUE)
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie mo�na model boostowa� poprzez dob�r wag, czyli parametry classwt i sampsize ale �rednio mi to pomog�o, 
#mo�na zrobi� p�tle co by sprawdza�a �r�dni�, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod mo�e si� wywala� je�li ustawimy wi�cej (sampsize) dla danej klasy ni� jestliczy dla niej zb�r ternuj�cy
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Dalc)

# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class") #class and prob
# Checking classification accuracy
mean(predValid == ValidSet$Dalc) #dok�adno�� jak dobrze nasz model przewiduje na niezale�ym zbierze ni� trenuj�cy                  
table(predValid,ValidSet$Dalc)

#importance(model1)     #wykres wa�no�ci zmiennych, dodamy to w pkcie 5   
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
##Plan dzia�ania (robimy wszystko dla zmiennej Dalc - daily alcohal cons., w sumie Walc mo�emy sobie darowa�, albo zrobi� kopiuj wklej)
#1. Sprawdzi� ka�dy parametr w p�tli, jaka warto�� nalepsza, czyli
# a) mtry (DONE)  -> najlepszy wynik (1,2)
# b) ntree (DONE) -> najlepszy wynik (400,500,600) trudno powiedzie� ale w tych okolicach
################################TO DO##############################
# c) classwt (da� pro�wnanie do braku tego parametru)
# d) samplesize (da� pro�wnanie do braku tego parametru)
#2. Sprawdzi� jaki stosunek zbioru trenuj�cego do waliduj�cego daje najlepszy rezultat
#3. Sprawdzi� ile najlepszych zmiennych daje najlepszy rezultat
#4. Sprawdzi� jaki seed daje najlepsze wyniki

#5. Je�li ju� dobierzemy najlepsze parametry to mo�na je wykorzysta�, pu�ci�
#jeszcze raz model z nimi, poda� dok�adno�� i narysowa� wykres wa�no�ci zmiennych (importance)
#6. Dodatkowo mo�na te� doda� zmienn� Dalc

##Jak to zrobi�
##Trzeba zrobi� loop w loopie, sprawdzamy przedzia� warto�ci, wszystko powtarzamy N razy, 
#sumujemy do zmiennej i obliczmy �redni�
#W przypadku 1. wystarczy tylko zmieni� dodawa� parametry w metodzie, w pozosta�ych trzeba budowa� te� 
#ca�y model od nowa

#TIP: Polecam zakomentowa� moje pomiary, 1a i 1b bo troch� zajmuje ich liczenie!!!!!!!

##########################################################################
##########################################################################
####Wyb�r parametr�w  (pkt 1)
model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 5, ntree = 500, importance = TRUE)
model1
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie mo�na model boostowa� poprzez dob�r wag, czyli parametry classwt i sampsize ale �rednio mi to pomog�o, 
#mo�na zrobi� p�tle co by sprawdza�a �r�dni�, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod mo�e si� wywala� je�li ustawimy wi�cej (sampsize) dla danej klasy ni� jest liczy dla niej zb�r ternuj�cy

##########################################################################
##########################################################################
####Wyb�r najistotniejszych zmiennych (pkt 2)
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj�cego do do waliduj�cego

##########################################################################
##########################################################################
####Wyb�r najistotniejszych zmiennych (pkt 3) - feature selection
#No. of cols in data frame
c <- ncol(TrainSet) #zliczamy zmienne kolumny
#Intializing the vector which will contain the p-values of all variables
pvalues <- numeric(c)
# Getting the p-values
for (i in 1:c)
{
  fit <- lm(TrainSet$Dalc ~ TrainSet[,i])  #sprawdzamy korelacj� przewidywanej zmiennej z konkretn� zmienn�
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}

#ord stores the column number in order of increasing p-value
ord <- order(pvalues) 
#Getting the column numbers for top 10 features with the predictor salerprice
ord <- ord[0:25]   #wybieramy liczb� najlepszych kolumn
Dalc <- TrainSet[,'Dalc']
TrainSet <- TrainSet[,ord]
TrainSet <- cbind(TrainSet,Dalc)
Dalc <- ValidSet[,'Dalc']
ValidSet <- ValidSet[,ord]
ValidSet <- cbind(ValidSet,Dalc)
##########################################################################
##########################################################################
####Wyb�r seeda (pkt 4)
set.seed(29790) # ustawianie ziarna doboru


##########################################################################
##########################################################################
####Dodanie tych dodatkowych parametr�w i generowanie nowego drzewa i wyliczeniem wa�no�ci (pkt 5)
#odkomentowa�, daje fajne wyniki
#importance(model1)     #wykres wa�no�ci zmiennych   
#varImpPlot(model1)  

##########################################################################
##########################################################################
####Dodanie zmiennej walc (pkt 6)
#usun�� poni�szy kod przy tworzeniu modelu
d1 <- d1[-c(28:29)] # wycinamy zmienn� Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]