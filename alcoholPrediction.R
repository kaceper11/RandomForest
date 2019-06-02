#Wzorzec: https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
#Feature selection: https://www.kaggle.com/nikhilesh87/easy-feature-selection-for-beginners-in-r
#Wniosek: Feature selection bierze najlepsze zmienne, a gdy chcemy uci¹æ wszystkie inne
#to model siê pogarsza i dostajemy wiêkszy b³¹d predykcji, => wiêcyj zmiennych => lepiej

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

levels(TrainSet$Dalc); # klasy który mamy przewidzieæ, 1 - ma³e spo¿ycie %, 5 - du¿e
TrainSet$Dalc <- factor(TrainSet$Dalc); # zmieniamy wartoœci z ci¹g³ych na dysktetne ¿eby móc skorzystaæ z klasyfikacji, w przeciwnym wypadku by³aby regresja




##Tworzenie drzewka
# Create a Random Forest model with default parameters
model1 <- randomForest(Dalc ~ ., data = TrainSet, importance = TRUE)
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie mo¿na model boostowaæ poprzez dobór wag, czyli parametry classwt i sampsize ale œrednio mi to pomog³o, 
#mo¿na zrobiæ pêtle co by sprawdza³a œrêdni¹, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod mo¿e siê wywalaæ jeœli ustawimy wiêcej (sampsize) dla danej klasy ni¿ jestliczy dla niej zbór ternuj¹cy
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Dalc)

# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class") #class and prob
# Checking classification accuracy
mean(predValid == ValidSet$Dalc) #dok³adnoœæ jak dobrze nasz model przewiduje na niezale¿ym zbierze ni¿ trenuj¹cy                  
table(predValid,ValidSet$Dalc)

#importance(model1)     #wykres wa¿noœci zmiennych, dodamy to w pkcie 5   
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
##Plan dzia³ania (robimy wszystko dla zmiennej Dalc - daily alcohal cons., w sumie Walc mo¿emy sobie darowaæ, albo zrobiæ kopiuj wklej)
#1. Sprawdziæ ka¿dy parametr w pêtli, jaka wartoœæ nalepsza, czyli
# a) mtry (DONE)  -> najlepszy wynik (1,2)
# b) ntree (DONE) -> najlepszy wynik (400,500,600) trudno powiedzieæ ale w tych okolicach
################################TO DO##############################
# c) classwt (daæ proównanie do braku tego parametru)
# d) samplesize (daæ proównanie do braku tego parametru)
#2. Sprawdziæ jaki stosunek zbioru trenuj¹cego do waliduj¹cego daje najlepszy rezultat
#3. Sprawdziæ ile najlepszych zmiennych daje najlepszy rezultat
#4. Sprawdziæ jaki seed daje najlepsze wyniki

#5. Jeœli ju¿ dobierzemy najlepsze parametry to mo¿na je wykorzystaæ, puœciæ
#jeszcze raz model z nimi, podaæ dok³adnoœæ i narysowaæ wykres wa¿noœci zmiennych (importance)
#6. Dodatkowo mo¿na te¿ dodaæ zmienn¹ Dalc

##Jak to zrobiæ
##Trzeba zrobiæ loop w loopie, sprawdzamy przedzia³ wartoœci, wszystko powtarzamy N razy, 
#sumujemy do zmiennej i obliczmy œredni¹
#W przypadku 1. wystarczy tylko zmieniæ dodawaæ parametry w metodzie, w pozosta³ych trzeba budowaæ te¿ 
#ca³y model od nowa

#TIP: Polecam zakomentowaæ moje pomiary, 1a i 1b bo trochê zajmuje ich liczenie!!!!!!!

##########################################################################
##########################################################################
####Wybór parametrów  (pkt 1)
model1 <- randomForest(Dalc ~ ., data = TrainSet, mtry = 5, ntree = 500, importance = TRUE)
model1
#, classwt = c(8E1,80E1,100E1,200E1,8000E1))
#, classwt = c(80E1,20E1,10E1,8E1,8E1))
#, sampsize = c(180,40,12,6,5), replace=TRUE)
#generalnie mo¿na model boostowaæ poprzez dobór wag, czyli parametry classwt i sampsize ale œrednio mi to pomog³o, 
#mo¿na zrobiæ pêtle co by sprawdza³a œrêdni¹, parametry te dobieramy dla wektora 5 elem. bo mamy 5 klas
#kod mo¿e siê wywalaæ jeœli ustawimy wiêcej (sampsize) dla danej klasy ni¿ jest liczy dla niej zbór ternuj¹cy

##########################################################################
##########################################################################
####Wybór najistotniejszych zmiennych (pkt 2)
train <- sample(nrow(d3), 0.70*nrow(d3), replace = FALSE) # stosunek zbioru trenuj¹cego do do waliduj¹cego

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
  fit <- lm(TrainSet$Dalc ~ TrainSet[,i])  #sprawdzamy korelacjê przewidywanej zmiennej z konkretn¹ zmienn¹
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}

#ord stores the column number in order of increasing p-value
ord <- order(pvalues) 
#Getting the column numbers for top 10 features with the predictor salerprice
ord <- ord[0:25]   #wybieramy liczbê najlepszych kolumn
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
####Dodanie tych dodatkowych parametrów i generowanie nowego drzewa i wyliczeniem wa¿noœci (pkt 5)
#odkomentowaæ, daje fajne wyniki
#importance(model1)     #wykres wa¿noœci zmiennych   
#varImpPlot(model1)  

##########################################################################
##########################################################################
####Dodanie zmiennej walc (pkt 6)
#usun¹æ poni¿szy kod przy tworzeniu modelu
d1 <- d1[-c(28:29)] # wycinamy zmienn¹ Walc, jest ona bardzo skorelowana, to prawie to samo
d2 <- d2[-c(28:29)]