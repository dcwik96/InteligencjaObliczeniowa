#BIBLIOTEKI
library(neuralnet)
library(readr)

#FUNKCJE POMOCNICZE
norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

#WCZYTANIE DANYCH
dane <- read_csv("dane.csv")
dane <- data.frame(dane[1:3], dane[9:12],dane[14], dane[29:30], dane[50])
#NORMALIZACJA DANYCH
dane.norm <- norm(dane)
#PODZIAL NA ZBIORY TRENINGOWY I TESTOWY
set.seed(1234)
ind <- sample(2, nrow(dane), replace=TRUE, prob=c(0.67, 0.33))
dane.train <- dane[ind==1, 1:length(dane)]
dane.test <- dane[ind==2, 1:length(dane)]

#ZAPISANIE DO ZMIENNEJ NAZWY KOLUMN OUTPUT I INPUT
outputNames <- names(dane[5:8])
inputNames <- names(dane[-5:-8])

#NEURALNET
###Ustawienie wartosci hidden
hiddenAmount <- 5
###Zapisanie do zmiennej ilosci zmiennych output(4) i input(7)
outputAmount <- length(outputNames)
inputAmount <- length(inputNames)
###Stworzenie formuly
formula <- paste(paste(outputNames, collapse=" + "),
                 paste(inputNames, collapse=" + "),
                 sep=" ~ ")
###Wykonanie metody "neuralnet"
dane.neuralnet <- neuralnet(formula, dane.train, hidden=hiddenAmount,threshold = 0.01, stepmax = 1e+6)
###Stworzenie wykresu sieci
plot(dane.neuralnet)
###Zapisanie do zmiennej wartosci bias i weights
bias1 <- dane.neuralnet[["weights"]][[1]][[1]][1,]
bias2 <- dane.neuralnet[["weights"]][[1]][[2]][1,]
weights1 <- dane.neuralnet[["weights"]][[1]][[1]][2:(inputAmount+1),]
weights2 <- dane.neuralnet[["weights"]][[1]][[2]][2:(hiddenAmount+1),]
###Sprawdzenie czy wszystkie wartosci policzone
length(bias1) == hiddenAmount
length(bias2) == outputAmount
length(weights1) == inputAmount * hiddenAmount
length(weights2) == hiddenAmount * outputAmount
###Wypis wartosci jako string ulatwiajacej uzycie danych w skrypcie JS
toString(bias1)
toString(bias2)
toString(weights1)
toString(weights2)
