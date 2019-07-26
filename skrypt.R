### INSTALACJA WYMAGANYCH PACZEK
install.packages("party")
install.packages("readr")
install.packages("e1071")
install.packages("ggplot2")
install.packages("class")
install.packages("MASS")
install.packages("editrules")
install.packages("arules")

library(party)
library(readr)
library(e1071)
library(ggplot2)
library(class)
library(MASS)
library(editrules)
library(arules)

### PRZYGOTOWANIE DANYCH
# Wczytanie danych z pliku
data <- read_csv("Pulpit/InteligencjaObliczeniowa/Projekt2/Projekt2/Dataset_spine.csv")
# Usuniecie ostatniej kolumny, ktora nie ma wplywu na zadanie
data <- data[,-14]
# Ustawienie nazw kolumn
names(data) <- c("Padanie_miednicy", "Pochylenie_miednicy", "Lordoza_lędźwiowa", "Nachylenie_kości_krzyżowej", "Kąt_ułożenia_miednicy", "Stopień_zwyrodnienia", "Zbocze_miednicy", "Kierunkowe_pochylenie", "Nachylenie_piersiowego", "Pochylenie_szyjnego", "Kąt_kości_krzyżowej", "Nachylenie_boczne", "Ocena")
# Zamiana abnormal/normal na 0-normal 1-abnormal
for ( i in 1:nrow(data) ) {
  if (data["Ocena"][i,] == "Abnaormal") data["Ocena"][i,] <- as.numeric(1)
  else if (data["Ocena"][i,] == "Normal") data["Ocena"][i,] <- as.numeric(0)
}

data$Ocena <- factor(data$Ocena)


### NORMALIZACJA
normalize <- function(vector) {
  (vector - min(vector))/((max(vector))-min(vector))
}

### PRZYGOTOWANIE DANYCH TESTOWYCH ORAZ TRENINGOWYCH
set.seed(1234)
#Dane surowe
data.raw <- data
#Dane testowe
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
#zwykle
data.training <- data[ind==1,] 
data.test <-data[ind==2,]
#znormalizowane
data.norm <- normalize(data[1:12])
data.norm <- cbind(data.norm, data[13])
data.norm.training <- data.norm[ind==1,]
data.norm.test <- data.norm[ind==2,]

faktyczna_ocena <- data.test[,13]

###KLASYFIKATORY
#DECISION TREE
data.ctree <- ctree(Ocena ~ ., data = data.training)
predicted_tree <- predict(data.ctree, data.test[,1:12])
real_tree <- data.test[,13]
cm_tree <- table(predicted_tree, faktyczna_ocena$Ocena)
accuracy_tree <- sum(diag(cm_tree))/sum(cm_tree)

#NAIVE BAYES
data.naive <- naiveBayes(Ocena ~ ., data = data.training)
predicted_naive <- predict(data.naive, data.test[,1:12])
cm_naive <- table(predicted_naive,faktyczna_ocena$Ocena)
accuracy_naive <- sum(diag(cm_naive))/sum(cm_naive)

#kNN 
data.knn3 <- knn(data.norm.training[,1:12], data.norm.test[,1:12], cl = data.norm.training[,13], k = 3, prob = FALSE)
predicted_knn3 <- data.knn3
cm_knn3 <- table(predicted_knn3,faktyczna_ocena$Ocena)
accuracy_knn3 <- sum(diag(cm_knn3))/sum(cm_knn3)

data.knn1 <- knn(data.norm.training[,1:12], data.norm.test[,1:12], cl = data.norm.training[,13], k = 1, prob = FALSE)
predicted_knn1 <- data.knn1
cm_knn1 <- table(predicted_knn1,faktyczna_ocena$Ocena)
accuracy_knn1 <- sum(diag(cm_knn1))/sum(cm_knn1)

#LDA
data.lda <- lda(Ocena ~ ., data = data.training)
predicted_lda <- predict(data.lda, data.test[, 1:12])$class
cm_lda <- table(predicted_lda, faktyczna_ocena$Ocena)
accuracy_lda <- sum(diag(cm_lda))/sum(cm_lda)

#WYKRESY
accuracies <- c(accuracy_tree, accuracy_lda, accuracy_knn3, accuracy_knn1, accuracy_naive) * 100
accuracies_prcnt <- paste("~", paste(format(accuracies, digits = 2, nsmall = 2), "%"))
classification_names <- c("Drzewo decyzyjne", "LDA", "kNN 3", "kNN 1", "Naive Bayes")
bp <- barplot(accuracies, main = "Dokladność procentowa klasyfikatorów", names.arg = classification_names, col = c("darkslategray4", "darkslategray4", "darkslategray3", "darkslategray2", "darkslategray1"), ylim = c(0,100))
text(x = bp, y = accuracies - 15, labels = accuracies_prcnt, pos = 3, cex = 1, col = "white")

### EWALUACJA KLASYFIKATOROW
#Funkcje pomocnicze
TP <- function(x) { #true positive - prawdziwie zaklasyfikowane jako pozytywne
  return(x[1,1])
}
FP <- function(x) { #false positive - fałszywie zaklasyfikowane jako pozytywne
  return(x[1,2])
}
TN <- function(x) { #true negative - prawdziwie zaklasyfikowane jako negatywne
  return(x[2,2])
}
FN <- function(x) { #false negative - fałszywie zaklasyfikowane jako negatywne
  return(x[2,1])
}
TPR <- function(x) { #odsetek prawdziwie pozytywnych (czułość)
  return (TP(x)/(TP(x)+FN(x)))
}
FPR <- function(x) { #odsetek fałszywie pozytywnych
  return (FP(x)/(FP(x)+TN(x)))
}
TNR <- function(x) { #odsetek prawdziwie negatywnych (swoistość)
  return (TN(x)/(TN(x)+FP(x)))
}
FNR <- function(x) { #odsetek fałszywie negatywnych
  return (FN(x)/(FN(x)+TP(x)))
}

#Testowanie dla naszych danych
crs <- data.frame("Drzewo decyzyjne", FPR(cm_tree), TPR(cm_tree)) #cr - classification ratings
names(crs) <- c("Name", "FPR", "TPR")
crs <- rbind(crs, data.frame("Name" = "LDA", "FPR" = FPR(cm_lda), "TPR" = TPR(cm_lda)))
crs <- rbind(crs, data.frame("Name" =  "kNN3", "FPR" = FPR(cm_knn3),"TPR" = TPR(cm_knn3)))
crs <- rbind(crs, data.frame("Name" =  "kNN1", "FPR" = FPR(cm_knn1),"TPR" = TPR(cm_knn1)))
crs <- rbind(crs, data.frame("Name" = "Naive Bayes", "FPR" = FPR(cm_naive), "TPR" = TPR(cm_naive)))
crs <- rbind(crs, data.frame("Name" = "Ideal", "FPR" = 0, "TPR" = 1))

#Wykres
colors <- c("red","blue","yellow", "green", "orange" , "black")
plot(x = crs$FPR, y = crs$TPR, col = colors[unique(crs$Name)], pch=19,
     xlab = "False Positive Rate", ylab = "True Positive Rate",ylim = c(0, 1), xlim=c(0,1), main = "ROC - ocena jakości klasyfikacji")
grid(nx = NULL, ny = NULL, col = "black", lty="dotted", lwd = par("lwd"), equilogs = TRUE)

## GRUPOWANIE METODA K-SREDNICH
data.log <- log(data.norm[,1:12])
data.log <- data.log[is.finite(rowSums(data.log)),]

data.scale <- scale(data.log, center=TRUE)
data.pca <- prcomp(data.scale)
data.kmeans <- predict(data.pca)

km_2 <- kmeans(data.kmeans, 2, iter.max = 100, algorithm = c("Lloyd"), trace=FALSE)
km_3 <- kmeans(data.kmeans, 3, iter.max = 100, algorithm = c("Lloyd"), trace=FALSE)
km_4 <- kmeans(data.kmeans, 4, iter.max = 100, algorithm = c("Lloyd"), trace=FALSE)

plot(data.kmeans, col=colors[km_2$cluster], main = "Grupowanie k średnich (2 klastry)")
points(km_2$centers, pch=10, cex=3, lwd=3)
legend(x="bottomleft", legend = unique(km_2$cluster),col=colors[unique(km_2$cluster)], pch=1)

plot(data.kmeans, col=colors[km_3$cluster], main = "Grupowanie k średnich (3 klastry)")
points(km_3$centers, pch=10, cex=3, lwd=3)
legend(x="bottomleft", legend = unique(km_3$cluster),col=colors[unique(km_3$cluster)], pch=1)

plot(data.kmeans, col=colors[km_4$cluster], main = "Grupowanie k średnich (4 klastry)")
points(km_4$centers, pch=10, cex=3, lwd=3)
legend(x="bottomleft", legend = unique(km_4$cluster),col=colors[unique(km_4$cluster)], pch=1)

plot(data.kmeans, col=colors[data$Ocena], main = "Oryginalny podział danych")
legend(x="bottomleft", legend = unique(data$Ocena),col=colors[unique(data$Ocena)], pch=1)

### REGULY ASOCJACYJNE
#Liczenie średnich dla kolumn
ruleData <- data[,1:13]

Padanie_miednicy.avg <- mean(ruleData$Padanie_miednicy)
Pochylenie_miednicy.avg <- mean(ruleData$Pochylenie_miednicy)
Lordoza_lędźwiowa.avg <- mean(ruleData$Lordoza_lędźwiowa)
Nachylenie_kości_krzyżowej.avg <- mean(ruleData$Nachylenie_kości_krzyżowej)
Kąt_ułożenia_miednicy.avg <- mean(ruleData$Kąt_ułożenia_miednicy)
Stopień_zwyrodnienia.avg <- mean(ruleData$Stopień_zwyrodnienia)
Zbocze_miednicy.avg <- mean(ruleData$Zbocze_miednicy)
Kierunkowe_pochylenie.avg <- mean(ruleData$Kierunkowe_pochylenie)
Nachylenie_piersiowego.avg <- mean(ruleData$Nachylenie_piersiowego)
Pochylenie_szyjnego.avg <- mean(ruleData$Pochylenie_szyjnego)
Kąt_kości_krzyżowej.avg <- mean(ruleData$Kąt_kości_krzyżowej)
Nachylenie_boczne.avg <- mean(ruleData$Nachylenie_boczne)

avg_names <-c("Padanie_miednicy", "Pochylenie_miednicy", "Lordoza_lędźwiowa", "Nachylenie_kości_krzyżowej", 
              "Kąt_ułożenia_miednicy", "Stopień_zwyrodnienia", "Zbocze_miednicy", "Kierunkowe_pochylenie", 
              "Nachylenie_piersiowego", "Pochylenie_szyjnego", "Kąt_kości_krzyżowej", "Nachylenie_boczne")
avg_values <- c(Padanie_miednicy.avg, Pochylenie_miednicy.avg, Lordoza_lędźwiowa.avg, 
                Nachylenie_kości_krzyżowej.avg, Kąt_ułożenia_miednicy.avg, Stopień_zwyrodnienia.avg, 
                Zbocze_miednicy.avg, Kierunkowe_pochylenie.avg, Nachylenie_piersiowego.avg, 
                Pochylenie_szyjnego.avg, Kąt_kości_krzyżowej.avg, Nachylenie_boczne.avg)


for (column in 1:12) {
  for (row in 1:nrow(ruleData)) {
    if(ruleData[row,][column] > avg_values[column]) {
      ruleData[row,][column] <- "Powyżej średniej"
    }
    if(ruleData[row,][column] <= avg_values[column]) {
      ruleData[row,][column] <- "Poniżej średniej"
    }
  }
}



ruleData$Padanie_miednicy <- factor(ruleData$Padanie_miednicy)
ruleData$Pochylenie_miednicy <- factor(ruleData$Pochylenie_miednicy)
ruleData$Lordoza_lędźwiowa <- factor(ruleData$Lordoza_lędźwiowa)
ruleData$Nachylenie_kości_krzyżowej <- factor(ruleData$Nachylenie_kości_krzyżowej)
ruleData$Kąt_ułożenia_miednicy <- factor(ruleData$Kąt_ułożenia_miednicy)
ruleData$Stopień_zwyrodnienia <- factor(ruleData$Stopień_zwyrodnienia)
ruleData$Zbocze_miednicy <- factor(ruleData$Zbocze_miednicy)
ruleData$Kierunkowe_pochylenie <- factor(ruleData$Kierunkowe_pochylenie)
ruleData$Nachylenie_piersiowego <- factor(ruleData$Nachylenie_piersiowego)
ruleData$Pochylenie_szyjnego <- factor(ruleData$Pochylenie_szyjnego)
ruleData$Kąt_kości_krzyżowej <- factor(ruleData$Kąt_kości_krzyżowej)
ruleData$Nachylenie_boczne <- factor(ruleData$Nachylenie_boczne)

rules <- apriori(ruleData ,parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Ocena=1", "Ocena=0"),default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
for( i in 1:10){
  inspect(rules.sorted[i])
}

for( i in 1:10){
  inspect(rules[i])
}



