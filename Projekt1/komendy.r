library(genalg)

# Wczytanie pliku jako matrix do zmiennej values
values <- read.table("/home/dcwik/Pulpit/InteligencjaObliczeniowa/Projekt1/wartosci.cnf", header = FALSE)
values <- values[,-4]
# Konwersja tablicy na liczbowa
zmiana <- mapply(values, FUN=as.numeric)
values <- matrix(data = zmiana, ncol = 3, nrow="iloscWierszy")
# Funkcja fitness nagradzająca
fitnessFunc <- function(chromosome) {
    counter = 0
    result = FALSE
    for(i in 1:nrow(values)){
        row <- values[i,]
        row_1 <- abs(row[1])
        row_2 <- abs(row[2])
        row_3 <- abs(row[3])

        x1 = chromosome[row_1]
        if(row[1] < 0){ x1 = !x1 }
        if(x1 == 1) {
            counter = counter +1
        }
        else {
            x2 = chromosome[row_2]
            if(row[2] < 0){ x2 = !x2 }
            if(x2 == 1) {
                counter = counter +1
            }
            else {
                x3 = chromosome[row_3]
                if(row[3] < 0){ x3 = !x3 }
                if(x3 == 1) {
                    counter = counter +1
                }
            }
        }
    }
    return(-counter)
}

# Funkcja fitness z karami
fitnessFunc <- function(chromosome) {
    counter = 0
    result = FALSE
    for(i in 1:nrow(values)){
        row <- values[i,]
        row_1 <- abs(row[1])
        row_2 <- abs(row[2])
        row_3 <- abs(row[3])
        
        x1 = chromosome[row_1]
        if(row[1] < 0){ x1 = !x1 }
        x2 = chromosome[row_2]
        if(row[2] < 0){ x2 = !x2 }
        x3 = chromosome[row_3]
        if(row[3] < 0){ x3 = !x3 }
        
        result = x1 | x2 | x3
        if(!result){ 
            counter = counter + 1 
        } 
    }
    return(counter)
}

# Uruchomienie algorytmu
GAmodel <- rbga.bin(size = "iloscZmiennyc", popSize = 50, iters = 100, mutationChance = 0.01, elitism = T, evalFunc = fitnessFunc)

# Sprawdzenie czasu algorytmu
time <- system.time(rbga.bin(size = "iloscZmiennyc", popSize = 50, iters = 100,
    mutationChance = 0.01, elitism = T, evalFunc = fitnessFunc))


