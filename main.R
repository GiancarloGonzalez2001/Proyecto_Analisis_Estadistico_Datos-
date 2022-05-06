# cargar paquete readr
library(readxl)
winequality_red <- read_excel("winequality-red.xlsx")
View(winequality_red)  

winequality_white <- read_excel("winequality-white.xlsx")
View(winequality_white) 

# Creamos tres grupos de acuerdo a la calidad de vino tinto 
winequality_red['quality'][winequality_red['quality'] == 3 | winequality_red['quality'] == 4] <- 0
winequality_red['quality'][winequality_red['quality'] == 5 | winequality_red['quality'] == 6] <- 1
winequality_red['quality'][winequality_red['quality'] == 7 | winequality_red['quality'] == 8] <- 2

# Creamos tres grupos de acuerdo a la calidad de vino blanco 
winequality_white['quality'][winequality_white['quality'] == 3 | winequality_white['quality'] == 4] <- 0
winequality_white['quality'][winequality_white['quality'] == 5 | winequality_white['quality'] == 6] <- 1
winequality_white['quality'][winequality_white['quality'] == 7 | winequality_white['quality'] == 8] <- 2




