# cargar paquete readr
library(readxl)
library(ggplot2)

# instalar ggplot2 
#install.packages("ggplot2", dependencies=TRUE)

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

#Selecionamos los datos de interes
data_red<-winequality_red[,0:11]
data_white<-winequality_white[,0:11]

#Normalizamos datos de interes para evitar datos muy atipicos
data_red <- as.data.frame (scale (data_red [0: 11]))
data_white <- as.data.frame (scale (data_white[0: 11]))

# Apartamos unos datos para entrenamiento 
train_red=data_red[0:1120,]
train_white=data_white[0:3429,]


# Apartamos los datos de testeo
test_red=data_red[1121:1599,]
test_white=data_white[3430:4898,]

# objetivos entrenamiento 
targets_train_red=winequality_red[0:1120,12]
targets_train_white=winequality_white[0:3429,12]

# objetivos testeo
targets_test_red=winequality_red[1121:1599,12]
targets_test_white=winequality_white[3430:4898,12]

#======================================================================
#     AHORA SI EMPEZAMOS
#======================================================================

# Exploracion de las variables del vino tinto 

plot(data_red$`fixed acidity`, main = "Acidez fija")
plot(data_red$`volatile acidity`, main="Acidez volatil")
plot(data_red$`citric acid`, main="Acidez citrica")
plot(data_red$`residual sugar`,main = "Azucar residual")
plot(data_red$chlorides, main = "Cloruros")
plot(data_red$`free sulfur dioxide`,main = "Dioxido de azufre libre" )
plot(data_red$`total sulfur dioxide`, main = "Dioxido de azufre total")
plot(data_red$density,main = "Densidad")
plot(data_red$pH, main = "PH")
plot(data_red$sulphates,main = "Sulfato")
plot(data_red$alcohol,main = "Alcohol")

# Exploramos las variables del vino blanco
plot(data_white$`fixed acidity`, main = "Acidez fija")
plot(data_white$`volatile acidity`, main="Acidez volatil")
plot(data_white$`citric acid`, main="Acidez citrica")
plot(data_white$`residual sugar`,main = "Azucar residual")
plot(data_white$chlorides, main = "Cloruros")
plot(data_white$`free sulfur dioxide`,main = "Dioxido de azufre libre" )
plot(data_white$`total sulfur dioxide`, main = "Dioxido de azufre total")
plot(data_white$density,main = "Densidad")
plot(data_white$pH, main = "PH")
plot(data_white$sulphates,main = "Sulfato")
plot(data_white$alcohol,main = "Alcohol")

#================================================================================
# Usamos el criterio del codo para ver el numeor de clusters optimo 
#================================================================================

#vino tinto 
wcss_red <- vector()
for(i in 1:20){
  wcss_red[i] <- sum(kmeans(data_red, i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss_red), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss_red), color = 'blue') + 
  ggtitle("Método del Codo vino tinto") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')



#vino blanco
wcss_white <- vector()
for(i in 1:20){
  wcss_white[i] <- sum(kmeans(data_white, i)$withinss)
}

ggplot() + geom_point(aes(x = 1:20, y = wcss_white), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss_white), color = 'blue') + 
  ggtitle("Método del Codo vino blanco") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

#Usamos el la funcion kmeans
kmeans_red <- kmeans(train_red, 3, iter.max = 1000, nstart = 20)
kmeans_white <- kmeans(train_white, 3, iter.max = 1000, nstart = 20)

labels_red=kmeans_red$cluster
labels_white=kmeans_white$cluster

# etiquetamos los datos de testeo
#pre_red=kmeans.predict(kmeans_red,test_red)
#pre_white=kmeans.predict(kmeans_white,test_white)

#Matrices de confusion
wqr=winequality_red$quality
wqr=wqr[-1121:-length(wqr)]
cfmatrix_red = confusionMatrix(as.factor(labels_red),as.factor(wqr))
cfmatrix_red

wqw=winequality_white$quality
wqw=wqw[-3430:-length(wqw)]
cfmatrix_white=confusionMatrix(as.factor(labels_white),as.factor(wqw))
cfmatrix_white
# hacemos el PCA vino rojo 
pca.red <- prcomp(train_red,scale=FALSE)
plot(pca.red)

# hacemos PCA para blanco
pca.white <- prcomp(train_white, scale=FALSE)
plot(pca.white)

# graficamos las dos primeras componetes de vino rojo 
# calidad 1 color negro 
# calidad 2 color rosado 
#calidad 3 color verde
plot(pca.red$x[,1],pca.red$x[,2],
     col=labels_red,cex.axis=.7,cex.lab=.7)

# Graficamos las nos primeras componetes ed vino blanco
# calidad 1 color negro 
# calidad 2 color rosado 
#calidad 3 color verde
plot(pca.white$x[,1],pca.white$x[,2],
     col=labels_white,cex.axis=.7,cex.lab=.7)
