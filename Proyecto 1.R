library(tidyverse)
library(psych)
library(ggthemes)

setwd("D:/Google Drive/UVG/V Semestre/Data Mining/Proyecto 1/Proyecto_Final_R")
train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE)

#factorizacion de numericas
train <- train %>%
  mutate(MSSubClass = as.factor(MSSubClass))

test <- test %>%
  mutate(MSSubClass = as.factor(MSSubClass))



#1.a. ¿Que tipos de datos identificamos?
str(train)



#1.b. ¿Cual es la distribucion de las variables univariadas?

#revision de todas las variables numericas
train_numericas <- train[,sapply(train, is.numeric)]
multi.hist(train_numericas, global = FALSE)

#histograma de numericas continuas
vector_continuas_numericas <- c(2:3,8:14,16,27:29,37)
train_continuas_numericas <- train_numericas[,vector_continuas_numericas]
multi.hist(train_continuas_numericas, global = FALSE, bcol = 'pink')

#Diagrama de barras de discretas
vector_discretas_numericas <- c(4:7,17:26,35:36)
train_discretas_numericas <- train_numericas[,vector_discretas_numericas]
par(mfrow=c(4,4))
for(c in colnames(train_discretas_numericas)) {
  barplot(table(train_discretas_numericas[, c]))
}



#1.c. Presentar por lo menos 3 tablas de contingencia que se relacionen con el target del caso.


#1.d. ¿Que relaciones identifican entre las variables que podran afectar en el precio de las casas?

