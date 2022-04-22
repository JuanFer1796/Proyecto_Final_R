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

#revision de todas las variables
multi.hist(train[,sapply(train, is.numeric)], global = FALSE)

#histograma de numericas continuas
train_numericas <- train[,sapply(train, is.numeric)]
vector_continuas_numericas <- c(2:3,8:14,16,27:29,37)
train_continuas_numericas <- train_numericas[,vector_continuas_numericas]
multi.hist(train_continuas_numericas, global = FALSE, bcol = 'pink')

#Diagrama de barras de discretas
vector_discretas_numericas <- c(4:7,17:26,35:36)
train_discretas_numericas<- train_numericas[,vector_discretas_numericas]

barplot(as.matrix(train_discretas_numericas[,5:10]),
        main="Multiple Bar Plots",
        
        # setting y label only
        # because x-label will be our
        # barplots name
        ylab="Count",
        
        # to plot the bars vertically
        beside=TRUE,
)

par()

#1.a. ¿Qué tipos de datos identificamos?
str(train)

#1.b. ¿Cuál es la distribución de las variables univariadas?

ggplot(train, aes(x=LotArea)) +
    geom_boxplot()

#1.c. Presentar por lo menos 3 tablas de contingencia que se relacionen con el target del caso.
table(train$LotShape, train$PoolQC)

#1.d. ¿Qué relaciones identifican entre las variables que podrían afectar en el precio de las casas?

