library(dplyr)
library(ggplot2)
library(psych)

train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE)

train <- train %>%
    mutate(MSSubClass = as.factor(MSSubClass))

test <- test %>%
    mutate(MSSubClass = as.factor(MSSubClass))

#1.a. ¿Qué tipos de datos identificamos?
str(train)

#1.b. ¿Cuál es la distribución de las variables univariadas?
multi.hist(train[,sapply(train, is.numeric)], global = FALSE)

ggplot(train, aes(x=LotArea)) +
    geom_boxplot()

#1.c. Presentar por lo menos 3 tablas de contingencia que se relacionen con el target del caso.
table(train$LotShape, train$PoolQC)

#1.d. ¿Qué relaciones identifican entre las variables que podrían afectar en el precio de las casas?
