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

#Condiciones usadas por muchas variables ordinales
ordinal_condition <- function(col) {
  ifelse(col == "Po", 1,
  ifelse(col == "Fa", 2,
  ifelse(col == "TA", 3,
  ifelse(col == "Gd", 4,
  ifelse(col == "Ex", 5, NA)))))
}

bsmt_condition <- function(col) {
  ifelse(col == "LwQ", 1,
  ifelse(col == "Rec", 2,
  ifelse(col == "BLQ", 3,
  ifelse(col == "ALQ", 4,
  ifelse(col == "GLQ", 5, NA)))))
}

#Limpieza de datos
transform_ordinal <- function(df) {
  df %>%
    mutate(LotShape = sapply(LotShape, switch,
                             "Reg" = 0,
                             "Slightly irregular" = 0,
                             "Moderately Irregular" = 1,
                             "Irregular" = 1),
           LandSlope = sapply(LandSlope, switch,
                              "Gtl" = 0,
                              "Mod"  = 1,
                              "Sev" = 1),
           ExterQual = ordinal_condition(ExterQual),
           ExterCond = ordinal_condition(ExterCond),
           BsmtQual = ordinal_condition(BsmtQual),
           BsmtCond = ordinal_condition(BsmtCond),
           BsmtExposure = ifelse(BsmtExposure == "No", 1,
                          ifelse(BsmtExposure == "Mn", 2,
                          ifelse(BsmtExposure == "Av", 3,
                          ifelse(BsmtExposure == "Gd", 4, NA)))),
           BsmtFinType1 = bsmt_condition(BsmtFinType1),
           BsmtFinType2 = bsmt_condition(BsmtFinType2),
           HeatingQC = ordinal_condition(HeatingQC),
           KitchenQual = ordinal_condition(KitchenQual),
           Functional = sapply(Functional, switch,
                               "Typ" = 1,
                               "Min1" = 2,
                               "Min2" = 2,
                               "Mod" = 2,
                               "Maj1" = 3,
                               "Maj2" = 3,
                               "Sev" = 3,
                               "Sal" = 3),
           FireplaceQu = ordinal_condition(FireplaceQu),
           GarageFinish = ifelse(GarageFinish == "Unf", 1,
                          ifelse(GarageFinish == "RFn", 2,
                          ifelse(GarageFinish == "Fin", 3, NA))),
           GarageQual = ordinal_condition(GarageQual),
           GarageCond = ordinal_condition(GarageCond))
}

train <- transform_ordinal(train)
test <- transform_ordinal(test)

combine_cols <- function(df) {
  df %>%
    mutate(BsmtFinSF = BsmtFinSF1 + BsmtFinSF2,
           Porch = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch) %>%
    select(-Id, -Utilities, -BsmtFinSF1, -BsmtFinSF2, -PoolQC, -OpenPorchSF, -EnclosedPorch, -X3SsnPorch, -ScreenPorch)
}

train <- combine_cols(train)
test <- combine_cols(test)