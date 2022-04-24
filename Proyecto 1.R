library(tidyverse)
library(psych)
library(ggthemes)
library(corrplot)

setwd("D:/Google Drive/UVG/V Semestre/Data Mining/Proyecto 1/Proyecto_Final_R")
setwd("~/GitHub/Proyecto_Final_R")

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
  barplot(table(train_discretas_numericas[, c]), main=c)
}


#1.c. Presentar por lo menos 3 tablas de contingencia que se relacionen con el target del caso.
#Transformacion de datos 

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
#funcion para convertir a bool 
bool_transform <- function(df) {
  df %>%
    mutate(Street = ifelse(Street == "Grvl",1,
                           ifelse(Street== "Pave", 0, NA)),
           Alley= ifelse(Alley== "Grvl",1,
                         ifelse(Alley== "Pave", 0, NA)),
           CentralAir= ifelse(CentralAir=="Y", 1, 
                              ifelse(CentralAir=="N",0, NA)),
           PoolArea= ifelse(PoolArea!=0, 1,
                            ifelse(PoolArea==0, 0, NA)),
           MiscVal= ifelse(MiscVal!=0, 1,
                            ifelse(MiscVal==0, 0, NA))
           
    )
}

train <- bool_transform(train)
test <- bool_transform(test)


#Tres tablas de contingencia 
#estilo de casa y tipo de venta 
table(train$HouseStyle, train$SaleType)

#tipo de estilo y zona de la ciudad 
table(train$MSSubClass, train$MSZoning)

#Tipo de foundation y tipo de techo 
table(train$RoofStyle, train$Foundation)


#1.d. ¿Que relaciones identifican entre las variables que podran afectar en el precio de las casas?

#Condiciones usadas por muchas variables ordinales

#se crea un nuevo df para los numericos de la tabla 
train_numericas2 <- train[,sapply(train, is.numeric)]
multi.hist(train_numericas2, global = FALSE)

#se encuentran los continuos y ordinales 
vector_continuas_numericas2 <- c(1:2,7:21,23:31,33:44,47:51)
train_continuas_numericas2 <- train_numericas2[,vector_continuas_numericas2]
multi.hist(train_continuas_numericas2, global = FALSE, bcol = 'pink')

m<-cor(train_continuas_numericas2, use="complete.obs")

#con todas las variables y ordenadas por mayot cor 
corrplot(m, addCoef.col = 'black', type = 'lower', number.cex= 0.5, tl.cex=0.5, cl.pos='n', 
         tl.srt = 1, order = "FPC")

#solo con la variable saleprice para analizar las relaciones de las demas var con ella 
corrplot(m[1:43, 41, drop=FALSE], addCoef.col = 'black', type = 'lower', number.cex= 0.5, tl.cex=0.5, cl.pos='n', 
         tl.srt = 1)


#df para discretas 
train_discretas_numericas2 <- train_numericas2[,-vector_continuas_numericas2]


#se juntan los cualitativos bool y los nominales
train_factor <- train[,sapply(train, is.factor)]
cualitativas_nominales<- cbind(train_discretas_numericas2,train_factor)

#barplots2
par(mfrow=c(6,6))
for(c in colnames(cualitativas_nominales)) {
  barplot(table(cualitativas_nominales[, c]), main = c)
}


#se crean los boxplots 
par(mfrow=c(4,3))
for(c in colnames(cualitativas_nominales[,1:12])) {
  boxplot(cualitativas_nominales[, c], main = c,col = rgb(1, 0, 0, alpha = 0.4))
    }

par(mfrow=c(4,3))
for(c in colnames(cualitativas_nominales[,13:23])) {
  boxplot(cualitativas_nominales[, c], main = c, global=FALSE,col = rgb(1, 0, 0, alpha = 0.4))
}
