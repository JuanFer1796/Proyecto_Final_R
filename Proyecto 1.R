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

#Condiciones ordinales utilizadas por muchas variables 
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
                             "Irregular" = 1), #Se aproxima porque Reg e Irregular tenian casi todos los datos.
           LandSlope = sapply(LandSlope, switch,
                              "Gtl" = 0,
                              "Mod"  = 1,
                              "Sev" = 1), #Se aproxima tambien por cantidad de datos
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
                               "Sal" = 3), #Se aproxima por cantidad de datos
           FireplaceQu = ordinal_condition(FireplaceQu),
           GarageFinish = ifelse(GarageFinish == "Unf", 1,
                                 ifelse(GarageFinish == "RFn", 2,
                                        ifelse(GarageFinish == "Fin", 3, NA))),
           GarageQual = ordinal_condition(GarageQual),
           GarageCond = ordinal_condition(GarageCond))
}

train <- transform_ordinal(train)
test <- transform_ordinal(test)

#Se combinan y se eliminan columnas
combine_cols <- function(df) {
  df %>%
    mutate(BsmtFinSF = BsmtFinSF1 + BsmtFinSF2,
           BsmtFinRating = ifelse(BsmtFinSF2 > 0, 
                                  ((BsmtFinType1*BsmtFinSF1) + (BsmtFinType2*BsmtFinSF2)) / (BsmtFinSF1 + BsmtFinSF2),
                                  BsmtFinType1),
           Porch = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch,
           FlrSF = X1stFlrSF + X2ndFlrSF) %>%
    select(-Id, -Utilities, -BsmtFinSF1, -BsmtFinSF2, -BsmtFinType1, -BsmtFinType2, -PoolQC, 
           -OpenPorchSF, -EnclosedPorch, -X3SsnPorch, -ScreenPorch)
}


train <- combine_cols(train)
test <- combine_cols(test)

#Funcion para convertir a bool 
bool_transform <- function(df) {
  df %>%
    mutate(Street = ifelse(Street == "Grvl",1,
                           ifelse(Street== "Pave", 0, NA)),
           Alley= ifelse(is.na(Alley), 0, 1),
           CentralAir= ifelse(CentralAir=="Y", 1, 
                              ifelse(CentralAir=="N",0, NA)),
           PoolArea= ifelse(PoolArea!=0, 1,
                            ifelse(PoolArea==0, 0, NA))
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

#se crea un nuevo df para los numericos de la tabla 
train_numericas2 <- train[,sapply(train, is.numeric)]
multi.hist(train_numericas2, global = FALSE)

#se encuentran los continuos
vector_continuas_numericas2 <- c(1, 2, 11, 17, 18, 21, 22, 24, 39, 42, 47, 48, 50, 51)
train_continuas_numericas2 <- train_numericas2[,vector_continuas_numericas2]

m <- cor(train_numericas2, use="complete.obs")

par(mfrow=c(1,1))
#con todas las variables y ordenadas por mayot cor 
corrplot(m, addCoef.col = 'black', number.cex= 0.5, tl.cex=0.5, cl.pos='n', 
          order = "FPC")

#solo con la variable saleprice para analizar las relaciones de las demas var con ella 
corrplot(m[c(1:46, 48:51), 47, drop=FALSE], addCoef.col = 'black', type = 'lower', number.cex= 0.5, tl.cex=0.5, cl.pos='n', 
         tl.srt = 1)

#df para discretas 
train_discretas_numericas2 <- train_numericas2[,-vector_continuas_numericas2]

#se juntan los cualitativos bool y los nominales
train_factor <- train[,sapply(train, is.factor)]
cualitativas_discretas <- cbind(train_discretas_numericas2, train_factor)

#se crean los boxplots 
par(mfrow=c(5,4))
for(c in colnames(cualitativas_discretas)) {
  boxplot(train$SalePrice ~ cualitativas_discretas[, c],
          main = c, col = rgb(1, 0, 0, alpha = 0.4))
}

num_high_cor <- c("OverralQual", "BsmtQual", "ExterQual", "GarageCars", "YearBuilt",
                  "KitchenQual", "TotalBsmtSF", "BsmtFinRating", "YearRemodAdd", "FlrSF",
                  "GrLivArea", "GarageFinish", "FullBath", "HeatingQC", "MasVnrArea",
                  "BsmtExposure")

cual_dif_sig <- c("MSZoning", "Neighborhood", "MasVnrType", "Electrical",
                  "GarageType", "SaleType")

#1.f.1. ¿Existe algun tipo de sesgo en una variable que pueda afectar el precio?
#Histogramas para las continuas
multi.hist(train_continuas_numericas2, global = FALSE, bcol = 'pink')

#Graficos de barras para las discretas y cualitativas
par(mfrow=c(5,6))
for(c in colnames(cualitativas_discretas)) {
  barplot(table(cualitativas_discretas[, c]), main = c)
}


#1.f.2. ¿Cuantos NAs hay en cada variable? ¿Pueden afectar al modelo?
train_og <- read.csv("train.csv", stringsAsFactors = TRUE)
colSums(is.na(train_og))
colSums(is.na(train))

#1.f.3. ¿Existen variables con relacion de mayor grado?
par(mfrow=c(4,4))
for(c in colnames(train_numericas2)) {
  plot(train_numericas2[, c], train_numericas2$SalePrice, main = c)
}

#1.f.4. ¿Cuales se deben eliminar debido a la multicolinealidad?
par(mfrow=c(1,1))
corrplot(m, addCoef.col = 'black', number.cex= 0.5, tl.cex=0.5, cl.pos='n', 
         order = "FPC")

#1.f.5. ¿Que variables es necesario dummificar?
par(mfrow=c(5,4))
for(c in colnames(cualitativas_discretas)) {
  boxplot(train$SalePrice ~ cualitativas_discretas[, c],
          main = c, col = rgb(1, 0, 0, alpha = 0.4))
}