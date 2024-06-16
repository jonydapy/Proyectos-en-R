### Intento 5

###comando para borrar toda la memoria antes de comenzar
rm(list=ls())

## Instalar librerias necesarias
install.packages("rmdformats") 
install.packages("corrgram")
install.packages("MASS")
install.packages("ggplot2")
install.packages("naniar")
install.packages("e1071")
install.packages("lattice")
install.packages("caret")
install.packages("car")
install.packages("caTools")
install.packages("corrplot")
install.packages("knitr")
install.packages("reshape2")


## Activamos Conjunto de librerias

library("rmdformats") 
library("corrgram")
library("MASS")
library("ggplot2")
library("naniar")
library("e1071")
library("lattice")
library("caret")
library("car")
library("caTools")
library("corrplot")
library("knitr")
library("reshape2")

# Importamos el dataset

vino_rojo <- read.csv("winequality-red.csv", header = TRUE, sep = ";")

rojo <- vino_rojo


# Primer Vistazo a los datos importados

str(rojo)


# Verificamos los nombres de las columnas de los datos

colnames(rojo)

# Ver un resumen de los datos para mejor comprension del dataset

summary(rojo)

# Remover filas duplicadas

rojo <- rojo[!duplicated(rojo),]
dim(rojo)

# Verificamos si tenemos valores perdidos

vis_miss(rojo)
sum(is.na(rojo))

# Vamos a contar cuantos vinos por calidad hay

table(rojo$quality)

# Vamos a verificar la correlacion que existen entre las variables con el metodo pearson y usando 2 decimales

round(cor(rojo, method = "pearson"), 2) #este paso no se si es tan necesario pero ayuda a visualizar

# Graficamos la correlacion para poder identificarlos visualmente

corrplot(cor(rojo))

corrgram(rojo, type = "data", lower.panel = panel.conf, upper.panel = panel.shade, 
         main = "Grafico de Correlacion para Vinos Rojos", order = T, cex.labels = 1.2)

### Histograma por cada variable de la tabla

attach(rojo)

#grafico 1 con 4 histogramas
par(mfrow = c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

barplot((table(quality)), col = c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Calidad", side = 1, outer = F, line = 2, cex = 0.8)

truehist(fixed.acidity, h= 0.5, col = "slategray3")
mtext("Fixed Acidity", side = 1, outer = F, line = 2, cex = 0.8)

truehist(volatile.acidity, h= 0.05, col = "slategray3")
mtext("Volatile Acidity", side = 1, outer = F, line = 2, cex = 0.8)

truehist(citric.acid, h= 0.1, col = "slategray3")
mtext("Citric Acid", side = 1, outer = F, line = 2, cex = 0.8)

#grafico 2 con 4 histogramas
par(mfrow = c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(residual.sugar, h= 1, col = "slategray3")
mtext("Residual SUgar", side = 1, outer = F, line = 2, cex = 0.8)

truehist(chlorides, h= 0.01, col = "slategray3")
mtext("Chlorides", side = 1, outer = F, line = 2, cex = 0.8)

truehist(free.sulfur.dioxide, h= 10, col = "slategray3")
mtext("Free Sulfur Dioxide", side = 1, outer = F, line = 2, cex = 0.8)

truehist(total.sulfur.dioxide, h= 20, col = "slategray3")
mtext("Total Sulfur Dioxide", side = 1, outer = F, line = 2, cex = 0.8)

#grafico 3 con 4 histogramas
par(mfrow = c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(density, h= 0.001, col = "slategray3")
mtext("Density", side = 1, outer = F, line = 2, cex = 0.8)

truehist(pH, h= 0.1, col = "slategray3")
mtext("pH", side = 1, outer = F, line = 2, cex = 0.8)

truehist(sulphates, h= 0.1, col = "slategray3")
mtext("Sulphates", side = 1, outer = F, line = 2, cex = 0.8)

truehist(alcohol, h= 0.5, col = "slategray3")
mtext("Alcohol", side = 1, outer = F, line = 2, cex = 0.8)

### Bigote - box plot

#grafico 1 bigote

par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(fixed.acidity, col = "slategray2", pch = 19)
mtext("Fixed Acidity", cex = 0.8, side = 1, line = 2)

boxplot(volatile.acidity, col = "slategray2", pch = 19)
mtext("Volatile Acidity", cex = 0.8, side = 1, line = 2)

boxplot(citric.acid, col = "slategray2", pch = 19)
mtext("Citric Acid", cex = 0.8, side = 1, line = 2)

boxplot(residual.sugar, col = "slategray2", pch = 19)
mtext("Residual Sugar", cex = 0.8, side = 1, line = 2)

boxplot(chlorides, col = "slategray2", pch = 19)
mtext("Chlorides", cex = 0.8, side = 1, line = 2)

#grafico 2 bigote

par(mfrow=c(1,6), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(free.sulfur.dioxide, col = "slategray2", pch = 19)
mtext("Free Sulful Diox", cex = 0.8, side = 1, line = 2)

boxplot(total.sulfur.dioxide, col = "slategray2", pch = 19)
mtext("Total Sulfur Diox", cex = 0.8, side = 1, line = 2)

boxplot(density, col = "slategray2", pch = 19)
mtext("Density", cex = 0.8, side = 1, line = 2)

boxplot(pH, col = "slategray2", pch = 19)
mtext("PH", cex = 0.8, side = 1, line = 2)

boxplot(sulphates, col = "slategray2", pch = 19)
mtext("Sulphates", cex = 0.8, side = 1, line = 2)

boxplot(alcohol, col = "slategray2", pch = 19)
mtext("Alcohol", cex = 0.8, side = 1, line = 2)

detach(rojo)

###
# Aca ya nos damos cuenta que tenemos outliers en todas las variables
###


## Desplazamiento

attach(rojo)

skewness(quality)
skewness(fixed.acidity)
skewness(volatile.acidity)
skewness(citric.acid)
skewness(residual.sugar)
skewness(chlorides)
skewness(free.sulfur.dioxide)
skewness(total.sulfur.dioxide)
skewness(density)
skewness(pH)
skewness(sulphates)
skewness(alcohol)

colnames(rojo)

detach(rojo)

# Reglas a tener en cuenta:
# si el valor de desplazamiento es 0 es perfectamente simetrico
# Si el valor es menor a -1 o mayor a 1, la distribucion esta muy desplazada
# Si el valor esta entre -0.5 y 0.5 es aproximadamente simetrica


# Transformacion y Preparacion de Datos

# Usamos boxcox

preprocess_rojo <- preProcess(rojo[,1:11], c("BoxCox", "center", "scale"))
nuevo_rojo <- data.frame(trans = predict(preprocess_rojo, rojo))

colnames(nuevo_rojo)

# Veamos el desplazamientio de los datos transformados

skewness(nuevo_rojo$trans.fixed.acidity)
skewness(nuevo_rojo$trans.volatile.acidity)
skewness(nuevo_rojo$trans.citric.acid)
skewness(nuevo_rojo$trans.residual.sugar)
skewness(nuevo_rojo$trans.chlorides)
skewness(nuevo_rojo$trans.free.sulfur.dioxide)
skewness(nuevo_rojo$trans.total.sulfur.dioxide)
skewness(nuevo_rojo$trans.density)
skewness(nuevo_rojo$trans.pH)
skewness(nuevo_rojo$trans.sulphates)
skewness(nuevo_rojo$trans.alcohol)
skewness(nuevo_rojo$trans.quality)

colnames(nuevo_rojo)


# con este proceso redujimos/corregimos el desplazamiento


### Ahora removeremos los outliers

nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.fixed.acidity) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.volatile.acidity) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.citric.acid) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.residual.sugar) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.chlorides) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.free.sulfur.dioxide) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.total.sulfur.dioxide) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.density) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.pH) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.sulphates) > 3,]
nuevo_rojo <- nuevo_rojo[!abs(nuevo_rojo$trans.alcohol) > 3,]

colnames(nuevo_rojo)

# Volvemos a verificar la correlacion e intervalo de confianza con corrplot

corrplot(cor(nuevo_rojo), type = "lower")

M <- cor(nuevo_rojo)
corrplot(M, type = "lower")

# Si sale algun error se debe ejecutar este comando en la terminal {plot.new(); dev.off()}

corrgram(nuevo_rojo, type = "data", lower.panel = panel.conf, upper.panel = panel.shade, 
         main = "Corrgram for wine quality", order = T, cex.labels = 1.1)

##########################
### REGRESION LOGISTICA###
##########################

# Aqui uso los datos tratados

# Creamos la comlumna para la parte binomial con 0 y 1

nuevo_rojo$category[nuevo_rojo$trans.quality <= 5] <- 0
nuevo_rojo$category[nuevo_rojo$trans.quality > 5] <- 1

nuevo_rojo$category <- as.factor(nuevo_rojo$category)

# Split de datos para test y pruebas

set.seed(1000)
spl = sample.split(nuevo_rojo$category, SplitRatio = 0.7)

rojo_entrenamiento = subset(nuevo_rojo, spl == TRUE)
rojo_test = subset(nuevo_rojo, spl == FALSE)

head(rojo_entrenamiento)

modelo_glm = glm(category ~ . - trans.quality, data = rojo_entrenamiento, family = "binomial"(link = "logit"))

# Stepwise

model_gl <- step(modelo_glm)


# Prediccion - entrenamiento

head(fitted(model_gl))

head(predict(model_gl))

head(predict(model_gl, type = "response"))

# Con categorizacion

trn_pred <- ifelse(predict(model_gl, type = "response") > 0.5, "Buen Vino", "Mal Vino")
head(trn_pred)


# Matriz de confusion - Entrenamiento

#tenemos que hacer una matriz de confusion a partir de nuestros datos

trn_tab <- table(predicted = trn_pred, actual = rojo_entrenamiento$category)
trn_tab

# Accuracy del set de entrenamiento

sum(diag(trn_tab))/length(rojo_entrenamiento$category)

# Matriz de confusion con test

tst_pred <- ifelse(predict(model_gl, newdata = rojo_test, type = "response") > 0.5, "Buen Vino", "Mal Vino")
tst_tab <- table(predicted = tst_pred, actual = rojo_test$category)
tst_tab

# Accuracy del set de test

sum(diag(tst_tab))/length(rojo_test$category)

############
##########
######
##

# Aqui uso los datos crudos

# Creamos la comlumna para la parte binomial con 0 y 1

nuevo_rojo$category[nuevo_rojo$trans.quality <= 5] <- 0
nuevo_rojo$category[nuevo_rojo$trans.quality > 5] <- 1

nuevo_rojo$category <- as.factor(nuevo_rojo$category)

# Split de datos para test y pruebas

set.seed(1000)
spl = sample.split(nuevo_rojo$category, SplitRatio = 0.7)

rojo_entrenamiento = subset(nuevo_rojo, spl == TRUE)
rojo_test = subset(nuevo_rojo, spl == FALSE)

head(rojo_entrenamiento)

modelo_glm = glm(category ~ . - trans.quality, data = rojo_entrenamiento, family = "binomial"(link = "logit"))

# Stepwise

model_gl <- step(modelo_glm)


# Prediccion - entrenamiento

head(fitted(model_gl))

head(predict(model_gl))

head(predict(model_gl, type = "response"))

# Con categorizacion

trn_pred <- ifelse(predict(model_gl, type = "response") > 0.5, "Buen Vino", "Mal Vino")
head(trn_pred)


# Matriz de confusion - Entrenamiento

#tenemos que hacer una matriz de confusion a partir de nuestros datos

trn_tab <- table(predicted = trn_pred, actual = rojo_entrenamiento$category)
trn_tab

# Accuracy del set de entrenamiento

sum(diag(trn_tab))/length(rojo_entrenamiento$category)

# Matriz de confusion con test

tst_pred <- ifelse(predict(model_gl, newdata = rojo_test, type = "response") > 0.5, "Buen Vino", "Mal Vino")
tst_tab <- table(predicted = tst_pred, actual = rojo_test$category)
tst_tab

# Accuracy del set de test

sum(diag(tst_tab))/length(rojo_test$category)




#######
########

# con lo que se dio en clase
library(ISLR)

final <- glm(nuevo_rojo$category ~ nuevo_rojo$trans.fixed.acidity + nuevo_rojo$trans.volatile.acidity + 
               nuevo_rojo$trans.citric.acid + nuevo_rojo$trans.residual.sugar + nuevo_rojo$trans.chlorides + 
               nuevo_rojo$trans.free.sulfur.dioxide + nuevo_rojo$trans.total.sulfur.dioxide + 
               nuevo_rojo$trans.density + nuevo_rojo$trans.pH + nuevo_rojo$trans.sulphates + 
               nuevo_rojo$trans.alcohol, family = "binomial")
summary(final)

fit.pred = ifelse(final$fitted.values > 0.5, 1, 0)
table(fit.pred, nuevo_rojo$category)


#######
#### CLASIFICACION
#######

#rojo_clas <- subset(nuevo_rojo, select = -category)
rojo_clas <- subset(nuevo_rojo, select = -trans.quality)
head(rojo_clas)

library(rpart)
library(rpart.plot)

set.seed(1000)
muestra <- nrow(rojo_clas)
tamano.entrenamiento <- round(muestra*0.7)
rojo_clas.indice <- sample(1:muestra, size = tamano.entrenamiento)
rojo_clas.entrenamiento <- rojo_clas[rojo_clas.indice,]
rojo_clas.test <- rojo_clas[-rojo_clas.indice,]

arbolito <- rpart(category ~ ., data = rojo_clas.entrenamiento, method = "class")
rpart.plot(arbolito)

plotcp(arbolito)

arbolito_2 <-  prune(arbolito, cp = 0.028)
rpart.plot(arbolito_2)

prediccion <- predict(arbolito_2, rojo_clas.test, type = "class")

str(prediccion)

mc <- table(rojo_clas.test$category, prediccion)
accuracy <- sum(diag(mc))/nrow(rojo_clas.test)
sensitivity <- mc[1,1] / sum(mc[1,])
specificity <- mc[2,2] / sum(mc[2,])

resultado <- data.frame(accuracy, sensitivity, specificity)
mc
resultado

library(ROCR)
pred <- prediction(as.numeric(prediccion), as.numeric(rojo_clas.test$category))
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
abline(a=0, b=1)

roc_auc <- performance(pred, measure = "auc")
roc_auc@y.values
