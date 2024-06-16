

#PAQUETES A SER UTILIZADOS 
install.packages("e1071")
install.packages("MASS")
install.packages("dplyr")
install.packages("readxl")
install.packages("caret")
install.packages("ROCR")





rm(list=ls())

library(e1071)
library(MASS)
library(dplyr)
library(readxl)
library(caret)
library(ROCR)

winequality_red <- read.csv("winequality_red.csv", sep=";")
preprocess_red <- preProcess(winequality_red[,1:11], c("BoxCox", "center", "scale"))
redwine <- data.frame(trans = predict(preprocess_red, winequality_red))

colnames(redwine)

# Veamos el desplazamientio de los datos transformados

skewness(redwine$trans.fixed.acidity)
skewness(redwine$trans.volatile.acidity)
skewness(redwine$trans.citric.acid)
skewness(redwine$trans.residual.sugar)
skewness(redwine$trans.chlorides)
skewness(redwine$trans.free.sulfur.dioxide)
skewness(redwine$trans.total.sulfur.dioxide)
skewness(redwine$trans.density)
skewness(redwine$trans.pH)
skewness(redwine$trans.sulphates)
skewness(redwine$trans.alcohol)
skewness(redwine$trans.quality)

colnames(redwine)



### Ahora removeremos los outliers

redwine <- redwine[!abs(redwine$trans.fixed.acidity) > 3,]
redwine <- redwine[!abs(redwine$trans.volatile.acidity) > 3,]
redwine <- redwine[!abs(redwine$trans.citric.acid) > 3,]
redwine <- redwine[!abs(redwine$trans.residual.sugar) > 3,]
redwine <- redwine[!abs(redwine$trans.chlorides) > 3,]
redwine <- redwine[!abs(redwine$trans.free.sulfur.dioxide) > 3,]
redwine <- redwine[!abs(redwine$trans.total.sulfur.dioxide) > 3,]
redwine <- redwine[!abs(redwine$trans.density) > 3,]
redwine <- redwine[!abs(redwine$trans.pH) > 3,]
redwine <- redwine[!abs(redwine$trans.sulphates) > 3,]
redwine <- redwine[!abs(redwine$trans.alcohol) > 3,]

colnames(redwine)


#Añadir la celda con variables categóricas
redwine <- mutate (redwine, trans.quality = ifelse (trans.quality <= 5, 0, 1))
View(redwine)


datos <- redwine



set.seed(99)
muestra<-nrow(datos)

tamaño.entrenamiento<-round(muestra*0.7)
datos.indice<-sample(1:muestra,size = tamaño.entrenamiento)
datos.entrenamiento<-datos[datos.indice,]
datos.test<-datos[-datos.indice,]

##########################################################################
#1 Generación de modelo SVM LINEAL a partir de conjunto de entrenamiento
##########################################################################


y_lin = as.factor(datos.entrenamiento$trans.quality)
modeloSVM_lin <- svm(y_lin~.,data = datos.entrenamiento, kernel = "linear", cost=10, scale = TRUE)


SVM_opt <-
  tune(
    svm,
    as.factor(trans.quality) ~ ., data = datos.entrenamiento,
    kernel = "linear",
    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5)), scale = FALSE)

summary(SVM_opt)

lin_opt = SVM_opt$best.model

#Comprobación en conjunto de prueba

yt_lin = as.factor(datos.test$trans.quality)
yp_lin = predict(lin_opt, datos.test)

#Matriz de confusión y principales métricas de evaluación
mc_lin <- table(yt_lin, yp_lin)
accuracy_lin <- sum(diag(mc_lin)) / nrow(datos.test)
sensitivity_lin <- mc_lin[1, 1] / sum(mc_lin[1, ])
specificity_lin <- mc_lin[2, 2] / sum(mc_lin[2, ])
métricas_lin <-
  data.frame(accuracy_lin, sensitivity_lin, specificity_lin)

summary(lin_opt)
table(yt_lin, yp_lin)
métricas_lin

#Generación de CURVA ROC
pred <-
  prediction(as.numeric(yp_lin), as.numeric(datos.test$trans.quality))
perf <- performance(pred, "tpr", "fpr")
plot(perf,
     colorize = TRUE,
     xlab = "Proporción de falsos positivos",
     ylab = "Proporción de verdaderos positivos")
abline(a = 0, b = 1)

#Area bajo la curva ROC
roc_auc <- performance(pred, measure = "auc")
roc_auc@y.values




#############################################################################

#2 Generación de modelo SVM RADIAL a partir de conjunto de entrenamiento
#############################################################################


y_rad = as.factor(datos.entrenamiento$trans.quality)
modeloSVM_rad <- svm(y_rad~.,data = datos.entrenamiento, kernel = "radial", gamma =1, cost=10, scale = TRUE)


SVMrad_opt <-
  tune(
    svm,
    as.factor(trans.quality) ~ .,
    data = datos.entrenamiento,
    kernel = "radial",
    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10),
                  gamma = c (0.5, 1, 2, 3, 4)),
    scale = TRUE )



summary(SVMrad_opt)


rad_opt = SVMrad_opt$best.model

#Comprobación en conjunto de prueba
yt_rad = as.factor(datos.test$trans.quality)
yp_rad = predict(rad_opt, datos.test)



#Matriz de confusión y principales métricas de evaluación
mc_rad <- table(yt_rad, yp_rad)
accuracy_rad <- sum(diag(mc_rad)) / nrow(datos.test)
sensitivity_rad <- mc_rad[1, 1] / sum(mc_rad[1, ])
specificity_rad <- mc_rad[2, 2] / sum(mc_rad[2, ])
métricas_rad <-
  data.frame(accuracy_rad, sensitivity_rad, specificity_rad)

summary(rad_opt)
table(yt_rad, yp_rad)
métricas_rad

#Generación de la curva ROC
pred_rad <-
  prediction(as.numeric(yp_rad), as.numeric(datos.test$trans.quality))
perf_rad <- performance(pred_rad, "tpr", "fpr")
plot(perf_rad,
     colorize = TRUE,
     xlab = "Proporción de falsos positivos",
     ylab = "Proporción de verdaderos positivos")
abline(a = 0, b = 1)

#Area bajo la curva ROC
roc_aucr <- performance(pred_rad, measure = "auc")
roc_aucr@y.values


