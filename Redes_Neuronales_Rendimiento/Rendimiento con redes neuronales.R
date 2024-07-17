#Importar datos (e.g. rendimiento educativo): Environment > Import dataset > From Excel> student_mat_diplomado

#Instalar librería "neuralnet" y ejecutarla

library(neuralnet)

#Partición de datos en conjunto de entrenamiento y de prueba

datos<-student_mat_diplomado
set.seed(55)
muestra<-nrow(datos)
tamaño.entrenamiento<-round(muestra*0.7)
datos.indice<-sample(1:muestra,size = tamaño.entrenamiento)
datos.entrenamiento<-datos[datos.indice,]
datos.test<-datos[-datos.indice,]

#Generación de ANN a partir de conjunto de entrenamiento

red_neuronal<-neuralnet(datos.entrenamiento$fail~.,data = datos.entrenamiento,hidden = 5, act.fct = "logistic")

#Comprobación en conjunto de prueba

results<-predict(red_neuronal,datos.test)
prediccion<-ifelse(results>0.5,1,0)

#Generar matriz de confusión y principales métricas de evaluación
mc<-table(datos.test$fail,prediccion)
accuracy<-sum(diag(mc))/nrow(datos.test)
sensitivity<-mc[1,1]/sum(mc[1,])
specificity<-mc[2,2]/sum(mc[2,])
métricas<-data.frame(accuracy,sensitivity,specificity)

#Salidas
mc
métricas
plot(red_neuronal)

# Instalar librería para curva ROC
library(ROCR)

#Generar Curva ROC
pred<-prediction(prediccion,datos.test$fail)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0, b= 1)
roc_auc<-performance(pred,measure = "auc")
roc_auc@y.values
