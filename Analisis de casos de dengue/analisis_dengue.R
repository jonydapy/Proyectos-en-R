#Importar datos
datos<-Datos_dengue

#Instalar  librerías
library(e1071)
library(MASS, lib.loc = "C:/Program Files/R/R-4.2.1/library")


#Partición de datos
set.seed(101)
muestra<-nrow(datos)
tamaño.entrenamiento<-round(muestra*0.7)
datos.indice<-sample(1:muestra,size = tamaño.entrenamiento)
datos.entrenamiento<-datos[datos.indice,]
datos.test<-datos[-datos.indice,]
y=as.factor(datos.entrenamiento$Clasif.Final)

#Generación de modelo SVM a partir de conjunto de entrenamiento. 
modeloSVM<-svm(y~.,data = datos.entrenamiento,kernel="linear", cost=10,cross=5)

#Comprobación en conjunto de prueba
yt=as.factor(datos.test$Clasif.Final)
yp=predict(modeloSVM,datos.test)

#Evaluación. Matriz de Confusión y Métricas
mc<-table(yt,yp)
accuracy<-sum(diag(mc))/nrow(datos.test)
sensitivity<-mc[1,1]/sum(mc[1,])
specificity<-mc[2,2]/sum(mc[2,])
métricas<-data.frame(accuracy,sensitivity,specificity)

#Salidas
summary(modeloSVM)
table(yt,yp)
métricas
