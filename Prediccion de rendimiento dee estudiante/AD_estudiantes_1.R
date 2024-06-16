#Instalar librerías para Árboles de Decisión

library(readxl)
library(rpart)
library(rpart.plot)

#Importar conjunto de datos. 
archivo <- file.choose()
student_mat_diplomado <- read_excel(archivo)
head(student_mat_diplomado)
datos<-subset(student_mat_diplomado,select = -G3)
attach(datos)

# Partición en conjunto de entrenamiento y prueba.
set.seed(101)
muestra<-nrow(datos)
tamaño.entrenamiento<-round(muestra*0.7)
datos.indice<-sample(1:muestra,size = tamaño.entrenamiento)
datos.entrenamiento<-datos[datos.indice,]
datos.test<-datos[-datos.indice,]

#Generando un árbol de clasificación
mi_arbol<-rpart(fail~.,data = datos.entrenamiento, method = "class")

#Salidas
rpart.plot(mi_arbol)
plotcp(mi_arbol)

#Poda del arbol
mi_arbol2<-prune(mi_arbol,cp=0.13)
rpart.plot(mi_arbol2)

#Comprobación en conjunto de prueba
prediccion<-predict(mi_arbol2,datos.test,type = "class")

#matriz de confusión y métricas
mc<-table(datos.test$fail,prediccion)
accuracy<-sum(diag(mc))/nrow(datos.test)
sensitivity<-mc[1,1]/sum(mc[1,])
specificity<-mc[2,2]/sum(mc[2,])
métricas<-data.frame(accuracy,sensitivity,specificity)
mc
métricas
