#Instalar librerías para Árboles de Decisión

library(readxl)
library(rpart)
library(rpart.plot)

#Importar conjunto de datos. 
archivo <- file.choose()
student_mat_diplomado <- read_excel(archivo)
head(student_mat_diplomado)
datos<-subset(student_mat_diplomado,select = -fail)
attach(datos)

# Partición en conjunto de entrenamiento y prueba.
set.seed(101)
muestra<-nrow(datos)
tamaño.entrenamiento<-round(muestra*0.7)
datos.indice<-sample(1:muestra,size = tamaño.entrenamiento)
datos.entrenamiento<-datos[datos.indice,]
datos.test<-datos[-datos.indice,]

#Generando un árbol de regresión
mi_arbol<-rpart(G3~.,data = datos.entrenamiento)

#Salidas
rpart.plot(mi_arbol)
plotcp(mi_arbol)

#Poda del arbol
mi_arbol2<-prune(mi_arbol,cp=0.26)
rpart.plot(mi_arbol2)

#Comprobación en conjunto de prueba
prediccion<-predict(mi_arbol2,datos.test)

#Evaluación del modelo 
plot(datos.test$G3,prediccion)
summary(lm(prediccion~datos.test$G3))
