if (!require('pacman'))
  install.packages("pacman")
library(pacman)
#Install Java for your system type
pacman::p_load(
  rgdal, #Geospatial Data Abstraction Library o GDAL
  terra, # Lectura, escritura, procesamiento de datos raster 
  sf, # Lectura, escritura, procesamiento de datos vectoriales 
  raster,# Predecesor de Terra, lectura, escritura, procesamiento de datos raster 
  tidyverse, # Conjunto de paquetes para manipulación abreviada y flujo de procesos
  readxl # Lectura de archivos excel
)
# 1. Datos vectoriales

# Importar datos en excel 
pozos <- read_excel("Datos/Pozos_Tubulares por Actividad_TODOS.xls") 

# Imprimir 
print(pozos)


# Convertir los datos excel a datos espaciales
pozos_sf<- pozos %>% st_as_sf(coords = c("X", "Y"),
                              remove = FALSE) 
# Sin CRS
st_crs(pozos_sf)

pozos_sf <- pozos_sf %>%  st_set_crs(32721)# Establecer un sistema de coordenadas

# Con CRS
# EPSG 
st_crs(pozos_sf)
# Una vez establecido el sistema de coordenadas ya sea esférico, cartesiano de 2D o de 3D, se pueden realizar operaciones espaciales con otros objetos


#Tipo de objetos 
class(pozos_sf)


# Elimina la columna geometria, lo cual puede ser útil cuando se trabaja con una gran cantidad de datos o geometrías muy complejas
# Esto se usa preferentemente una vez terminados los geoprocesos, o cuando se quiere reducir el tamaño debido a uso de memoria
pozos_sf_df <- pozos_sf %>% st_drop_geometry()


plot(pozos_sf$geometry)


### Importar un archivo shapefile de polígonos ----

distritos <- read_sf("Datos/barloc_paraguay_2012_.shp")

# Imprime el crs del archivo shapefile de polygonos
st_crs(distritos)

# Elimina dimensiones adicionales Z y M, cuando algunas operaciones no toleran este tipo de dimensiones, es conveniente utilizar esta función
distritos <- distritos  %>% st_zm(., drop = TRUE,what = "ZM")


# Plotea la geometría de distritos

plot(distritos$geometry)

### Manipulación de variables ----

# Funciona básicamente como cualquier dataframe, esto incluye a la funciones pipe, mutate y otras tidyverse

distritos <- distritos %>% st_drop_geometry() # Eliminamos la columna de geometria, solo a través de esta función



distritos <- distritos %>% select(DIST_DESC,DPTO_DESC,AREA,CANT_VIV)

# Contar cuantas viviendas hay en un departamento específico agrupando por nombre de departamento 


dpto_count_viv_chaco <-
  distritos %>%  filter(DPTO_DESC %in% c("BOQUERON", "ALTO PARAGUAY", "PRESIDENTE HAYES")) %>%  group_by(DPTO_DESC) %>% summarise(Cant_viv = sum(n())) %>% ungroup()

#Eliminar
rm(distritos)

# Otro archivo de polígonos
departamentos <- read_sf("Datos/DEPARTAMENTOS_PARAGUAY_2012_.shp") %>% filter(DPTO_DESC == "BOQUERON")

#Plot de Boquerón
plot(departamentos$geometry)

# Imprimir geometría completa
departamentos$geometry[[1]]


# Obtener el área de un poligno

departamentos <- departamentos %>% mutate(area_ha = as.numeric(st_area(geometry)/10000))# El área se obtiene  de acuerdo a la unidad del CRS

#EPSG
### Transformaciones y reprojecciones ----

departamentos_UTM_20S <- departamentos %>% st_set_crs(32720) %>% mutate(area_ha = as.numeric(st_area(geometry)/10000))# Solo establece en crs


departamentos_UTM_20S <- departamentos %>% st_transform(32720)  %>% mutate(area_ha = as.numeric(st_area(geometry)/10000))# Solo establece en crs

#Establecemos el crs correcto

departamentos_UTM_21S <- departamentos %>% st_transform(32721)  %>% mutate(area_ha = as.numeric(st_area(geometry)/10000))# Solo establece en crs

# Imprimimos ambas superficies obtenidas

departamentos_UTM_20S$area_ha
departamentos_UTM_21S$area_ha




# Diferencia de superficie obtenida con diferentes proyecciones

departamentos_UTM_20S$area_ha- departamentos_UTM_21S$area_ha


# Operaciones espaciales entre vectores----

# Subset de puntos en polígnos

properties 



# Reproyectamos a otro crs

pozos_sf <- pozos_sf%>% select(-Departamen)

pozos_sf_en_dpto <- pozos_sf %>% st_transform(32720)  %>% sf::st_intersection(departamentos_UTM_21S)

write_sf(pozos_sf, "pozos_sf.shp")

pozos_sf_en_dpto <- pozos_sf %>% st_transform(32720)  %>% sf::st_intersection(departamentos_UTM_20S)

# Ploteamos los pozos dentro de la geometria de Boqueron

plot(departamentos_UTM_20S$geometry, col = 'grey')
plot(pozos_sf_en_dpto$geometry,add = TRUE)

# Union espacial 

pozos_sf_union <- pozos_sf %>% select(Nombre_del) %>% st_join(distritos[,c("DIST_DESC", "DPTO_DESC", "BAR_LOC")], left = TRUE)

pozos_sf_union <-pozos_sf_union %>%  st_drop_geometry() %>% group_by(DIST_DESC) %>% summarise( "Suma" = sum(n())) 


sum_de_pozos <- sum(pozos_sf_union$Suma) / nrow(cant_dist)
