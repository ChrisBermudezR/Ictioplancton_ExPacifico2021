#Título del script: Gráfica del cambio mareal
#Autores: Christian Bermúdez-Rivas 
#Objetivo: construir la gráfica del cambio mareal en el área de Sanquianga-Gorgona
#Lenguaje: R
#Fecha: Enero 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################

####Librería####
if(!require(tidyverse))install.packages("tidyverse")
if(!require(dplyr))install.packages("dplyr")
if(!require(ggrepel))install.packages("ggrepel")
if(!require(scales))install.packages("scales")



library(tidyverse)
library(dplyr)
library(ggrepel)
library(scales)



source("../../Funciones/ciclo_Mareal.R")

####Entrada de datos####
marea<-read.table("Mareas_Gorgona_Event_10min.csv", header=TRUE, sep = ",")
#names(marea)<-c("combinada", "altura", "fecha", "hora")
#marea <- dplyr::mutate(marea, fecha_Hora= paste(fecha, hora)) 
marea$fecha_Hora<- as.POSIXct(marea$fecha_Hora)
head(marea)
write.table(marea, "Mareas_Gorgona_Event_10min.csv", sep = ",", col.names = TRUE)

estaciones<-read.table("Alturas_Mareales_Estaciones.csv", header=TRUE, sep = ",")
#names(estaciones)<-c("codigo", "hora", "fecha", "altura")
#estaciones <- mutate(estaciones, fecha_Hora = paste(fecha, hora)) 
estaciones$fecha_Hora<- as.POSIXct(estaciones$fecha_Hora)
head(estaciones)
write.table(estaciones, "Alturas_Mareales_Estaciones.csv", sep = ",", col.names = TRUE)


####Construcción de la gráfica####
ciclo<-ciclo_mareal(marea, marea$fecha_Hora, marea$altura)

tiff("onda_Mareal_Gorgona.tif", width = 4000, height = 2000, res = "300", units = "px", pointsize = 12, compression = c("lzw"))
ciclo+
  geom_point(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red", size = 4))+
  geom_label_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), y=estaciones$altura,label = estaciones$codigo),box.padding   = 0.35, 
                                       point.padding = 0.5,
                                       segment.color = 'black')+
  theme(legend.position = "none")
dev.off()


png(filename ="onda_Mareal_Gorgona.png", width = 4000, height = 2000, res = "300", units = "px", pointsize = 15)
ciclo+
  geom_point(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red", size = 4))+
  geom_label_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), y=estaciones$altura,label = estaciones$codigo),box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  theme(legend.position = "none")
dev.off()





####Entrada de datos####
marea<-read.table("Gorgona_Pronostico.txt", header=TRUE, sep = ",")


marea$fecha_transformada <- as.Date(marea$Fecha, format = "%d/%m/%Y")
marea$fecha_Hora<-paste(marea$fecha_transformada, marea$Hora, sep=" ")

#names(marea)<-c("combinada", "altura", "fecha", "hora")
#marea <- dplyr::mutate(marea, fecha_Hora= paste(fecha, hora)) 
marea$fecha_Hora<- as.POSIXct(marea$fecha_Hora)
head(marea)
write.table(marea, "Mareas_Gorgona_Event_10min.csv", sep = ",", col.names = TRUE)

estaciones<-read.table("Alturas_Mareales_Estaciones.csv", header=TRUE, sep = ",")

estaciones$fecha <- as.Date(estaciones$fecha, format = "%d/%m/%Y")
estaciones$fecha_Hora<-paste(estaciones$fecha, estaciones$hora, sep=" ")
#names(estaciones)<-c("codigo", "hora", "fecha", "altura")
#estaciones <- mutate(estaciones, fecha_Hora = paste(fecha, hora)) 
estaciones$fecha_Hora<- as.POSIXct(estaciones$fecha_Hora)
head(estaciones)
write.table(estaciones, "Alturas_Mareales_Estaciones.csv", sep = ",", col.names = TRUE)


####Construcción de la gráfica####
ciclo<-ciclo_mareal(marea, marea$fecha_Hora, marea$Marea_filtro)

tiff("onda_Mareal_Gorgona.tif", width = 4000, height = 2000, res = "300", units = "px", pointsize = 12, compression = c("lzw"))
ciclo+
  geom_point(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red", size = 4))+
  geom_label_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), y=estaciones$altura,label = estaciones$codigo),box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  theme(legend.position = "none")
dev.off()


png(filename ="onda_Mareal_Gorgona.png", width = 4000, height = 2000, res = "300", units = "px", pointsize = 15)
ciclo+
  geom_point(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red", size = 4))+
  geom_label_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), y=estaciones$altura,label = estaciones$codigo),box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  theme(legend.position = "none")
dev.off()



ciclo+
  geom_point(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red", size = 4))+
  geom_label_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), y=estaciones$altura,label = estaciones$codigo),box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  theme(legend.position = "none")