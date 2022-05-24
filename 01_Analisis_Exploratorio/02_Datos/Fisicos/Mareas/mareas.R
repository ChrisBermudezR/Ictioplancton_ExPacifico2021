#Título del scrip: Gráfica del cambio mareal
#Autores: Christian Bermúdez-Rivas 
#Objetivo: construir la gráfica del cambio mareal en el área de Sanquianga-Gorgona
#Lenguaje: R
#Fecha: Enero 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################

####Librería####
library(ggplot2)
library(dplyr)
library(lubridate)


####Entrada de datos####
marea<-read.table("Mareas_Gorgona_Event_10min.csv", header=TRUE, sep = ",")
names(marea)<-c("combinada", "altura", "fecha", "hora")
marea <- mutate(marea, fecha_Hora = paste(fecha, hora)) 
marea$fecha_Hora<- as.POSIXct(marea$fecha_Hora)
head(marea)


estaciones<-read.table("Alturas_Mareales_Estaciones.csv", header=TRUE, sep = ",")
names(estaciones)<-c("codigo", "hora", "fecha", "altura")
estaciones <- mutate(estaciones, fecha_Hora = paste(fecha, hora)) 
estaciones$fecha_Hora<- as.POSIXct(estaciones$fecha_Hora)
head(estaciones)


####Construcción de la gráfica####
grafica<-ggplot2::ggplot(data=estaciones, aes(x=as.POSIXct(fecha_Hora), y=altura))+
    geom_point(color = "red", size = 4)
   
tiff("onda_Mareal_Gorgona.tif", width = 4000, height = 2000, res = "300", units = "px", pointsize = 12, compression = c("lzw"))
grafica+
  geom_label_repel(aes(label = codigo),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  geom_line(data=marea, aes(x=fecha_Hora, y=altura),size=1, colour="grey")+
  geom_hline(yintercept = 1:3,linetype='dotted', col = 'red')+
  labs(x = "dias", y = "Altura mareal (m)") +
  theme_classic()+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2021-04-29 00:00:00"),
                 as.POSIXct("2021-05-05 00:00:00"), "6 hours"),
    labels = date_format("%a-%d\n%H:%M", tz = ""),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2021-04-29 00:00:00"),
      as.POSIXct("2021-05-05 00:00:00")
    )
  )
dev.off()

 

  
