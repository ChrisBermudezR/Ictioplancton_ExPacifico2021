#Titulo: Visualización y Análisis descriptivo - CCCP
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos físicos obtenidos de la sonda CDT-O General Oceanics 19v PLUS.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################



#Este script tiene cómo objetivo realizar las gráficas del componente físico de la Expedición Pacífico.

library(tidyverse)
library(gridExtra)
library(gsw)
library(oce)
library(pastecs)

#####Manejo del conjunto de datos####
#Cargar el archivo de datos
Datos_CTDO_CCCP<-readr::read_csv("./02_Datos/Fisicos/Datos_CTDO_CCCP.csv")
#Asignar factores al evento de muestreo que en este caso está descrito por la variable "Codigo"
Datos_CTDO_CCCP$No.Estacion<-as.factor(Datos_CTDO_CCCP$No.Estacion)
Datos_CTDO_CCCP$Marea<-as.factor(Datos_CTDO_CCCP$Marea)
Datos_CTDO_CCCP$Codigo<-as.factor(Datos_CTDO_CCCP$Codigo)
Datos_CTDO_CCCP <- Datos_CTDO_CCCP %>% group_by(Codigo)



####Estadistica descriptiva####
  



EstadisticasDescrip<-Datos_CTDO_CCCP %>% summarise_each(funs(mean(., na.rm = TRUE),   median(., na.rm = TRUE),n(),sd(., na.rm = TRUE), min(., na.rm = TRUE),max(., na.rm = TRUE)), Temperatura, Salinidad, Oxigeno, Densidad, Profundidad)

write_csv(EstadisticasDescrip, "./01_Resultados/Fisicos_EstadisticasDescrip_CCCP.csv", col_names = TRUE)


#########################

Temperatura_Total_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Transecto, y=Temperatura)) + 
  geom_boxplot()+ 
  theme_bw()+geom_jitter(width=0.2,alpha=0.2) +
  stat_summary(fun=mean, aes(y = Temperatura), geom="point", shape=20, size=4, color="red", position = position_dodge(width =0.8)) +
  labs( y = "Temperatura [°C]", x = "Transectos")
  
Salnidad_Total_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Transecto, y=Salinidad)) + 
  geom_boxplot()+ 
  theme_bw()+geom_jitter(width=0.2,alpha=0.2) +
  stat_summary(fun=mean, aes(y = Salinidad), geom="point", shape=20, size=4, color="red", position = position_dodge(width =0.8)) +
  labs( y = "Salinidad [PSU]", x = "Transectos")

Densidad_Total_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Transecto, y=Densidad)) + 
  geom_boxplot()+  
  theme_bw()+geom_jitter(width=0.2,alpha=0.2) +
  stat_summary(fun=mean, aes(y = Densidad), geom="point", shape=20, size=4, color="red", position = position_dodge(width =0.8)) +
  labs( y = "Densidad [kg/m3]", x = "Transectos")

Oxigeno_Total_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Transecto, y=Oxigeno)) + 
  geom_boxplot()+  
  theme_bw()+geom_jitter(width=0.2,alpha=0.2) +
  stat_summary(fun=mean, aes(y = Oxigeno), geom="point", shape=20, size=4, color="red", position = position_dodge(width =0.8)) +
  labs( y = "Oxígeno disuelto [mg/L]", x = "Transectos")
 

tiff(filename = "./03_Imagenes/01_Datos_Totales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2, Temperatura_Total_CCCP, 
             Salnidad_Total_CCCP,Densidad_Total_CCCP,Oxigeno_Total_CCCP, 
             top="Datos totales")
dev.off()



png(filename = "./03_Imagenes/01_Datos_Totales_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, res = 300)
grid.arrange(nrow=2, ncol=2, Temperatura_Total_CCCP, 
             Salnidad_Total_CCCP,Densidad_Total_CCCP,Oxigeno_Total_CCCP, 
             top="Datos totales")
dev.off()


Temperatura_Hist_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Temperatura)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma de la Temperatura [°C]",
       subtitle = "(Distribuido por Transectos.)",
       y = "Frecuencia", x = "[°C]")+
  facet_grid(Marea~Transecto)
Salinidad_Hist_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Salinidad)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma de la Salinidad [PSU]",
       subtitle = "(Distribuido por Transectos.)",
       y = "Frecuencia", x = "[PSU]")+
  facet_grid(Marea~Transecto)
Densidad_Hist_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Densidad)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma de la Densidad [kg/m3]",
       subtitle = "(Distribuido por Transectos.)",
       y = "Frecuencia", x = " [kg/m3]")+
  facet_grid(Marea~Transecto)

Oxigeno_Hist_CCCP<-ggplot(Datos_CTDO_CCCP, aes(x=Oxigeno)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma del Oxígeno disuelto [mg/L]",
       subtitle = "(Distribuido por Transectos.)",
       y = "Frecuencia", x = " mg/L]")+
  facet_grid(Marea~Transecto)

tiff(filename = "./03_Imagenes/02_Histogramas_CCCP.tif", width = 40, height = 30, units = "cm", pointsize = 30, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_CCCP, Salinidad_Hist_CCCP, Densidad_Hist_CCCP, Oxigeno_Hist_CCCP)
dev.off()

png(filename = "./03_Imagenes/02_Histogramas_CCCP.png", width = 40, height = 30, units = "cm", pointsize = 30, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_CCCP, Salinidad_Hist_CCCP, Densidad_Hist_CCCP, Oxigeno_Hist_CCCP)
dev.off()

Temperatura_boxplot_CCCP<-ggplot(Datos_CTDO_CCCP) + 
  geom_boxplot(aes(x=No.Estacion, y=Temperatura))+ 
  theme_bw()+
  stat_summary(fun=mean, aes(y = Temperatura,x=No.Estacion), geom="point", shape=20, size=2, color="red", position = position_dodge(width =0.8)) +
  labs(title = "Boxplot de la Temperatura [°C]",
       y = "Temperatura [°C]", x = "Estaciones")+
  facet_grid(Marea~Transecto)

Salinidad_boxplot_CCCP<-ggplot(Datos_CTDO_CCCP) + 
  geom_boxplot(aes(x=No.Estacion, y=Salinidad))+ 
  theme_bw()+
  stat_summary(fun=mean, aes(y = Salinidad,x=No.Estacion), geom="point", shape=20, size=2, color="red", position = position_dodge(width =0.8)) +
  labs(title = "Boxplot de la Salinidad [PSU]",
       y = "Salinidad [PSU]", x = "Estaciones")+
  facet_grid(Marea~Transecto)

Densidad_boxplot_CCCP<-ggplot(Datos_CTDO_CCCP) + 
  geom_boxplot(aes(x=No.Estacion, y=Densidad))+ 
  stat_summary(fun=mean, aes(y = Densidad,x=No.Estacion), geom="point", shape=20, size=2, color="red", position = position_dodge(width =0.8)) +
  theme_bw()+
  labs(title = "Boxplot de la Densidad [Kg/m3]",
       y = "Densidad [Kg/m3]", x = "Estaciones")+
  facet_grid(Marea~Transecto)

Oxigeno_boxplot_CCCP<-ggplot(Datos_CTDO_CCCP) + 
  geom_boxplot(aes(x=No.Estacion, y=Oxigeno))+ 
  stat_summary(fun=mean, aes(y = Oxigeno,x=No.Estacion), geom="point", shape=20, size=2, color="red", position = position_dodge(width =0.8)) +
  theme_bw()+
  labs(title = "Boxplot de la Oxígeno disuelto [mg/L]",
       y = "Oxígeno Disuelto - [mg/L]", x = "Estaciones")+
  facet_grid(Marea~Transecto)


tiff(filename = "./03_Imagenes/03_Boxplot_CCCP.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_CCCP, Salinidad_boxplot_CCCP, 
             Densidad_boxplot_CCCP, Oxigeno_boxplot_CCCP)
dev.off()

png(filename = "./03_Imagenes/03_Boxplot_CCCP.png", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_CCCP, Salinidad_boxplot_CCCP, 
             Densidad_boxplot_CCCP, Oxigeno_boxplot_CCCP)
dev.off()



#Crear una lista con los niveles del código de evento de muestreo
niveles_Codigo<-as.list(levels(Datos_CTDO_CCCP$Codigo))

#Ciclo para asignar objetos a un filtro de datos por cada evento de muestreo
for(i in seq_along(niveles_Codigo)){
  assign(paste0(niveles_Codigo[[i]], "_CCCP"),dplyr::filter(Datos_CTDO_CCCP, Codigo==niveles_Codigo[[i]]))
}
  


Estandar_75<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(75,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}

Estandar_15<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(15,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}
Estandar_40<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(40,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}

  
Temp_A06A_CCCP<-Estandar_15(A06A_CCCP, "A06 - Marea Alta", A06A_CCCP$Temperatura, A06A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A06B_CCCP<-Estandar_15(A06B_CCCP, "A06 - Marea Baja", A06B_CCCP$Temperatura, A06B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05A_CCCP<-Estandar_15(A05A_CCCP, "A05 - Marea Alta", A05A_CCCP$Temperatura, A05A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05B_CCCP<-Estandar_15(A05B_CCCP, "A05 - Marea Baja", A05B_CCCP$Temperatura, A05B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04A_CCCP<-Estandar_15(A04A_CCCP, "A04 - Marea Alta", A04A_CCCP$Temperatura, A04A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04B_CCCP<-Estandar_15(A04B_CCCP, "A04 - Marea Baja", A04B_CCCP$Temperatura, A04B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03A_CCCP<-Estandar_40(A03A_CCCP, "A03 - Marea Alta", A03A_CCCP$Temperatura, A03A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03B_CCCP<-Estandar_40(A03B_CCCP, "A03 - Marea Baja", A03B_CCCP$Temperatura, A03B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02A_CCCP<-Estandar_75(A02A_CCCP, "A02 - Marea Alta", A02A_CCCP$Temperatura, A02A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02B_CCCP<-Estandar_75(A02B_CCCP, "A02 - Marea Baja", A02B_CCCP$Temperatura, A02B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01A_CCCP<-Estandar_75(A01A_CCCP, "A01 - Marea Alta", A01A_CCCP$Temperatura, A01A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01B_CCCP<-Estandar_75(A01B_CCCP, "A01 - Marea Baja", A01B_CCCP$Temperatura, A01B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_A01A_CCCP, Temp_A01B_CCCP, Temp_A02A_CCCP,Temp_A02B_CCCP, Temp_A03A_CCCP, Temp_A03B_CCCP, Temp_A04A_CCCP, Temp_A04B_CCCP, Temp_A05A_CCCP,Temp_A05B_CCCP, Temp_A06A_CCCP, Temp_A06B_CCCP, top="Transecto Amarales")
dev.off()

png(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_A01A_CCCP, Temp_A01B_CCCP, Temp_A02A_CCCP,Temp_A02B_CCCP, Temp_A03A_CCCP, Temp_A03B_CCCP, Temp_A04A_CCCP, Temp_A04B_CCCP, Temp_A05A_CCCP,Temp_A05B_CCCP, Temp_A06A_CCCP, Temp_A06B_CCCP, top="Transecto Amarales")
dev.off()

Sal_A06A_CCCP<-Estandar_15(A06A_CCCP, "A06 - Marea Alta", A06A_CCCP$Salinidad, A06A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A06B_CCCP<-Estandar_15(A06B_CCCP, "A06 - Marea Baja", A06B_CCCP$Salinidad, A06B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05A_CCCP<-Estandar_15(A05A_CCCP, "A05 - Marea Alta", A05A_CCCP$Salinidad, A05A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05B_CCCP<-Estandar_15(A05B_CCCP, "A05 - Marea Baja", A05B_CCCP$Salinidad, A05B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04A_CCCP<-Estandar_15(A04A_CCCP, "A04 - Marea Alta", A04A_CCCP$Salinidad, A04A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04B_CCCP<-Estandar_15(A04B_CCCP, "A04 - Marea Baja", A04B_CCCP$Salinidad, A04B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03A_CCCP<-Estandar_40(A03A_CCCP, "A03 - Marea Alta", A03A_CCCP$Salinidad, A03A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03B_CCCP<-Estandar_40(A03B_CCCP, "A03 - Marea Baja", A03B_CCCP$Salinidad, A03B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02A_CCCP<-Estandar_75(A02A_CCCP, "A02 - Marea Alta", A02A_CCCP$Salinidad, A02A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02B_CCCP<-Estandar_75(A02B_CCCP, "A02 - Marea Baja", A02B_CCCP$Salinidad, A02B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01A_CCCP<-Estandar_75(A01A_CCCP, "A01 - Marea Alta", A01A_CCCP$Salinidad, A01A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01B_CCCP<-Estandar_75(A01B_CCCP, "A01 - Marea Baja", A01B_CCCP$Salinidad, A01B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_A01A_CCCP, Sal_A01B_CCCP, Sal_A02A_CCCP,Sal_A02B_CCCP, Sal_A03A_CCCP, Sal_A03B_CCCP, Sal_A04A_CCCP, Sal_A04B_CCCP, Sal_A05A_CCCP,Sal_A05B_CCCP, Sal_A06A_CCCP, Sal_A06B_CCCP, top="Transecto Amarales")
dev.off()

Den_A06A_CCCP<-Estandar_15(A06A_CCCP, "A06 - Marea Alta", A06A_CCCP$Densidad, A06A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A06B_CCCP<-Estandar_15(A06B_CCCP, "A06 - Marea Baja", A06B_CCCP$Densidad, A06B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05A_CCCP<-Estandar_15(A05A_CCCP, "A05 - Marea Alta", A05A_CCCP$Densidad, A05A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05B_CCCP<-Estandar_15(A05B_CCCP, "A05 - Marea Baja", A05B_CCCP$Densidad, A05B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04A_CCCP<-Estandar_15(A04A_CCCP, "A04 - Marea Alta", A04A_CCCP$Densidad, A04A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04B_CCCP<-Estandar_15(A04B_CCCP, "A04 - Marea Baja", A04B_CCCP$Densidad, A04B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03A_CCCP<-Estandar_40(A03A_CCCP, "A03 - Marea Alta", A03A_CCCP$Densidad, A03A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03B_CCCP<-Estandar_40(A03B_CCCP, "A03 - Marea Baja", A03B_CCCP$Densidad, A03B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02A_CCCP<-Estandar_75(A02A_CCCP, "A02 - Marea Alta", A02A_CCCP$Densidad, A02A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02B_CCCP<-Estandar_75(A02B_CCCP, "A02 - Marea Baja", A02B_CCCP$Densidad, A02B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01A_CCCP<-Estandar_75(A01A_CCCP, "A01 - Marea Alta", A01A_CCCP$Densidad, A01A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01B_CCCP<-Estandar_75(A01B_CCCP, "A01 - Marea Baja", A01B_CCCP$Densidad, A01B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Densidad_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_A01A_CCCP, Den_A01B_CCCP, Den_A02A_CCCP,Den_A02B_CCCP, Den_A03A_CCCP, Den_A03B_CCCP, Den_A04A_CCCP, Den_A04B_CCCP, Den_A05A_CCCP,Den_A05B_CCCP, Den_A06A_CCCP, Den_A06B_CCCP, top="Transecto Amarales")
dev.off()

Oxi_A06A_CCCP<-Estandar_15(A06A_CCCP, "A06 - Marea Alta", A06A_CCCP$Oxigeno, A06A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A06B_CCCP<-Estandar_15(A06B_CCCP, "A06 - Marea Baja", A06B_CCCP$Oxigeno, A06B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05A_CCCP<-Estandar_15(A05A_CCCP, "A05 - Marea Alta", A05A_CCCP$Oxigeno, A05A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05B_CCCP<-Estandar_15(A05B_CCCP, "A05 - Marea Baja", A05B_CCCP$Oxigeno, A05B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04A_CCCP<-Estandar_15(A04A_CCCP, "A04 - Marea Alta", A04A_CCCP$Oxigeno, A04A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04B_CCCP<-Estandar_15(A04B_CCCP, "A04 - Marea Baja", A04B_CCCP$Oxigeno, A04B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03A_CCCP<-Estandar_40(A03A_CCCP, "A03 - Marea Alta", A03A_CCCP$Oxigeno, A03A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03B_CCCP<-Estandar_40(A03B_CCCP, "A03 - Marea Baja", A03B_CCCP$Oxigeno, A03B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02A_CCCP<-Estandar_75(A02A_CCCP, "A02 - Marea Alta", A02A_CCCP$Oxigeno, A02A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02B_CCCP<-Estandar_75(A02B_CCCP, "A02 - Marea Baja", A02B_CCCP$Oxigeno, A02B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01A_CCCP<-Estandar_75(A01A_CCCP, "A01 - Marea Alta", A01A_CCCP$Oxigeno, A01A_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01B_CCCP<-Estandar_75(A01B_CCCP, "A01 - Marea Baja", A01B_CCCP$Oxigeno, A01B_CCCP$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")


tiff(filename = "./03_Imagenes/Oxigeno_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Oxi_A01A_CCCP, Oxi_A01B_CCCP, Oxi_A02A_CCCP,Oxi_A02B_CCCP, Oxi_A03A_CCCP, Oxi_A03B_CCCP, Oxi_A04A_CCCP, Oxi_A04B_CCCP, Oxi_A05A_CCCP,Oxi_A05B_CCCP, Oxi_A06A_CCCP, Oxi_A06B_CCCP, top="Transecto Amarales")
dev.off()



Temp_S06A_CCCP<-Estandar_15(S06A_CCCP, "S06 - Marea Alta", S06A_CCCP$Temperatura, S06A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S06B_CCCP<-Estandar_15(S06B_CCCP, "S06 - Marea Baja", S06B_CCCP$Temperatura, S06B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05A_CCCP<-Estandar_15(S05A_CCCP, "S05 - Marea Alta", S05A_CCCP$Temperatura, S05A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05B_CCCP<-Estandar_15(S05B_CCCP, "S05 - Marea Baja", S05B_CCCP$Temperatura, S05B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04A_CCCP<-Estandar_15(S04A_CCCP, "S04 - Marea Alta", S04A_CCCP$Temperatura, S04A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04B_CCCP<-Estandar_15(S04B_CCCP, "S04 - Marea Baja", S04B_CCCP$Temperatura, S04B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03A_CCCP<-Estandar_40(S03A_CCCP, "S03 - Marea Alta", S03A_CCCP$Temperatura, S03A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03B_CCCP<-Estandar_40(S03B_CCCP, "S03 - Marea Baja", S03B_CCCP$Temperatura, S03B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02A_CCCP<-Estandar_75(S02A_CCCP, "S02 - Marea Alta", S02A_CCCP$Temperatura, S02A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02B_CCCP<-Estandar_75(S02B_CCCP, "S02 - Marea Baja", S02B_CCCP$Temperatura, S02B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01A_CCCP<-Estandar_75(S01A_CCCP, "S01 - Marea Alta", S01A_CCCP$Temperatura, S01A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01B_CCCP<-Estandar_75(S01B_CCCP, "S01 - Marea Baja", S01B_CCCP$Temperatura, S01B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_S01A_CCCP, Temp_S01B_CCCP, Temp_S02A_CCCP,Temp_S02B_CCCP, Temp_S03A_CCCP, Temp_S03B_CCCP, Temp_S04A_CCCP, Temp_S04B_CCCP, Temp_S05A_CCCP,Temp_S05B_CCCP, Temp_S06A_CCCP, Temp_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

Sal_S06A_CCCP<-Estandar_15(S06A_CCCP, "S06 - Marea Alta", S06A_CCCP$Salinidad, S06A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S06B_CCCP<-Estandar_15(S06B_CCCP, "S06 - Marea Baja", S06B_CCCP$Salinidad, S06B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05A_CCCP<-Estandar_15(S05A_CCCP, "S05 - Marea Alta", S05A_CCCP$Salinidad, S05A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05B_CCCP<-Estandar_15(S05B_CCCP, "S05 - Marea Baja", S05B_CCCP$Salinidad, S05B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04A_CCCP<-Estandar_15(S04A_CCCP, "S04 - Marea Alta", S04A_CCCP$Salinidad, S04A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04B_CCCP<-Estandar_15(S04B_CCCP, "S04 - Marea Baja", S04B_CCCP$Salinidad, S04B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03A_CCCP<-Estandar_40(S03A_CCCP, "S03 - Marea Alta", S03A_CCCP$Salinidad, S03A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03B_CCCP<-Estandar_40(S03B_CCCP, "S03 - Marea Baja", S03B_CCCP$Salinidad, S03B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02A_CCCP<-Estandar_75(S02A_CCCP, "S02 - Marea Alta", S02A_CCCP$Salinidad, S02A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02B_CCCP<-Estandar_75(S02B_CCCP, "S02 - Marea Baja", S02B_CCCP$Salinidad, S02B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01A_CCCP<-Estandar_75(S01A_CCCP, "S01 - Marea Alta", S01A_CCCP$Salinidad, S01A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01B_CCCP<-Estandar_75(S01B_CCCP, "S01 - Marea Baja", S01B_CCCP$Salinidad, S01B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_S01A_CCCP, Sal_S01B_CCCP, Sal_S02A_CCCP,Sal_S02B_CCCP, Sal_S03A_CCCP, Sal_S03B_CCCP, Sal_S04A_CCCP, Sal_S04B_CCCP, Sal_S05A_CCCP,Sal_S05B_CCCP, Sal_S06A_CCCP, Sal_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

Den_S06A_CCCP<-Estandar_15(S06A_CCCP, "S06 - Marea Alta", S06A_CCCP$Densidad, S06A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S06B_CCCP<-Estandar_15(S06B_CCCP, "S06 - Marea Baja", S06B_CCCP$Densidad, S06B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05A_CCCP<-Estandar_15(S05A_CCCP, "S05 - Marea Alta", S05A_CCCP$Densidad, S05A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05B_CCCP<-Estandar_15(S05B_CCCP, "S05 - Marea Baja", S05B_CCCP$Densidad, S05B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04A_CCCP<-Estandar_15(S04A_CCCP, "S04 - Marea Alta", S04A_CCCP$Densidad, S04A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04B_CCCP<-Estandar_15(S04B_CCCP, "S04 - Marea Baja", S04B_CCCP$Densidad, S04B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03A_CCCP<-Estandar_40(S03A_CCCP, "S03 - Marea Alta", S03A_CCCP$Densidad, S03A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03B_CCCP<-Estandar_40(S03B_CCCP, "S03 - Marea Baja", S03B_CCCP$Densidad, S03B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02A_CCCP<-Estandar_75(S02A_CCCP, "S02 - Marea Alta", S02A_CCCP$Densidad, S02A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02B_CCCP<-Estandar_75(S02B_CCCP, "S02 - Marea Baja", S02B_CCCP$Densidad, S02B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01A_CCCP<-Estandar_75(S01A_CCCP, "S01 - Marea Alta", S01A_CCCP$Densidad, S01A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01B_CCCP<-Estandar_75(S01B_CCCP, "S01 - Marea Baja", S01B_CCCP$Densidad, S01B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")


tiff(filename = "./03_Imagenes/Densidad_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_S01A_CCCP, Den_S01B_CCCP, Den_S02A_CCCP,Den_S02B_CCCP, Den_S03A_CCCP, Den_S03B_CCCP, Den_S04A_CCCP, Den_S04B_CCCP, Den_S05A_CCCP,Den_S05B_CCCP, Den_S06A_CCCP, Den_S06B_CCCP, top="Transecto Sanquianga")
dev.off()


Oxi_S06A_CCCP<-Estandar_15(S06A_CCCP, "S06 - Marea Alta", S06A_CCCP$Oxigeno, S06A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S06B_CCCP<-Estandar_15(S06B_CCCP, "S06 - Marea Baja", S06B_CCCP$Oxigeno, S06B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05A_CCCP<-Estandar_15(S05A_CCCP, "S05 - Marea Alta", S05A_CCCP$Oxigeno, S05A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05B_CCCP<-Estandar_15(S05B_CCCP, "S05 - Marea Baja", S05B_CCCP$Oxigeno, S05B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04A_CCCP<-Estandar_15(S04A_CCCP, "S04 - Marea Alta", S04A_CCCP$Oxigeno, S04A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04B_CCCP<-Estandar_15(S04B_CCCP, "S04 - Marea Baja", S04B_CCCP$Oxigeno, S04B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03A_CCCP<-Estandar_40(S03A_CCCP, "S03 - Marea Alta", S03A_CCCP$Oxigeno, S03A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03B_CCCP<-Estandar_40(S03B_CCCP, "S03 - Marea Baja", S03B_CCCP$Oxigeno, S03B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02A_CCCP<-Estandar_75(S02A_CCCP, "S02 - Marea Alta", S02A_CCCP$Oxigeno, S02A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02B_CCCP<-Estandar_75(S02B_CCCP, "S02 - Marea Baja", S02B_CCCP$Oxigeno, S02B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01A_CCCP<-Estandar_75(S01A_CCCP, "S01 - Marea Alta", S01A_CCCP$Oxigeno, S01A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01B_CCCP<-Estandar_75(S01B_CCCP, "S01 - Marea Baja", S01B_CCCP$Oxigeno, S01B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Oxigeno_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Oxi_S01A_CCCP, Oxi_S01B_CCCP, Oxi_S02A_CCCP,Oxi_S02B_CCCP, Oxi_S03A_CCCP, Oxi_S03B_CCCP, Oxi_S04A_CCCP, Oxi_S04B_CCCP, Oxi_S05A_CCCP,Oxi_S05B_CCCP, Oxi_S06A_CCCP, Oxi_S06B_CCCP, top="Transecto Sanquianga")
dev.off()


Temp_G06A_CCCP<-Estandar_15(G06A_CCCP, "G06 - Marea Alta", G06A_CCCP$Temperatura, G06A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G06B_CCCP<-Estandar_15(G06B_CCCP, "G06 - Marea Baja", G06B_CCCP$Temperatura, G06B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05A_CCCP<-Estandar_15(G05A_CCCP, "G05 - Marea Alta", G05A_CCCP$Temperatura, G05A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05B_CCCP<-Estandar_15(G05B_CCCP, "G05 - Marea Baja", G05B_CCCP$Temperatura, G05B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04A_CCCP<-Estandar_15(G04A_CCCP, "G04 - Marea Alta", G04A_CCCP$Temperatura, G04A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04B_CCCP<-Estandar_15(G04B_CCCP, "G04 - Marea Baja", G04B_CCCP$Temperatura, G04B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03A_CCCP<-Estandar_40(G03A_CCCP, "G03 - Marea Alta", G03A_CCCP$Temperatura, G03A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03B_CCCP<-Estandar_40(G03B_CCCP, "G03 - Marea Baja", G03B_CCCP$Temperatura, G03B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02A_CCCP<-Estandar_75(G02A_CCCP, "G02 - Marea Alta", G02A_CCCP$Temperatura, G02A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02B_CCCP<-Estandar_75(G02B_CCCP, "G02 - Marea Baja", G02B_CCCP$Temperatura, G02B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01A_CCCP<-Estandar_75(G01A_CCCP, "G01 - Marea Alta", G01A_CCCP$Temperatura, G01A_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01B_CCCP<-Estandar_75(G01B_CCCP, "G01 - Marea Baja", G01B_CCCP$Temperatura, G01B_CCCP$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_G01A_CCCP, Temp_G01B_CCCP, Temp_G02A_CCCP,Temp_G02B_CCCP, Temp_G03A_CCCP, Temp_G03B_CCCP, Temp_G04A_CCCP, Temp_G04B_CCCP, Temp_G05A_CCCP,Temp_G05B_CCCP, Temp_G06A_CCCP, Temp_G06B_CCCP, top="Transecto Guascama")
dev.off()

Sal_G06A_CCCP<-Estandar_15(G06A_CCCP, "G06 - Marea Alta", G06A_CCCP$Salinidad, G06A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G06B_CCCP<-Estandar_15(G06B_CCCP, "G06 - Marea Baja", G06B_CCCP$Salinidad, G06B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05A_CCCP<-Estandar_15(G05A_CCCP, "G05 - Marea Alta", G05A_CCCP$Salinidad, G05A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05B_CCCP<-Estandar_15(G05B_CCCP, "G05 - Marea Baja", G05B_CCCP$Salinidad, G05B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04A_CCCP<-Estandar_15(G04A_CCCP, "G04 - Marea Alta", G04A_CCCP$Salinidad, G04A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04B_CCCP<-Estandar_15(G04B_CCCP, "G04 - Marea Baja", G04B_CCCP$Salinidad, G04B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03A_CCCP<-Estandar_40(G03A_CCCP, "G03 - Marea Alta", G03A_CCCP$Salinidad, G03A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03B_CCCP<-Estandar_40(G03B_CCCP, "G03 - Marea Baja", G03B_CCCP$Salinidad, G03B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02A_CCCP<-Estandar_75(G02A_CCCP, "G02 - Marea Alta", G02A_CCCP$Salinidad, G02A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02B_CCCP<-Estandar_75(G02B_CCCP, "G02 - Marea Baja", G02B_CCCP$Salinidad, G02B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01A_CCCP<-Estandar_75(G01A_CCCP, "G01 - Marea Alta", G01A_CCCP$Salinidad, G01A_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01B_CCCP<-Estandar_75(G01B_CCCP, "G01 - Marea Baja", G01B_CCCP$Salinidad, G01B_CCCP$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")


tiff(filename = "./03_Imagenes/Salinidad_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_G01A_CCCP, Sal_G01B_CCCP, Sal_G02A_CCCP,Sal_G02B_CCCP, Sal_G03A_CCCP, Sal_G03B_CCCP, Sal_G04A_CCCP, Sal_G04B_CCCP, Sal_G05A_CCCP,Sal_G05B_CCCP, Sal_G06A_CCCP, Sal_G06B_CCCP, top="Transecto Guascama")
dev.off()


Den_G06A_CCCP<-Estandar_15(G06A_CCCP, "G06 - Marea Alta", G06A_CCCP$Densidad, G06A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G06B_CCCP<-Estandar_15(G06B_CCCP, "G06 - Marea Baja", G06B_CCCP$Densidad, G06B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05A_CCCP<-Estandar_15(G05A_CCCP, "G05 - Marea Alta", G05A_CCCP$Densidad, G05A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05B_CCCP<-Estandar_15(G05B_CCCP, "G05 - Marea Baja", G05B_CCCP$Densidad, G05B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04A_CCCP<-Estandar_15(G04A_CCCP, "G04 - Marea Alta", G04A_CCCP$Densidad, G04A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04B_CCCP<-Estandar_15(G04B_CCCP, "G04 - Marea Baja", G04B_CCCP$Densidad, G04B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03A_CCCP<-Estandar_40(G03A_CCCP, "G03 - Marea Alta", G03A_CCCP$Densidad, G03A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03B_CCCP<-Estandar_40(G03B_CCCP, "G03 - Marea Baja", G03B_CCCP$Densidad, G03B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02A_CCCP<-Estandar_75(G02A_CCCP, "G02 - Marea Alta", G02A_CCCP$Densidad, G02A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02B_CCCP<-Estandar_75(G02B_CCCP, "G02 - Marea Baja", G02B_CCCP$Densidad, G02B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01A_CCCP<-Estandar_75(G01A_CCCP, "G01 - Marea Alta", G01A_CCCP$Densidad, G01A_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01B_CCCP<-Estandar_75(G01B_CCCP, "G01 - Marea Baja", G01B_CCCP$Densidad, G01B_CCCP$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Densidad_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_G01A_CCCP, Den_G01B_CCCP, Den_G02A_CCCP,Den_G02B_CCCP, Den_G03A_CCCP, Den_G03B_CCCP, Den_G04A_CCCP, Den_G04B_CCCP, Den_G05A_CCCP,Den_G05B_CCCP, Den_G06A_CCCP, Den_G06B_CCCP, top="Transecto Guascama")
dev.off()


Oxi_G06A_CCCP<-Estandar_15(G06A_CCCP, "G06 - Marea Alta", G06A_CCCP$Oxigeno, G06A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G06B_CCCP<-Estandar_15(G06B_CCCP, "G06 - Marea Baja", G06B_CCCP$Oxigeno, G06B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G05A_CCCP<-Estandar_15(G05A_CCCP, "G05 - Marea Alta", G05A_CCCP$Oxigeno, G05A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G05B_CCCP<-Estandar_15(G05B_CCCP, "G05 - Marea Baja", G05B_CCCP$Oxigeno, G05B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G04A_CCCP<-Estandar_15(G04A_CCCP, "G04 - Marea Alta", G04A_CCCP$Oxigeno, G04A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G04B_CCCP<-Estandar_15(G04B_CCCP, "G04 - Marea Baja", G04B_CCCP$Oxigeno, G04B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G03A_CCCP<-Estandar_40(G03A_CCCP, "G03 - Marea Alta", G03A_CCCP$Oxigeno, G03A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G03B_CCCP<-Estandar_40(G03B_CCCP, "G03 - Marea Baja", G03B_CCCP$Oxigeno, G03B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G02A_CCCP<-Estandar_75(G02A_CCCP, "G02 - Marea Alta", G02A_CCCP$Oxigeno, G02A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G02B_CCCP<-Estandar_75(G02B_CCCP, "G02 - Marea Baja", G02B_CCCP$Oxigeno, G02B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G01A_CCCP<-Estandar_75(G01A_CCCP, "G01 - Marea Alta", G01A_CCCP$Oxigeno, G01A_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G01B_CCCP<-Estandar_75(G01B_CCCP, "G01 - Marea Baja", G01B_CCCP$Oxigeno, G01B_CCCP$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")

tiff(filename = "./03_Imagenes/Oxigeno_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Oxi_G01A_CCCP, Oxi_G01B_CCCP, Oxi_G02A_CCCP,Oxi_G02B_CCCP, Oxi_G03A_CCCP, Oxi_G03B_CCCP, Oxi_G04A_CCCP, Oxi_G04B_CCCP, Oxi_G05A_CCCP,Oxi_G05B_CCCP, Oxi_G06A_CCCP, Oxi_G06B_CCCP, top="Transecto Guascama")
dev.off()



#Temperatura
Temp01_CCCP<-grid.arrange(nrow=6, ncol=2,Temp_A01A_CCCP, Temp_A01B_CCCP, Temp_A02A_CCCP,Temp_A02B_CCCP, Temp_A03A_CCCP, Temp_A03B_CCCP, Temp_A04A_CCCP, Temp_A04B_CCCP, Temp_A05A_CCCP,Temp_A05B_CCCP, Temp_A06A_CCCP, Temp_A06B_CCCP, top="Transecto Amarales")
Temp02_CCCP<-grid.arrange(nrow=6, ncol=2,Temp_S01A_CCCP, Temp_S01B_CCCP, Temp_S02A_CCCP,Temp_S02B_CCCP, Temp_S03A_CCCP, Temp_S03B_CCCP, Temp_S04A_CCCP, Temp_S04B_CCCP, Temp_S05A_CCCP,Temp_S05B_CCCP, Temp_S06A_CCCP, Temp_S06B_CCCP, top="Transecto Sanquianga")
Temp03_CCCP<-grid.arrange(nrow=6, ncol=2,Temp_G01A_CCCP, Temp_G01B_CCCP, Temp_G02A_CCCP,Temp_G02B_CCCP, Temp_G03A_CCCP, Temp_G03B_CCCP, Temp_G04A_CCCP, Temp_G04B_CCCP, Temp_G05A_CCCP,Temp_G05B_CCCP, Temp_G06A_CCCP, Temp_G06B_CCCP, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Temperatura_Perfiles_CCCP.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Temp01_CCCP, Temp02_CCCP, Temp03_CCCP)
dev.off() 

png(filename = "./03_Imagenes/Temperatura_Perfiles_CCCP.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Temp01_CCCP, Temp02_CCCP, Temp03_CCCP)
dev.off() 

Sal01_CCCP<-grid.arrange(nrow=6, ncol=2,Sal_S01A_CCCP, Sal_S01B_CCCP, Sal_S02A_CCCP,Sal_S02B_CCCP, Sal_S03A_CCCP, Sal_S03B_CCCP, Sal_S04A_CCCP, Sal_S04B_CCCP, Sal_S05A_CCCP,Sal_S05B_CCCP, Sal_S06A_CCCP, Sal_S06B_CCCP, top="Transecto Sanquianga")
Sal02_CCCP<-grid.arrange(nrow=6, ncol=2,Sal_A01A_CCCP, Sal_A01B_CCCP, Sal_A02A_CCCP,Sal_A02B_CCCP, Sal_A03A_CCCP, Sal_A03B_CCCP, Sal_A04A_CCCP, Sal_A04B_CCCP, Sal_A05A_CCCP,Sal_A05B_CCCP, Sal_A06A_CCCP, Sal_A06B_CCCP, top="Transecto Amarales")
Sal03_CCCP<-grid.arrange(nrow=6, ncol=2,Sal_G01A_CCCP, Sal_G01B_CCCP, Sal_G02A_CCCP,Sal_G02B_CCCP, Sal_G03A_CCCP, Sal_G03B_CCCP, Sal_G04A_CCCP, Sal_G04B_CCCP, Sal_G05A_CCCP,Sal_G05B_CCCP, Sal_G06A_CCCP, Sal_G06B_CCCP, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Salinidad_Perfiles_CCCP.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Sal01_CCCP, Sal02_CCCP, Sal03_CCCP)
dev.off()

png(filename = "./03_Imagenes/Salinidad_Perfiles_CCCP.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Sal01_CCCP, Sal02_CCCP, Sal03_CCCP)
dev.off()

Den01_CCCP<-grid.arrange(nrow=6, ncol=2,Den_S01A_CCCP, Den_S01B_CCCP, Den_S02A_CCCP,Den_S02B_CCCP, Den_S03A_CCCP, Den_S03B_CCCP, Den_S04A_CCCP, Den_S04B_CCCP, Den_S05A_CCCP,Den_S05B_CCCP, Den_S06A_CCCP, Den_S06B_CCCP, top="Transecto Sanquianga")
Den02_CCCP<-grid.arrange(nrow=6, ncol=2,Den_A01A_CCCP, Den_A01B_CCCP, Den_A02A_CCCP,Den_A02B_CCCP, Den_A03A_CCCP, Den_A03B_CCCP, Den_A04A_CCCP, Den_A04B_CCCP, Den_A05A_CCCP,Den_A05B_CCCP, Den_A06A_CCCP, Den_A06B_CCCP, top="Transecto Amarales")
Den03_CCCP<-grid.arrange(nrow=6, ncol=2,Den_G01A_CCCP, Den_G01B_CCCP, Den_G02A_CCCP,Den_G02B_CCCP, Den_G03A_CCCP, Den_G03B_CCCP, Den_G04A_CCCP, Den_G04B_CCCP, Den_G05A_CCCP,Den_G05B_CCCP, Den_G06A_CCCP, Den_G06B_CCCP, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Densidad_Perfiles_CCCP.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Den01_CCCP, Den02_CCCP, Den03_CCCP)
dev.off()

png(filename = "./03_Imagenes/Densidad_Perfiles_CCCP.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Den01_CCCP, Den02_CCCP, Den03_CCCP)
dev.off()



Oxi01_CCCP<-grid.arrange(nrow=6, ncol=2,Oxi_A01A_CCCP, Oxi_A01B_CCCP, Oxi_A02A_CCCP,Oxi_A02B_CCCP, Oxi_A03A_CCCP, Oxi_A03B_CCCP, Oxi_A04A_CCCP, Oxi_A04B_CCCP, Oxi_A05A_CCCP,Oxi_A05B_CCCP, Oxi_A06A_CCCP, Oxi_A06B_CCCP, top="Transecto Amarales")
Oxi02_CCCP<-grid.arrange(nrow=6, ncol=2,Oxi_S01A_CCCP, Oxi_S01B_CCCP, Oxi_S02A_CCCP,Oxi_S02B_CCCP, Oxi_S03A_CCCP, Oxi_S03B_CCCP, Oxi_S04A_CCCP, Oxi_S04B_CCCP, Oxi_S05A_CCCP,Oxi_S05B_CCCP, Oxi_S06A_CCCP, Oxi_S06B_CCCP, top="Transecto Sanquianga")
Oxi03_CCCP<-grid.arrange(nrow=6, ncol=2,Oxi_G01A_CCCP, Oxi_G01B_CCCP, Oxi_G02A_CCCP,Oxi_G02B_CCCP, Oxi_G03A_CCCP, Oxi_G03B_CCCP, Oxi_G04A_CCCP, Oxi_G04B_CCCP, Oxi_G05A_CCCP,Oxi_G05B_CCCP, Oxi_G06A_CCCP, Oxi_G06B_CCCP, top="Transecto Guascama")

png(filename = "./03_Imagenes/Oxigeno_Perfiles_CCCP.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Oxi01_CCCP, Oxi02_CCCP, Oxi03_CCCP)
dev.off()
