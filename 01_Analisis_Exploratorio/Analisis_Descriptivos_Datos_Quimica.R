#Titulo: Visualización y Análisis descriptivo - Datos Químicos
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos químicos obtenidos de los análisis en laboratorio.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################


library(tidyverse)
library(gridExtra)
library(gsw)
library(oce)
library(lattice)
library(latticeExtra)
library(GGally)
library(ggplot2)


#Carga de datos química
Datos_Quimica<-read.table("./02_Datos/Quimicos/Datos_Quimica.csv", header = TRUE, sep=",")
as.factor(Datos_Quimica$Transecto)->Datos_Quimica$Transecto
as.numeric(Datos_Quimica$No.Estacion)->Datos_Quimica$No.Estacion
as.factor(Datos_Quimica$Codigo)->Datos_Quimica$Codigo
#Cargade datos físicos asignados solo para la superficie
Datos_Fisica_Sup<-readr::read_csv("./01_Resultados/Fisicos_EstadisticasDescrip_CCCP.csv")


#Uniendo los dos conjuntos de datos

Datos_Totales <- merge(Datos_Quimica,Datos_Fisica_Sup,by="Codigo")
colnames(Datos_Totales_Limpios)

Datos_Totales_Limpios<-Datos_Totales %>% select(
  Codigo,
  ID,
  Transecto,
  No.Estacion,
  Estacion,           
  latitud ,          
  longitud,
  Fecha,              
  Hora ,              
  Marea ,             
  NO2,                
  NO3,               
  PO4,              
  SiO2,
  Clorofila,
  Conductividad,
  Salinidad, 
  pH,
  OD,
  Transparencia,
  SST,
  TSI_Clor,
  TSI_SECCHI,
  Temperatura_mean,  
  Salinidad_mean,
  Oxigeno_mean,
  Densidad_mean,
  Temperatura_median,
  Salinidad_median,
  Oxigeno_median,
  Densidad_median,
  Temperatura_sd,
  Salinidad_sd,
  Oxigeno_sd,
  Densidad_sd,
  Temperatura_min,
  Salinidad_min,
  Oxigeno_min,
  Densidad_min,
  Temperatura_max,
  Salinidad_max,
  Oxigeno_max,
  Densidad_max,
  Profundidad_max  
)
colnames(Datos_Totales_Limpios)



variables<-colnames(Datos_Totales_Limpios[11:44])
etiqueta_para_y<-c(
  "Exp_NO2",
  "Exp_NO3",
  "Exp_PO4",
  "Exp_SiO2",
  "Exp_Clorofila",
  "Conductividad en superficie (mS/cm)",
  "Salinidad en superficie [PSU]",
  "pH en superficie ",
  "Oxígeno disuelto en superficie [mg/L] ",
  "Transparencia (m)",
  "Sólidos Suspendidos Totales (mg/L)",
  "TSI Clorofila (m)",
  "TSI Disco Secchi (m)",
  "Media de la Temperatura en la columna [°C]",
  "Media de la Salinidad en la columna [PSU]",
  "Media del Oxígeno disuelto en la columna [mg/L]",
  "Exp_Densidad",
  "Mediana de la Temperatura en la columna [°C]",
  "Mediana de la Salinidad en la columna [PSU]",
  "Mediana del Oxígeno disuelto en la columna [mg/L]",
  "Exp_Densidad",
  "Des.std de la Temperatura en la columna [°C]",
  "Des.std  de la Salinidad en la columna [PSU]",
  "Des.std  del Oxígeno disuelto en la columna [mg/L]",
  "Exp_Densidad",
  "Mínimo de la Temperatura en la columna [°C]",
  "Mínimo de la Salinidad en la columna [PSU]",
  "Mínimo del Oxígeno disuelto en la columna [mg/L]",
  "Exp_Densidad",
  "Máximo de la Temperatura en la columna [°C]",
  "Máximo de la Salinidad en la columna [PSU]",
  "Máximo del Oxígeno disuelto en la columna [mg/L]",
  "Exp_Densidad",
  "Máximo de la Profundidad [m]"
)



expression( bar(x))





####Creación de la visualización 

####Datos Físicos####

#Boxplot entre mareas

boxplot_Marea<-function(datos,variable, y_etiqueta){
  ggplot(datos, aes(x=Marea, y=variable, color=Marea)) + 
    geom_boxplot()+ 
    stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
    labs( y = y_etiqueta, x = "Marea")+
    scale_x_discrete(limits=c("Alta","Baja"))+
    scale_color_manual(values=c("chocolate1", "deepskyblue"))+
    theme_bw()+
    geom_point(position = position_jitterdodge())+
    theme(legend.position = "none", legend.title = element_blank()) + 
    guides(fill=guide_legend(reverse=TRUE)) 
}

#ciclo para imprimir los objetos para ejecutar la función
#Se debe tomar lo que imprime en la consola, copiarlo en el script y borrar el [1] y las comillas que encierran la expresión
for (i in 1:34){
  print(paste0(variables[i], "_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$",variables[i],", ","'",etiqueta_para_y[i],"'",")"))
}

 NO2_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, 'Exp_NO2')
 NO3_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, 'Exp_NO3')
 PO4_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, 'Exp_PO4')
 SiO2_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, 'Exp_SiO2')
 Clorofila_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, 'Exp_Clorofila')
 Conductividad_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, 'Conductividad en superficie (mS/cm)')
 Salinidad_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, 'Salinidad en superficie [PSU]')
 pH_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, 'pH en superficie ')
 OD_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, 'Oxígeno disuelto en superficie [mg/L] ')
 Transparencia_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, 'Transparencia (m)')
 SST_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, 'Sólidos Suspendidos Totales (mg/L)')
 TSI_Clor_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, 'TSI Clorofila (m)')
 TSI_SECCHI_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, 'TSI Disco Secchi (m)')
 Temperatura_mean_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, 'Media de la Temperatura en la columna [°C]')
 Salinidad_mean_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, 'Media de la Salinidad en la columna [PSU]')
 Oxigeno_mean_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, 'Media del Oxígeno disuelto en la columna [mg/L]')
 Densidad_mean_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, 'Exp_Densidad')
 Temperatura_median_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, 'Mediana de la Temperatura en la columna [°C]')
 Salinidad_median_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, 'Mediana de la Salinidad en la columna [PSU]')
 Oxigeno_median_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, 'Mediana del Oxígeno disuelto en la columna [mg/L]')
 Densidad_median_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, 'Exp_Densidad')
 Temperatura_sd_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, 'Des.std de la Temperatura en la columna [°C]')
 Salinidad_sd_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, 'Des.std de la Salinidad en la columna [PSU]')
 Oxigeno_sd_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, 'Des.std del Oxígeno disuelto en la columna [mg/L]')
 Densidad_sd_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, 'Exp_Densidad')
 Temperatura_min_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, 'Mínimo de la Temperatura en la columna [°C]')
 Salinidad_min_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, 'Mínimo de la Salinidad en la columna [PSU]')
 Oxigeno_min_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, 'Mínimo del Oxígeno disuelto en la columna [mg/L]')
 Densidad_min_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, 'Exp_Densidad')
 Temperatura_max_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, 'Máximo de la Temperatura en la columna [°C]')
 Salinidad_max_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, 'Máximo de la Salinidad en la columna [PSU]')
 Oxigeno_max_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, 'Máximo del Oxígeno disuelto en la columna [mg/L]')
 Densidad_max_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max,'Exp_Densidad' )
 Profundidad_max_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, 'Máximo de la Profundidad [m]')


 for (i in 1:34){
   print(paste0(variables[i], "_boxplot_Mareas"))
 }
 
 
   
   tiff(filename = "./03_Imagenes/boxplot_Mareas_01.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                NO2_boxplot_Mareas, 
                NO3_boxplot_Mareas, 
                PO4_boxplot_Mareas, 
                SiO2_boxplot_Mareas, 
                Clorofila_boxplot_Mareas, 
                Conductividad_boxplot_Mareas, 
                Salinidad_boxplot_Mareas, 
                pH_boxplot_Mareas,
                top="Datos totales")
   dev.off()
 

   tiff(filename = "./03_Imagenes/boxplot_Mareas_02.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                OD_boxplot_Mareas, 
                Transparencia_boxplot_Mareas, 
                SST_boxplot_Mareas, 
                TSI_Clor_boxplot_Mareas, 
                TSI_SECCHI_boxplot_Mareas, 
                Temperatura_mean_boxplot_Mareas, 
                Salinidad_mean_boxplot_Mareas, 
                Oxigeno_mean_boxplot_Mareas,
                top="Datos totales")
   dev.off()
 
   
   tiff(filename = "./03_Imagenes/boxplot_Mareas_03.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                Densidad_mean_boxplot_Mareas, 
                Temperatura_median_boxplot_Mareas, 
                Salinidad_median_boxplot_Mareas, 
                Oxigeno_median_boxplot_Mareas, 
                Densidad_median_boxplot_Mareas, 
                Temperatura_sd_boxplot_Mareas, 
                Salinidad_sd_boxplot_Mareas, 
                Oxigeno_sd_boxplot_Mareas,
                top="Datos totales")
   dev.off()
   
   tiff(filename = "./03_Imagenes/boxplot_Mareas_04.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=5, ncol=2, 
                Densidad_sd_boxplot_Mareas, 
                Temperatura_min_boxplot_Mareas, 
                Salinidad_min_boxplot_Mareas, 
                Oxigeno_min_boxplot_Mareas, 
                Densidad_min_boxplot_Mareas, 
                Temperatura_max_boxplot_Mareas, 
                Salinidad_max_boxplot_Mareas, 
                Oxigeno_max_boxplot_Mareas,
                Densidad_max_boxplot_Mareas,
                Profundidad_max_boxplot_Mareas,
                top="Datos totales")
   dev.off()
 
 png(filename = "./03_Imagenes/boxplot_Mareas_01.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              NO2_boxplot_Mareas, 
              NO3_boxplot_Mareas, 
              PO4_boxplot_Mareas, 
              SiO2_boxplot_Mareas, 
              Clorofila_boxplot_Mareas, 
              Conductividad_boxplot_Mareas, 
              Salinidad_boxplot_Mareas, 
              pH_boxplot_Mareas,
              top="Datos totales")
 dev.off()
 
 
 
 
 
 
 
 
 
 
 
 #####
 
 
boxplot_transecto<-function(datos,variable, y_etiqueta){
  ggplot(datos, aes(x=Transecto, y=variable, color=Marea)) + 
    geom_boxplot()+ 
    stat_summary(fun=mean, aes(y = variable, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
    labs( y = y_etiqueta, x = "Transecto")+
    theme_classic()+
    geom_point(position = position_jitterdodge()) 
}
 
 
 
 NO2_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, 'Exp_NO2')
 NO3_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, 'Exp_NO3')
 PO4_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, 'Exp_PO4')
 SiO2_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, 'Exp_SiO2')
 Clorofila_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, 'Exp_Clorofila')
 Conductividad_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, 'Conductividad en superficie (mS/cm)')
 Salinidad_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, 'Salinidad en superficie [PSU]')
 pH_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, 'pH en superficie ')
 OD_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, 'Oxígeno disuelto en superficie [mg/L] ')
 Transparencia_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, 'Transparencia (m)')
 SST_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, 'Sólidos Suspendidos Totales (mg/L)')
 TSI_Clor_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, 'TSI Clorofila (m)')
 TSI_SECCHI_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, 'TSI Disco Secchi (m)')
 Temperatura_mean_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, 'Media de la Temperatura en la columna [°C]')
 Salinidad_mean_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, 'Media de la Salinidad en la columna [PSU]')
 Oxigeno_mean_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, 'Media del Oxígeno disuelto en la columna [mg/L]')
 Densidad_mean_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, 'Exp_Densidad')
 Temperatura_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, 'Mediana de la Temperatura en la columna [°C]')
 Salinidad_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, 'Mediana de la Salinidad en la columna [PSU]')
 Oxigeno_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, 'Mediana del Oxígeno disuelto en la columna [mg/L]')
 Densidad_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, 'Exp_Densidad')
 Temperatura_sd_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, 'Des.std de la Temperatura en la columna [°C]')
 Salinidad_sd_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, 'Des.std de la Salinidad en la columna [PSU]')
 Oxigeno_sd_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, 'Des.std del Oxígeno disuelto en la columna [mg/L]')
 Densidad_sd_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, 'Exp_Densidad')
 Temperatura_min_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, 'Mínimo de la Temperatura en la columna [°C]')
 Salinidad_min_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, 'Mínimo de la Salinidad en la columna [PSU]')
 Oxigeno_min_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, 'Mínimo del Oxígeno disuelto en la columna [mg/L]')
 Densidad_min_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, 'Exp_Densidad')
 Temperatura_max_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, 'Máximo de la Temperatura en la columna [°C]')
 Salinidad_max_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, 'Máximo de la Salinidad en la columna [PSU]')
 Oxigeno_max_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, 'Máximo del Oxígeno disuelto en la columna [mg/L]')
 Densidad_max_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max, 'Exp_Densidad')
 Profundidad_max_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, 'Máximo de la Profundidad [m]')
 
 
 
 
 
 
 
 
tiff(filename = "01_Datos_Quimica.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica, NO3_Quimica, PO4_Quimica,SiO2_Quimica,Clorofila_Quimica,Salinidad_Quimica,pH_Quimica,OD_Quimica,Transparencia_Quimica,SST_Quimica,
             top="Datos totales")
dev.off()

png(filename = "./03_Imagenes/01_Datos_Quimica.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica, NO3_Quimica, PO4_Quimica,SiO2_Quimica,Clorofila_Quimica,Salinidad_Quimica,pH_Quimica,OD_Quimica,Transparencia_Quimica,SST_Quimica,
             top="Datos totales")
dev.off()


graf_lineas<-function(datos,variable, y_etiqueta){

ggplot(datos, aes(x=No.Estacion, y=variable)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = y_etiqueta)+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
   theme_bw()+
  facet_grid(Marea~Transecto)
}


NO2_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, 'Exp_NO2')
NO3_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, 'Exp_NO3')
PO4_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, 'Exp_PO4')
SiO2_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, 'Exp_SiO2')
Clorofila_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, 'Exp_Clorofila')
Conductividad_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, 'Conductividad en superficie (mS/cm)')
Salinidad_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, 'Salinidad en superficie [PSU]')
pH_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, 'pH en superficie ')
OD_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, 'Oxígeno disuelto en superficie [mg/L] ')
Transparencia_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, 'Transparencia (m)')
SST_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, 'Sólidos Suspendidos Totales (mg/L)')
TSI_Clor_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, 'TSI Clorofila (m)')
TSI_SECCHI_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, 'TSI Disco Secchi (m)')
Temperatura_mean_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, 'Media de la Temperatura en la columna [°C]')
Salinidad_mean_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, 'Media de la Salinidad en la columna [PSU]')
Oxigeno_mean_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, 'Media del Oxígeno disuelto en la columna [mg/L]')
Densidad_mean_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, 'Exp_Densidad')
Temperatura_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, 'Mediana de la Temperatura en la columna [°C]')
Salinidad_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, 'Mediana de la Salinidad en la columna [PSU]')
Oxigeno_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, 'Mediana del Oxígeno disuelto en la columna [mg/L]')
Densidad_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, 'Exp_Densidad')
Temperatura_sd_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, 'Des.std de la Temperatura en la columna [°C]')
Salinidad_sd_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, 'Des.std de la Salinidad en la columna [PSU]')
Oxigeno_sd_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, 'Des.std del Oxígeno disuelto en la columna [mg/L]')
Densidad_sd_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, 'Exp_Densidad')
Temperatura_min_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, 'Mínimo de la Temperatura en la columna [°C]')
Salinidad_min_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, 'Mínimo de la Salinidad en la columna [PSU]')
Oxigeno_min_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, 'Mínimo del Oxígeno disuelto en la columna [mg/L]')
Densidad_min_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, 'Exp_Densidad')
Temperatura_max_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, 'Máximo de la Temperatura en la columna [°C]')
Salinidad_max_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, 'Máximo de la Salinidad en la columna [PSU]')
Oxigeno_max_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, 'Máximo del Oxígeno disuelto en la columna [mg/L]')
Densidad_max_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max, 'Exp_Densidad')
Profundidad_max_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, 'Máximo de la Profundidad [m]')




tiff(filename = "02_Quimica_Linea.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_line, NO3_line, PO4_line,SiO2_line,Clorofila_line,Salinidad_line,pH_line,OD_line,Transparencia_line,SST_line)
dev.off()


tiff(filename = "04_correlaciones_Quimica.tif", width = 30, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
Datos_Quimica %>% ggpairs(columns = 7:17,upper = list(continuous = wrap("cor", method = "spearman")))
dev.off()





#Revisar despues
NO2_WIRE<- interpBarnes(Datos_Quimica$latitud, Datos_Quimica$longitud, Datos_Quimica$NO2)
NO2_WIRE_Graf<-wireframe(NO2_WIRE$zg, xlab="Longitud", ylab=list("Latitud", rot = 0), zlab=list("NO2", rot = 90), main= "Temperatura Superficial", cex=5, zoom=0.8, screen = list(z = 15, x = -50, y = -1), colorkey=TRUE, drape=TRUE)
NO2_contour<-contour(NO2_WIRE$xg, NO2_WIRE$yg, NO2_WIRE$zg, xlab="Lon", ylab="Lat", labcex=1)

filled.contour(NO2_WIRE$zg, plot.axes = {
  axis(NO2_WIRE$xg)
  axis(NO2_WIRE$yg)
  contour(NO2_WIRE$zg, add = TRUE, lwd = 2)
}
)

cols <- hcl.colors(10, "YlOrRd")

contour(NO2_WIRE$zg,
        col = cols)



ggpairs(Datos_Quimica,          # Data frame
        columns = 7:17) # Columns
Datos_Quimica %>% ggpairs(columns = 7:17,upper = list(continuous = wrap("cor", method = "spearman")))



printVar = function(x,y){
  vals = cor.test(x,y,
                  method="spearman")[c("estimate","p.value")]
  names(vals) = c("rho","p")
  paste(names(vals),signif(unlist(vals),2),collapse="\n")
}

my_fn <- function(data, mapping, ...){
  # takes in x and y for each panel
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  colorData <- eval_data_col(data, mapping$colour)
  
  # if you have colors, split according to color group and calculate cor
  
  byGroup =by(data.frame(xData,yData),colorData,function(i)printVar(i[,1],i[,2]))
  byGroup = data.frame(col=names(byGroup),label=as.character(byGroup))
  byGroup$x = 0.5
  byGroup$y = seq(0.8-0.3,0.2,length.out=nrow(byGroup))
  
  #main correlation
  mainCor = printVar(xData,yData)
  
  p <- ggplot(data = data, mapping = mapping) +
    annotate(x=0.5,y=0.8,label=mainCor,geom="text",size=3) +
    geom_text(data=byGroup,inherit.aes=FALSE,
              aes(x=x,y=y,col=col,label=label),size=3)+ 
    theme_void() + ylim(c(0,1))
  p
}

ggpairs(Datos_Quimica,columns = 7:17,
        mapping=ggplot2::aes(colour = Datos_Quimica$Marea),
        axisLabels = "show", 
        upper = list(continuous = my_fn))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
