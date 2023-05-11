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
library(devtools)
library(raster)
library(rgdal)

devtools::install_github("ChrisBermudezR/IctioExPacificoAnalisisPack")
library(IctioExPacificoAnalisisPack)

source("./Funciones/")

#Carga de datos química
Datos_Quimica<-read.table("./02_Datos/Quimicos/Datos_Quimica.csv", header = TRUE, sep=",")
as.factor(Datos_Quimica$Transecto)->Datos_Quimica$Transecto
as.numeric(Datos_Quimica$No.Estacion)->Datos_Quimica$No.Estacion
as.factor(Datos_Quimica$Codigo)->Datos_Quimica$Codigo
#Cargade datos físicos asignados solo para la superficie
Datos_Fisica_Sup_CCCP<-readr::read_csv("./01_Resultados/Fisicos_EstadisticasDescrip_CCCP.csv")
Datos_Fisica_Sup_PNN<-readr::read_csv("./01_Resultados/Fisicos_EstadisticasDescrip_PNN.csv")

#Uniendo los dos conjuntos de datos

Datos_Totales_CCCP <- merge(Datos_Quimica,Datos_Fisica_Sup_CCCP,by="Codigo")
Datos_Totales_PNN <- merge(Datos_Quimica,Datos_Fisica_Sup_PNN,by="Codigo")


write_csv(Datos_Totales_CCCP, "./01_Resultados/Datos_Totales_CCCP.csv", col_names = TRUE)
write_csv(Datos_Totales_PNN, "./01_Resultados/Datos_Totales_PNN.csv", col_names = TRUE)



colnames(Datos_Totales_CCCP)

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

#Expresiones para las leyendas de las variables

Exp_NO2= expression(paste("[NO"[2]^"-","] [",mu,"M]"))
Exp_NO3= expression(paste("[NO"[3]^"-","] [",mu,"M]"))
Exp_PO4= expression(paste("[PO"[4]^"-3","] [",mu,"M]"))
Exp_SiO2= expression(paste("[SiO"[2],"] [",mu,"M]"))
Exp_Clorofila=expression(paste("Clorofila [",mu,"g/L]"))
Exp_Conductividad="Conductividad en superficie (mS/cm)"
Exp_Salinidad="Salinidad en superficie (PSU)"
Exp_pH="pH en superficie "
Exp_OD=expression(paste("Ox. D. en superficie [mg O"[2],"/L]"))
Exp_Transparencia="Transparencia (m)"
Exp_SST="SST [mg/L]"
Exp_TSIClorofila="TSI Clorofila (m)"
Exp_TSIDiscoSecchi="TSI Disco Secchi (m)"
Exp_meanTemp=expression(paste(bar(x)," de la Temp. en prof. (°C)"))
Exp_meansal=expression(paste(bar(x)," de la Sal. en prof. (PSU)"))
Exp_meanoxi=expression(paste(bar(x),"  del Ox. D. en prof. [mg O"[2],"/L]"))
Exp_meanden=expression(paste(bar(x)," de la Den. (kg/m"^3,")"))
Exp_medianTemp=expression(paste("Q"[2]," de la Temp. en prof. (°C)"))
Exp_mediansal=expression(paste("Q"[2]," de la Sal. en prof. (PSU)"))
Exp_medianoxi=expression(paste("Q"[2]," del Ox. D. en prof. [mg O"[2],"/L]"))
Exp_medianden=expression(paste("Q"[2]," de la Den. (kg/m"^3,")"))
Exp_stdTemp=expression(paste("S de la Temp. en prof. (°C)"))
Exp_stdsal=expression(paste("S de la Sal. en prof. (PSU)"))
Exp_stdnoxi=expression(paste("S del Ox. D. en prof. [mg O"[2],"/L]"))
Exp_stdden=expression(paste("S de la Den. (kg/m"^3,")"))
Exp_minTemp=expression(paste("min de la Temp. en prof. (°C)"))
Exp_minsal=expression(paste("min de la Sal. en prof. (PSU)"))
Exp_minnoxi=expression(paste("min del Ox. D. en prof. [mg O"[2],"/L]"))
Exp_minden=expression(paste("min de la Den. (kg/m"^3,")"))
Exp_maxTemp=expression(paste("max de la Temp. en prof. (°C)"))
Exp_maxsal=expression(paste("max de la Sal. en prof. (PSU)"))
Exp_maxnoxi=expression(paste("max del Ox. D. en prof. [mg O"[2],"/L]"))
Exp_maxden=expression(paste("max de la Den. (kg/m"^3,")"))
Exp_maxProf="Máximo de la Profundidad [m]"

Exp_NO2= expression(paste("[NO"[2]^"-","] [",mu,"M]"))
Exp_NO3= expression(paste("[NO"[3]^"-","] [",mu,"M]"))
Exp_PO4= expression(paste("[PO"[4]^"-3","] [",mu,"M]"))
Exp_SiO2= expression(paste("[SiO"[2],"] [",mu,"M]"))
Exp_Clorofila2=expression(paste("[",mu,"g/L]"))
Exp_Conductividad2="(mS/cm)"
Exp_Salinidad2=" (PSU)"
Exp_pH2="pH"
Exp_OD2=expression(paste("[mg O"[2],"/L]"))
Exp_Transparencia2="(m)"
Exp_SST2="[mg/L]"
Exp_TSI_Clor="(m)"
Exp_TSI_SECCHI="(m)"
Exp_Temperatura_mean=expression(paste(" (°C)"))
Exp_Salinidad_mean=expression(paste(" (PSU)"))
Exp_Oxigeno_mean=expression(paste(" [mg O"[2],"/L]"))
Exp_Densidad_mean=expression(paste(" (kg/m"^3,")"))
Exp_Temperatura_median=expression(paste("(°C)"))
Exp_Salinidad_median=expression(paste("(PSU)"))
Exp_Oxigeno_median=expression(paste("[mg O"[2],"/L]"))
Exp_Densidad_median=expression(paste("(kg/m"^3,")"))
Exp_Temperatura_sd=expression(paste("(°C)"))
Exp_Salinidad_sd=expression(paste("(PSU)"))
Exp_Oxigeno_sd=expression(paste("[mg O"[2],"/L]"))
Exp_Densidad_sd=expression(paste("(kg/m"^3,")"))
Exp_Temperatura_min=expression(paste("(°C)"))
Exp_Salinidad_min=expression(paste("(PSU)"))
Exp_Oxigeno_min=expression(paste("[mg O"[2],"/L]"))
Exp_Densidad_min=expression(paste("(kg/m"^3,")"))
Exp_Temperatura_max=expression(paste("(°C)"))
Exp_Salinidad_max=expression(paste("(PSU)"))
Exp_Oxigeno_max=expression(paste("[mg O"[2],"/L]"))
Exp_Densidad_max=expression(paste("(kg/m"^3,")"))
Exp_Profundidad_max="[m]"

####Creación de la visualización 

####Datos Físicos####

#Boxplot entre mareas


#ciclo para imprimir los objetos para ejecutar la función
#Se debe tomar lo que imprime en la consola, copiarlo en el script y borrar el [1] y las comillas que encierran la expresión
for (i in 1:34){
  print(paste0(variables[i], "_boxplot_Mareas<-boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$",variables[i],", ","'",etiqueta_para_y[i],"'",")"))
}

 NO2_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
 NO3_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
 PO4_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
 SiO2_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
 Clorofila_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
 Conductividad_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, Exp_Conductividad)
 Salinidad_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, Exp_Salinidad)
 pH_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
 OD_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
 Transparencia_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
 SST_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
 TSI_Clor_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, Exp_TSIClorofila)
 TSI_SECCHI_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
 Temperatura_mean_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, Exp_meanTemp)
 Salinidad_mean_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, Exp_meansal)
 Oxigeno_mean_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, Exp_meanoxi)
 Densidad_mean_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, Exp_meanden)
 Temperatura_median_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
 Salinidad_median_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
 Oxigeno_median_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
 Densidad_median_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
 Temperatura_sd_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, Exp_stdTemp)
 Salinidad_sd_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, Exp_stdsal)
 Oxigeno_sd_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, Exp_stdnoxi)
 Densidad_sd_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, Exp_stdden)
 Temperatura_min_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, Exp_minTemp)
 Salinidad_min_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, Exp_minsal)
 Oxigeno_min_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, Exp_minnoxi)
 Densidad_min_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, Exp_minden)
 Temperatura_max_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, Exp_maxTemp)
 Salinidad_max_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, Exp_maxsal)
 Oxigeno_max_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, Exp_maxnoxi)
 Densidad_max_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max,Exp_maxden)
 Profundidad_max_boxplot_Mareas<-IctioExPacificoAnalisisPack::boxplot_Marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, Exp_maxProf)


 for (i in 1:34){
   print(paste0(variables[i], "_boxplot_Mareas"))
 }
 
 
   
tiff(filename = "./03_Imagenes/boxplot_Mareas_01.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
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
 

tiff(filename = "./03_Imagenes/boxplot_Mareas_02.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
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
 
   
   tiff(filename = "./03_Imagenes/boxplot_Mareas_03.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
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
   
   tiff(filename = "./03_Imagenes/boxplot_Mareas_04.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
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
 
 

png(filename = "./03_Imagenes/boxplot_Mareas_02.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
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
 
   
png(filename = "./03_Imagenes/boxplot_Mareas_03.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
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
   
png(filename = "./03_Imagenes/boxplot_Mareas_04.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
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
 
 
 
 
 
 
 
 
 
 #####
 
 NO2_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
 NO3_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
 PO4_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
 SiO2_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
 Clorofila_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
 Conductividad_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, Exp_Conductividad)
 Salinidad_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, Exp_Salinidad)
 pH_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
 OD_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
 Transparencia_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
 SST_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
 TSI_Clor_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, Exp_TSIClorofila)
 TSI_SECCHI_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
 Temperatura_mean_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, Exp_meanTemp)
 Salinidad_mean_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, Exp_meansal)
 Oxigeno_mean_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, Exp_meanoxi)
 Densidad_mean_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, Exp_meanden)
 Temperatura_median_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
 Salinidad_median_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
 Oxigeno_median_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
 Densidad_median_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
 Temperatura_sd_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, Exp_stdTemp)
 Salinidad_sd_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, Exp_stdsal)
 Oxigeno_sd_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, Exp_stdnoxi)
 Densidad_sd_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, Exp_stdden)
 Temperatura_min_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, Exp_minTemp)
 Salinidad_min_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, Exp_minsal)
 Oxigeno_min_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, Exp_minnoxi)
 Densidad_min_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, Exp_minden)
 Temperatura_max_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, Exp_maxTemp)
 Salinidad_max_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, Exp_maxsal)
 Oxigeno_max_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, Exp_maxnoxi)
 Densidad_max_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max, Exp_maxden)
 Profundidad_max_boxplot_transecto<-IctioExPacificoAnalisisPack::boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, Exp_maxProf)
 
 
 
 
  
tiff(filename = "./03_Imagenes/boxplot_transecto_01.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                NO2_boxplot_transecto, 
                NO3_boxplot_transecto, 
                PO4_boxplot_transecto, 
                SiO2_boxplot_transecto, 
                Clorofila_boxplot_transecto, 
                Conductividad_boxplot_transecto, 
                Salinidad_boxplot_transecto, 
                pH_boxplot_transecto,
                top="Datos totales")
   dev.off()
 

tiff(filename = "./03_Imagenes/boxplot_transecto_02.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                OD_boxplot_transecto, 
                Transparencia_boxplot_transecto, 
                SST_boxplot_transecto, 
                TSI_Clor_boxplot_transecto, 
                TSI_SECCHI_boxplot_transecto, 
                Temperatura_mean_boxplot_transecto, 
                Salinidad_mean_boxplot_transecto, 
                Oxigeno_mean_boxplot_transecto,
                top="Datos totales")
   dev.off()
 
   
   tiff(filename = "./03_Imagenes/boxplot_transecto_03.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                Densidad_mean_boxplot_transecto, 
                Temperatura_median_boxplot_transecto, 
                Salinidad_median_boxplot_transecto, 
                Oxigeno_median_boxplot_transecto, 
                Densidad_median_boxplot_transecto, 
                Temperatura_sd_boxplot_transecto, 
                Salinidad_sd_boxplot_transecto, 
                Oxigeno_sd_boxplot_transecto,
                top="Datos totales")
   dev.off()
   
   tiff(filename = "./03_Imagenes/boxplot_transecto_04.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=5, ncol=2, 
                Densidad_sd_boxplot_transecto, 
                Temperatura_min_boxplot_transecto, 
                Salinidad_min_boxplot_transecto, 
                Oxigeno_min_boxplot_transecto, 
                Densidad_min_boxplot_transecto, 
                Temperatura_max_boxplot_transecto, 
                Salinidad_max_boxplot_transecto, 
                Oxigeno_max_boxplot_transecto,
                Densidad_max_boxplot_transecto,
                Profundidad_max_boxplot_transecto,
                top="Datos totales")
   dev.off()
 
 png(filename = "./03_Imagenes/boxplot_transecto_01.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              NO2_boxplot_transecto, 
              NO3_boxplot_transecto, 
              PO4_boxplot_transecto, 
              SiO2_boxplot_transecto, 
              Clorofila_boxplot_transecto, 
              Conductividad_boxplot_transecto, 
              Salinidad_boxplot_transecto, 
              pH_boxplot_transecto,
              top="Datos totales")
 dev.off()
 
 

png(filename = "./03_Imagenes/boxplot_transecto_02.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                OD_boxplot_transecto, 
                Transparencia_boxplot_transecto, 
                SST_boxplot_transecto, 
                TSI_Clor_boxplot_transecto, 
                TSI_SECCHI_boxplot_transecto, 
                Temperatura_mean_boxplot_transecto, 
                Salinidad_mean_boxplot_transecto, 
                Oxigeno_mean_boxplot_transecto,
                top="Datos totales")
   dev.off()
 
   
png(filename = "./03_Imagenes/boxplot_transecto_03.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                Densidad_mean_boxplot_transecto, 
                Temperatura_median_boxplot_transecto, 
                Salinidad_median_boxplot_transecto, 
                Oxigeno_median_boxplot_transecto, 
                Densidad_median_boxplot_transecto, 
                Temperatura_sd_boxplot_transecto, 
                Salinidad_sd_boxplot_transecto, 
                Oxigeno_sd_boxplot_transecto,
                top="Datos totales")
   dev.off()
   
png(filename = "./03_Imagenes/boxplot_transecto_04.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=5, ncol=2, 
                Densidad_sd_boxplot_transecto, 
                Temperatura_min_boxplot_transecto, 
                Salinidad_min_boxplot_transecto, 
                Oxigeno_min_boxplot_transecto, 
                Densidad_min_boxplot_transecto, 
                Temperatura_max_boxplot_transecto, 
                Salinidad_max_boxplot_transecto, 
                Oxigeno_max_boxplot_transecto,
                Densidad_max_boxplot_transecto,
                Profundidad_max_boxplot_transecto,
                top="Datos totales")
   dev.off()
 
 
 
 

NO2_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
NO3_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
PO4_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
SiO2_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
Clorofila_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
Conductividad_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Conductividad, Exp_Conductividad)
Salinidad_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad, Exp_Salinidad)
pH_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
OD_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
Transparencia_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
SST_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
TSI_Clor_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_Clor, Exp_TSIClorofila)
TSI_SECCHI_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
Temperatura_mean_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_mean, Exp_meanTemp)
Salinidad_mean_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_mean, Exp_meansal)
Oxigeno_mean_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_mean, Exp_meanoxi)
Densidad_mean_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_mean, Exp_meanden)
Temperatura_median_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
Salinidad_median_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
Oxigeno_median_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
Densidad_median_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
Temperatura_sd_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_sd, Exp_stdTemp)
Salinidad_sd_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_sd, Exp_stdsal)
Oxigeno_sd_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_sd, Exp_stdnoxi)
Densidad_sd_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_sd, Exp_stdden)
Temperatura_min_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_min, Exp_minTemp)
Salinidad_min_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_min, Exp_minsal)
Oxigeno_min_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_min, Exp_minnoxi)
Densidad_min_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_min, Exp_minden)
Temperatura_max_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_max, Exp_maxTemp)
Salinidad_max_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_max, Exp_maxsal)
Oxigeno_max_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_max, Exp_maxnoxi)
Densidad_max_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_max, Exp_maxden)
Profundidad_max_graf_lineas<-IctioExPacificoAnalisisPack::graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Profundidad_max, Exp_maxProf)



   
tiff(filename = "./03_Imagenes/graf_lineas_01.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                NO2_graf_lineas, 
                NO3_graf_lineas, 
                PO4_graf_lineas, 
                SiO2_graf_lineas, 
                Clorofila_graf_lineas, 
                Conductividad_graf_lineas, 
                Salinidad_graf_lineas, 
                pH_graf_lineas,
                top="Datos totales")
   dev.off()
 

tiff(filename = "./03_Imagenes/graf_lineas_02.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                OD_graf_lineas, 
                Transparencia_graf_lineas, 
                SST_graf_lineas, 
                TSI_Clor_graf_lineas, 
                TSI_SECCHI_graf_lineas, 
                Temperatura_mean_graf_lineas, 
                Salinidad_mean_graf_lineas, 
                Oxigeno_mean_graf_lineas,
                top="Datos totales")
   dev.off()
 
   
   tiff(filename = "./03_Imagenes/graf_lineas_03.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=4, ncol=2, 
                Densidad_mean_graf_lineas, 
                Temperatura_median_graf_lineas, 
                Salinidad_median_graf_lineas, 
                Oxigeno_median_graf_lineas, 
                Densidad_median_graf_lineas, 
                Temperatura_sd_graf_lineas, 
                Salinidad_sd_graf_lineas, 
                Oxigeno_sd_graf_lineas,
                top="Datos totales")
   dev.off()
   
   tiff(filename = "./03_Imagenes/graf_lineas_04.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
   grid.arrange(nrow=5, ncol=2, 
                Densidad_sd_graf_lineas, 
                Temperatura_min_graf_lineas, 
                Salinidad_min_graf_lineas, 
                Oxigeno_min_graf_lineas, 
                Densidad_min_graf_lineas, 
                Temperatura_max_graf_lineas, 
                Salinidad_max_graf_lineas, 
                Oxigeno_max_graf_lineas,
                Densidad_max_graf_lineas,
                Profundidad_max_graf_lineas,
                top="Datos totales")
   dev.off()
 
 png(filename = "./03_Imagenes/graf_lineas_01.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              NO2_graf_lineas, 
              NO3_graf_lineas, 
              PO4_graf_lineas, 
              SiO2_graf_lineas, 
              Clorofila_graf_lineas, 
              Conductividad_graf_lineas, 
              Salinidad_graf_lineas, 
              pH_graf_lineas,
              top="Datos totales")
 dev.off()
 
 

png(filename = "./03_Imagenes/graf_lineas_02.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                OD_graf_lineas, 
                Transparencia_graf_lineas, 
                SST_graf_lineas, 
                TSI_Clor_graf_lineas, 
                TSI_SECCHI_graf_lineas, 
                Temperatura_mean_graf_lineas, 
                Salinidad_mean_graf_lineas, 
                Oxigeno_mean_graf_lineas,
                top="Datos totales")
   dev.off()
 
   
png(filename = "./03_Imagenes/graf_lineas_03.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=4, ncol=2, 
                Densidad_mean_graf_lineas, 
                Temperatura_median_graf_lineas, 
                Salinidad_median_graf_lineas, 
                Oxigeno_median_graf_lineas, 
                Densidad_median_graf_lineas, 
                Temperatura_sd_graf_lineas, 
                Salinidad_sd_graf_lineas, 
                Oxigeno_sd_graf_lineas,
                top="Datos totales")
   dev.off()
   
png(filename = "./03_Imagenes/graf_lineas_04.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
   grid.arrange(nrow=5, ncol=2, 
                Densidad_sd_graf_lineas, 
                Temperatura_min_graf_lineas, 
                Salinidad_min_graf_lineas, 
                Oxigeno_min_graf_lineas, 
                Densidad_min_graf_lineas, 
                Temperatura_max_graf_lineas, 
                Salinidad_max_graf_lineas, 
                Oxigeno_max_graf_lineas,
                Densidad_max_graf_lineas,
                Profundidad_max_graf_lineas,
                top="Datos totales")
   dev.off()
 
 
 ################################################

   
   marea_alta<-Datos_Totales_Limpios%>% filter(Marea=="Alta")
   marea_baja<-Datos_Totales_Limpios%>% filter(Marea=="Baja")
   
   costa<-readOGR("../SIG_Datos/costa.shp")
   rios<-readOGR("../SIG_Datos/rios_wgs84.shp")
   estaciones<-readOGR("../SIG_Datos/estaciones.shp")
   areas_protegidas<-readOGR("../SIG_Datos/areas_protegidas.shp")
 

 rasterizar_Variable<-function(nombre_variable,longitud, latitud, variable, marea, leyenda){
  assign("WIRE",interpBarnes(longitud, latitud, variable), envir = parent.frame())
  assign(paste0("pts.grid"),expand.grid(Longitud=WIRE$x, Latitud=WIRE$y), envir = parent.frame())
  assign(paste0("pts.grid"), mutate(pts.grid, variable=as.vector(WIRE$zg)), envir = parent.frame())
  assign(paste0("export"),  raster::rasterFromXYZ(pts.grid), envir = parent.frame())
  assign(paste0(nombre_variable,"_",marea,"_pts.grid"),  rasterFromXYZ(pts.grid), envir = parent.frame())
  raster::writeRaster(export, filename=paste("../SIG_Datos/grids/",nombre_variable,"_",marea, ".tif", sep = ""),overwrite=TRUE)
  
  ggplot(pts.grid, aes(Longitud, Latitud)) +
    geom_raster(aes(fill = variable))+
    geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="grey", fill="grey") +
    geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="dodgerblue", fill="dodgerblue") +
    coord_sf(xlim = c(-78.4055, -78.217), ylim = c(2.55, 2.853), expand = FALSE)+   
    geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent") +
    theme_bw()+
    scale_fill_gradientn(colours = terrain.colors(7))+
    geom_point(data=marea_altagra, aes(x= longitud, y= latitud))+
    labs(fill=leyenda)
    #ggrepel::geom_text_repel(data=marea_alta,aes(x=longitud, y=latitud,label = Estacion),box.padding   = 0.3, direction = "x")
}

 
 rasterizar_Variable("NO2", marea_baja$longitud, marea_baja$latitud, marea_baja$NO2, "Baja",Exp_NO2)
 

 for (i in colnames(marea_baja)[11:44]){
   print(paste0(i, "_BajaGrid<-rasterizar_Variable('",i,"', marea_baja$longitud, marea_baja$latitud, marea_baja$",i,", 'Baja',  Exp_", i, ")"))
   
 }
 
 
 NO2_BajaGrid<-rasterizar_Variable('NO2', marea_baja$longitud, marea_baja$latitud, marea_baja$NO2, 'Baja',  Exp_NO2)
 NO3_BajaGrid<-rasterizar_Variable('NO3', marea_baja$longitud, marea_baja$latitud, marea_baja$NO3, 'Baja',  Exp_NO3)
 PO4_BajaGrid<-rasterizar_Variable('PO4', marea_baja$longitud, marea_baja$latitud, marea_baja$PO4, 'Baja',  Exp_PO4)
 SiO2_BajaGrid<-rasterizar_Variable('SiO2', marea_baja$longitud, marea_baja$latitud, marea_baja$SiO2, 'Baja',  Exp_SiO2)
 Clorofila_BajaGrid<-rasterizar_Variable('Clorofila', marea_baja$longitud, marea_baja$latitud, marea_baja$Clorofila, 'Baja',  Exp_Clorofila2)
 Conductividad_BajaGrid<-rasterizar_Variable('Conductividad', marea_baja$longitud, marea_baja$latitud, marea_baja$Conductividad, 'Baja',  Exp_Conductividad2)
 Salinidad_BajaGrid<-rasterizar_Variable('Salinidad', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad, 'Baja',  Exp_Salinidad2)
 pH_BajaGrid<-rasterizar_Variable('pH', marea_baja$longitud, marea_baja$latitud, marea_baja$pH, 'Baja',  Exp_pH2)
 OD_BajaGrid<-rasterizar_Variable('OD', marea_baja$longitud, marea_baja$latitud, marea_baja$OD, 'Baja',  Exp_OD2)
 Transparencia_BajaGrid<-rasterizar_Variable('Transparencia', marea_baja$longitud, marea_baja$latitud, marea_baja$Transparencia, 'Baja',  Exp_Transparencia2)
 SST_BajaGrid<-rasterizar_Variable('SST', marea_baja$longitud, marea_baja$latitud, marea_baja$SST, 'Baja',  Exp_SST2)
 TSI_Clor_BajaGrid<-rasterizar_Variable('TSI_Clor', marea_baja$longitud, marea_baja$latitud, marea_baja$TSI_Clor, 'Baja',  Exp_TSI_Clor)
 TSI_SECCHI_BajaGrid<-rasterizar_Variable('TSI_SECCHI', marea_baja$longitud, marea_baja$latitud, marea_baja$TSI_SECCHI, 'Baja',  Exp_TSI_SECCHI)
 Temperatura_mean_BajaGrid<-rasterizar_Variable('Temperatura_mean', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_mean, 'Baja',  Exp_Temperatura_mean)
 Salinidad_mean_BajaGrid<-rasterizar_Variable('Salinidad_mean', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_mean, 'Baja',  Exp_Salinidad_mean)
 Oxigeno_mean_BajaGrid<-rasterizar_Variable('Oxigeno_mean', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_mean, 'Baja',  Exp_Oxigeno_mean)
 Densidad_mean_BajaGrid<-rasterizar_Variable('Densidad_mean', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_mean, 'Baja',  Exp_Densidad_mean)
 Temperatura_median_BajaGrid<-rasterizar_Variable('Temperatura_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_median, 'Baja',  Exp_Temperatura_median)
 Salinidad_median_BajaGrid<-rasterizar_Variable('Salinidad_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_median, 'Baja',  Exp_Salinidad_median)
 Oxigeno_median_BajaGrid<-rasterizar_Variable('Oxigeno_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_median, 'Baja',  Exp_Oxigeno_median)
 Densidad_median_BajaGrid<-rasterizar_Variable('Densidad_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_median, 'Baja',  Exp_Densidad_median)
 Temperatura_sd_BajaGrid<-rasterizar_Variable('Temperatura_sd', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_sd, 'Baja',  Exp_Temperatura_sd)
 Salinidad_sd_BajaGrid<-rasterizar_Variable('Salinidad_sd', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_sd, 'Baja',  Exp_Salinidad_sd)
 Oxigeno_sd_BajaGrid<-rasterizar_Variable('Oxigeno_sd', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_sd, 'Baja',  Exp_Oxigeno_sd)
 Densidad_sd_BajaGrid<-rasterizar_Variable('Densidad_sd', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_sd, 'Baja',  Exp_Densidad_sd)
 Temperatura_min_BajaGrid<-rasterizar_Variable('Temperatura_min', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_min, 'Baja',  Exp_Temperatura_min)
 Salinidad_min_BajaGrid<-rasterizar_Variable('Salinidad_min', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_min, 'Baja',  Exp_Salinidad_min)
 Oxigeno_min_BajaGrid<-rasterizar_Variable('Oxigeno_min', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_min, 'Baja',  Exp_Oxigeno_min)
 Densidad_min_BajaGrid<-rasterizar_Variable('Densidad_min', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_min, 'Baja',  Exp_Densidad_min)
 Temperatura_max_BajaGrid<-rasterizar_Variable('Temperatura_max', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_max, 'Baja',  Exp_Temperatura_max)
 Salinidad_max_BajaGrid<-rasterizar_Variable('Salinidad_max', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_max, 'Baja',  Exp_Salinidad_max)
 Oxigeno_max_BajaGrid<-rasterizar_Variable('Oxigeno_max', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_max, 'Baja',  Exp_Oxigeno_max)
 Densidad_max_BajaGrid<-rasterizar_Variable('Densidad_max', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_max, 'Baja',  Exp_Densidad_max)
 Profundidad_max_BajaGrid<-rasterizar_Variable('Profundidad_max', marea_baja$longitud, marea_baja$latitud, marea_baja$Profundidad_max, 'Baja',  Exp_Profundidad_max)

 
 NO2_AltaGrid<-rasterizar_Variable('NO2', marea_alta$longitud, marea_alta$latitud, marea_alta$NO2, 'Alta',  Exp_NO2)
 NO3_AltaGrid<-rasterizar_Variable('NO3', marea_alta$longitud, marea_alta$latitud, marea_alta$NO3, 'Alta',  Exp_NO3)
 PO4_AltaGrid<-rasterizar_Variable('PO4', marea_alta$longitud, marea_alta$latitud, marea_alta$PO4, 'Alta',  Exp_PO4)
 SiO2_AltaGrid<-rasterizar_Variable('SiO2', marea_alta$longitud, marea_alta$latitud, marea_alta$SiO2, 'Alta',  Exp_SiO2)
 Clorofila_AltaGrid<-rasterizar_Variable('Clorofila', marea_alta$longitud, marea_alta$latitud, marea_alta$Clorofila, 'Alta',  Exp_Clorofila2)
 Conductividad_AltaGrid<-rasterizar_Variable('Conductividad', marea_alta$longitud, marea_alta$latitud, marea_alta$Conductividad, 'Alta',  Exp_Conductividad2)
 Salinidad_AltaGrid<-rasterizar_Variable('Salinidad', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad, 'Alta',  Exp_Salinidad2)
 pH_AltaGrid<-rasterizar_Variable('pH', marea_alta$longitud, marea_alta$latitud, marea_alta$pH, 'Alta',  Exp_pH2)
 OD_AltaGrid<-rasterizar_Variable('OD', marea_alta$longitud, marea_alta$latitud, marea_alta$OD, 'Alta',  Exp_OD2)
 Transparencia_AltaGrid<-rasterizar_Variable('Transparencia', marea_alta$longitud, marea_alta$latitud, marea_alta$Transparencia, 'Alta',  Exp_Transparencia2)
 SST_AltaGrid<-rasterizar_Variable('SST', marea_alta$longitud, marea_alta$latitud, marea_alta$SST, 'Alta',  Exp_SST2)
 TSI_Clor_AltaGrid<-rasterizar_Variable('TSI_Clor', marea_alta$longitud, marea_alta$latitud, marea_alta$TSI_Clor, 'Alta',  Exp_TSI_Clor)
 TSI_SECCHI_AltaGrid<-rasterizar_Variable('TSI_SECCHI', marea_alta$longitud, marea_alta$latitud, marea_alta$TSI_SECCHI, 'Alta',  Exp_TSI_SECCHI)
 Temperatura_mean_AltaGrid<-rasterizar_Variable('Temperatura_mean', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_mean, 'Alta',  Exp_Temperatura_mean)
 Salinidad_mean_AltaGrid<-rasterizar_Variable('Salinidad_mean', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_mean, 'Alta',  Exp_Salinidad_mean)
 Oxigeno_mean_AltaGrid<-rasterizar_Variable('Oxigeno_mean', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_mean, 'Alta',  Exp_Oxigeno_mean)
 Densidad_mean_AltaGrid<-rasterizar_Variable('Densidad_mean', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_mean, 'Alta',  Exp_Densidad_mean)
 Temperatura_median_AltaGrid<-rasterizar_Variable('Temperatura_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_median, 'Alta',  Exp_Temperatura_median)
 Salinidad_median_AltaGrid<-rasterizar_Variable('Salinidad_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_median, 'Alta',  Exp_Salinidad_median)
 Oxigeno_median_AltaGrid<-rasterizar_Variable('Oxigeno_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_median, 'Alta',  Exp_Oxigeno_median)
 Densidad_median_AltaGrid<-rasterizar_Variable('Densidad_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_median, 'Alta',  Exp_Densidad_median)
 Temperatura_sd_AltaGrid<-rasterizar_Variable('Temperatura_sd', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_sd, 'Alta',  Exp_Temperatura_sd)
 Salinidad_sd_AltaGrid<-rasterizar_Variable('Salinidad_sd', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_sd, 'Alta',  Exp_Salinidad_sd)
 Oxigeno_sd_AltaGrid<-rasterizar_Variable('Oxigeno_sd', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_sd, 'Alta',  Exp_Oxigeno_sd)
 Densidad_sd_AltaGrid<-rasterizar_Variable('Densidad_sd', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_sd, 'Alta',  Exp_Densidad_sd)
 Temperatura_min_AltaGrid<-rasterizar_Variable('Temperatura_min', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_min, 'Alta',  Exp_Temperatura_min)
 Salinidad_min_AltaGrid<-rasterizar_Variable('Salinidad_min', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_min, 'Alta',  Exp_Salinidad_min)
 Oxigeno_min_AltaGrid<-rasterizar_Variable('Oxigeno_min', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_min, 'Alta',  Exp_Oxigeno_min)
 Densidad_min_AltaGrid<-rasterizar_Variable('Densidad_min', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_min, 'Alta',  Exp_Densidad_min)
 Temperatura_max_AltaGrid<-rasterizar_Variable('Temperatura_max', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_max, 'Alta',  Exp_Temperatura_max)
 Salinidad_max_AltaGrid<-rasterizar_Variable('Salinidad_max', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_max, 'Alta',  Exp_Salinidad_max)
 Oxigeno_max_AltaGrid<-rasterizar_Variable('Oxigeno_max', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_max, 'Alta',  Exp_Oxigeno_max)
 Densidad_max_AltaGrid<-rasterizar_Variable('Densidad_max', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_max, 'Alta',  Exp_Densidad_max)
 Profundidad_max_AltaGrid<-rasterizar_Variable('Profundidad_max', marea_alta$longitud, marea_alta$latitud, marea_alta$Profundidad_max, 'Alta',  Exp_Profundidad_max)

 tiff(filename = "./03_Imagenes/grid_grafica_01.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              NO2_AltaGrid, NO2_BajaGrid,
              NO3_AltaGrid, NO3_BajaGrid,
              PO4_AltaGrid, PO4_BajaGrid,
              SiO2_AltaGrid, SiO2_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_02.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Clorofila_AltaGrid, Clorofila_BajaGrid,
              Conductividad_AltaGrid, Conductividad_BajaGrid,
              Salinidad_AltaGrid, Salinidad_BajaGrid,
              pH_AltaGrid, pH_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_03.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              OD_AltaGrid, OD_BajaGrid,
              Transparencia_AltaGrid, Transparencia_BajaGrid,
              SST_AltaGrid, SST_BajaGrid,
              TSI_Clor_AltaGrid, TSI_Clor_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_04.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              TSI_SECCHI_AltaGrid, TSI_SECCHI_BajaGrid,
              Temperatura_mean_AltaGrid, Temperatura_mean_BajaGrid,
              Salinidad_mean_AltaGrid, Salinidad_mean_BajaGrid,
              Oxigeno_mean_AltaGrid, Oxigeno_mean_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_05.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Densidad_mean_AltaGrid, Densidad_mean_BajaGrid,
              Temperatura_median_AltaGrid, Temperatura_median_BajaGrid,
              Salinidad_median_AltaGrid, Salinidad_median_BajaGrid,
              Oxigeno_median_AltaGrid, Oxigeno_median_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_06.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Densidad_median_AltaGrid, Densidad_median_BajaGrid,
              Temperatura_sd_AltaGrid, Temperatura_sd_BajaGrid,
              Salinidad_sd_AltaGrid, Salinidad_sd_BajaGrid,
              Oxigeno_sd_AltaGrid, Oxigeno_sd_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_07.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Densidad_sd_AltaGrid, Densidad_sd_BajaGrid,
              Temperatura_min_AltaGrid, Temperatura_min_BajaGrid,
              Salinidad_min_AltaGrid, Salinidad_min_BajaGrid,
              Oxigeno_min_AltaGrid, Oxigeno_min_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_08.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Densidad_min_AltaGrid, Densidad_min_BajaGrid,
              Temperatura_max_AltaGrid, Temperatura_max_BajaGrid,
              Salinidad_max_AltaGrid, Salinidad_max_BajaGrid,
              Oxigeno_max_AltaGrid, Oxigeno_max_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 tiff(filename = "./03_Imagenes/grid_grafica_09.tif", width = 30, height = 50, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
 grid.arrange(nrow=4, ncol=2, 
              Densidad_max_AltaGrid, Densidad_max_BajaGrid,
              Profundidad_max_AltaGrid, Profundidad_max_BajaGrid,
              top="mareas", left="Alta", right="baja")
 dev.off()
 
 
 
printVar = function(x,y){
  vals = cor.test(x,y,
                  method="spearman")[c("estimate","p.value")]
  names(vals) = c("rho","p")
  paste(names(vals),signif(unlist(vals),2),collapse="/n")
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




