#Titulo: Visualización y Análisis descriptivo - PNN
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos físicos obtenidos de la sonda CastAWAY General Oceanics 19v PLUS.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################


library(tidyverse)
library(gridExtra)
library(gsw)
library(oce)
library(pastecs)
library(devtools)



devtools::install_github("ChrisBermudezR/IctioExPacificoAnalisisPack")
library(IctioExPacificoAnalisisPack)

#####Manejo del conjunto de datos####
#Cargar el archivo de datos
Datos_CTDO_PNN<-readr::read_csv("./02_Datos/Fisicos/Datos_CTDO_PNN.csv")
#Asignar factores al evento de muestreo que en este caso está descrito por la variable "Codigo"
Datos_CTDO_PNN$No.Estacion<-as.factor(Datos_CTDO_PNN$No.Estacion)
Datos_CTDO_PNN$Marea<-as.factor(Datos_CTDO_PNN$Marea)
Datos_CTDO_PNN$Codigo<-as.factor(Datos_CTDO_PNN$Codigo)
Datos_CTDO_PNN <- Datos_CTDO_PNN %>% group_by(Codigo)

####Estadistica descriptiva####

EstadisticasDescrip<-Datos_CTDO_PNN %>% summarise_each(funs(mean(., na.rm = TRUE),   median(., na.rm = TRUE),n(),sd(., na.rm = TRUE), min(., na.rm = TRUE),max(., na.rm = TRUE)), Temperatura, Salinidad,  Densidad, Profundidad)

write_csv(EstadisticasDescrip, "./01_Resultados/Fisicos_EstadisticasDescrip_PNN.csv", col_names = TRUE)




Temperatura_Total_PNN<-IctioExPacificoAnalisisPack::boxplot_CTDO(Datos_CTDO_PNN, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Temperatura, "Transectos", "Temperatura (°C)")
Salinidad_Total_PNN<-IctioExPacificoAnalisisPack::boxplot_CTDO(Datos_CTDO_PNN, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Salinidad, "Transectos", "Salinidad (PSU)")
Densidad_Total_PNN<-IctioExPacificoAnalisisPack::boxplot_CTDO(Datos_CTDO_PNN, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Densidad, "Transectos", "Densidad (KG/m3)")





tiff(filename = "./03_Imagenes/01_Datos_Totales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2, Temperatura_Total_PNN, 
             Salinidad_Total_PNN,Densidad_Total_PNN,
             top="Datos totales")
dev.off()



png(filename = "./03_Imagenes/01_Datos_Totales_PNN.png", width = 20, height = 30, units = "cm", pointsize = 15, res = 300)
grid.arrange(nrow=2, ncol=2, Temperatura_Total_PNN, 
             Salinidad_Total_PNN,Densidad_Total_PNN, 
             top="Datos totales")
dev.off()



Temperatura_Hist_PNN<-IctioExPacificoAnalisisPack::histograma_Transecto(Datos_CTDO_PNN, Datos_CTDO_PNN$Marea, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Temperatura, "Temperatura (°C)")
Salinidad_Hist_PNN<-IctioExPacificoAnalisisPack::histograma_Transecto(Datos_CTDO_PNN, Datos_CTDO_PNN$Marea, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Salinidad, "Salinidad (PSU)")
Densidad_Hist_PNN<-IctioExPacificoAnalisisPack::histograma_Transecto(Datos_CTDO_PNN, Datos_CTDO_PNN$Marea, Datos_CTDO_PNN$Transecto, Datos_CTDO_PNN$Densidad, "Densidad (KG/m3)")



tiff(filename = "./03_Imagenes/02_Histogramas_PNN.tif", width = 40, height = 30, units = "cm", pointsize = 30, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_PNN, Salinidad_Hist_PNN, Densidad_Hist_PNN)
dev.off()

png(filename = "./03_Imagenes/02_Histogramas_PNN.png", width = 40, height = 30, units = "cm", pointsize = 30, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_PNN, Salinidad_Hist_PNN, Densidad_Hist_PNN, Oxigeno_Hist_PNN)
dev.off()



Temperatura_boxplot_PNN<-IctioExPacificoAnalisisPack::IctioExPacificoAnalisisPack::boxplot_profundidad(Datos_CTDO_PNN,Datos_CTDO_PNN$No.Estacion,Datos_CTDO_PNN$Temperatura,"Temperatura (°C)")
Salinidad_boxplot_PNN<-IctioExPacificoAnalisisPack::boxplot_profundidad(Datos_CTDO_PNN,Datos_CTDO_PNN$No.Estacion,Datos_CTDO_PNN$Salinidad,"Salinidad (PSU)")
Densidad_boxplot_PNN<-IctioExPacificoAnalisisPack::boxplot_profundidad(Datos_CTDO_PNN,Datos_CTDO_PNN$No.Estacion,Datos_CTDO_PNN$Densidad,"Densidad (KG/m3)")




tiff(filename = "./03_Imagenes/03_Boxplot_PNN.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_PNN, Salinidad_boxplot_PNN, 
             Densidad_boxplot_PNN)
dev.off()

png(filename = "./03_Imagenes/03_Boxplot_PNN.png", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_PNN, Salinidad_boxplot_PNN, 
             Densidad_boxplot_PNN)
dev.off()



#Crear una lista con los niveles del código de evento de muestreo
niveles_Codigo<-as.list(levels(Datos_CTDO_PNN$Codigo))

#Ciclo para asignar objetos a un filtro de datos por cada evento de muestreo
for(i in seq_along(niveles_Codigo)){
  assign(paste0(niveles_Codigo[[i]], "_PNN"),dplyr::filter(Datos_CTDO_PNN, Codigo==niveles_Codigo[[i]]))
}

lista_codigos<- list()

for(i in seq_along(niveles_Codigo)){
  lista_codigos[[i]]<-print(paste0(niveles_Codigo[[i]], "_PNN"))
}

for (i in 1:36){
  print(paste0("Temp_",lista_codigos[[i]],"<-IctioExPacificoAnalisisPack::perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Temperatura,", lista_codigos[[i]],"$Profundidad,"," 'Temperatura - (°C)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Sal_",lista_codigos[[i]],"<-IctioExPacificoAnalisisPack::perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Salinidad,", lista_codigos[[i]],"$Profundidad,"," 'Salinidad - (PSU)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Oxi_",lista_codigos[[i]],"<-IctioExPacificoAnalisisPack::perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Oxigeno,", lista_codigos[[i]],"$Profundidad,"," 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Den_",lista_codigos[[i]],"<-IctioExPacificoAnalisisPack::perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Densidad,", lista_codigos[[i]],"$Profundidad,"," 'Densidad - (kg/m3)', 'Profundidad [m]')"))
}

Temp_A01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01A_PNN,A01A_PNN$Codigo,A01A_PNN$Marea,A01A_PNN$Temperatura,A01A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01B_PNN,A01B_PNN$Codigo,A01B_PNN$Marea,A01B_PNN$Temperatura,A01B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02A_PNN,A02A_PNN$Codigo,A02A_PNN$Marea,A02A_PNN$Temperatura,A02A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02B_PNN,A02B_PNN$Codigo,A02B_PNN$Marea,A02B_PNN$Temperatura,A02B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03A_PNN,A03A_PNN$Codigo,A03A_PNN$Marea,A03A_PNN$Temperatura,A03A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03B_PNN,A03B_PNN$Codigo,A03B_PNN$Marea,A03B_PNN$Temperatura,A03B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04A_PNN,A04A_PNN$Codigo,A04A_PNN$Marea,A04A_PNN$Temperatura,A04A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04B_PNN,A04B_PNN$Codigo,A04B_PNN$Marea,A04B_PNN$Temperatura,A04B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05A_PNN,A05A_PNN$Codigo,A05A_PNN$Marea,A05A_PNN$Temperatura,A05A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05B_PNN,A05B_PNN$Codigo,A05B_PNN$Marea,A05B_PNN$Temperatura,A05B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06A_PNN,A06A_PNN$Codigo,A06A_PNN$Marea,A06A_PNN$Temperatura,A06A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_A06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06B_PNN,A06B_PNN$Codigo,A06B_PNN$Marea,A06B_PNN$Temperatura,A06B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01A_PNN,G01A_PNN$Codigo,G01A_PNN$Marea,G01A_PNN$Temperatura,G01A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01B_PNN,G01B_PNN$Codigo,G01B_PNN$Marea,G01B_PNN$Temperatura,G01B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02A_PNN,G02A_PNN$Codigo,G02A_PNN$Marea,G02A_PNN$Temperatura,G02A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02B_PNN,G02B_PNN$Codigo,G02B_PNN$Marea,G02B_PNN$Temperatura,G02B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03A_PNN,G03A_PNN$Codigo,G03A_PNN$Marea,G03A_PNN$Temperatura,G03A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03B_PNN,G03B_PNN$Codigo,G03B_PNN$Marea,G03B_PNN$Temperatura,G03B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04A_PNN,G04A_PNN$Codigo,G04A_PNN$Marea,G04A_PNN$Temperatura,G04A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04B_PNN,G04B_PNN$Codigo,G04B_PNN$Marea,G04B_PNN$Temperatura,G04B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05A_PNN,G05A_PNN$Codigo,G05A_PNN$Marea,G05A_PNN$Temperatura,G05A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05B_PNN,G05B_PNN$Codigo,G05B_PNN$Marea,G05B_PNN$Temperatura,G05B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06A_PNN,G06A_PNN$Codigo,G06A_PNN$Marea,G06A_PNN$Temperatura,G06A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_G06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06B_PNN,G06B_PNN$Codigo,G06B_PNN$Marea,G06B_PNN$Temperatura,G06B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01A_PNN,S01A_PNN$Codigo,S01A_PNN$Marea,S01A_PNN$Temperatura,S01A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01B_PNN,S01B_PNN$Codigo,S01B_PNN$Marea,S01B_PNN$Temperatura,S01B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02A_PNN,S02A_PNN$Codigo,S02A_PNN$Marea,S02A_PNN$Temperatura,S02A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02B_PNN,S02B_PNN$Codigo,S02B_PNN$Marea,S02B_PNN$Temperatura,S02B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03A_PNN,S03A_PNN$Codigo,S03A_PNN$Marea,S03A_PNN$Temperatura,S03A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03B_PNN,S03B_PNN$Codigo,S03B_PNN$Marea,S03B_PNN$Temperatura,S03B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04A_PNN,S04A_PNN$Codigo,S04A_PNN$Marea,S04A_PNN$Temperatura,S04A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04B_PNN,S04B_PNN$Codigo,S04B_PNN$Marea,S04B_PNN$Temperatura,S04B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05A_PNN,S05A_PNN$Codigo,S05A_PNN$Marea,S05A_PNN$Temperatura,S05A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05B_PNN,S05B_PNN$Codigo,S05B_PNN$Marea,S05B_PNN$Temperatura,S05B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06A_PNN,S06A_PNN$Codigo,S06A_PNN$Marea,S06A_PNN$Temperatura,S06A_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
Temp_S06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06B_PNN,S06B_PNN$Codigo,S06B_PNN$Marea,S06B_PNN$Temperatura,S06B_PNN$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')

Sal_A01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01A_PNN,A01A_PNN$Codigo,A01A_PNN$Marea,A01A_PNN$Salinidad,A01A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01B_PNN,A01B_PNN$Codigo,A01B_PNN$Marea,A01B_PNN$Salinidad,A01B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02A_PNN,A02A_PNN$Codigo,A02A_PNN$Marea,A02A_PNN$Salinidad,A02A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02B_PNN,A02B_PNN$Codigo,A02B_PNN$Marea,A02B_PNN$Salinidad,A02B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03A_PNN,A03A_PNN$Codigo,A03A_PNN$Marea,A03A_PNN$Salinidad,A03A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03B_PNN,A03B_PNN$Codigo,A03B_PNN$Marea,A03B_PNN$Salinidad,A03B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04A_PNN,A04A_PNN$Codigo,A04A_PNN$Marea,A04A_PNN$Salinidad,A04A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04B_PNN,A04B_PNN$Codigo,A04B_PNN$Marea,A04B_PNN$Salinidad,A04B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05A_PNN,A05A_PNN$Codigo,A05A_PNN$Marea,A05A_PNN$Salinidad,A05A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05B_PNN,A05B_PNN$Codigo,A05B_PNN$Marea,A05B_PNN$Salinidad,A05B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06A_PNN,A06A_PNN$Codigo,A06A_PNN$Marea,A06A_PNN$Salinidad,A06A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_A06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06B_PNN,A06B_PNN$Codigo,A06B_PNN$Marea,A06B_PNN$Salinidad,A06B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01A_PNN,G01A_PNN$Codigo,G01A_PNN$Marea,G01A_PNN$Salinidad,G01A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01B_PNN,G01B_PNN$Codigo,G01B_PNN$Marea,G01B_PNN$Salinidad,G01B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02A_PNN,G02A_PNN$Codigo,G02A_PNN$Marea,G02A_PNN$Salinidad,G02A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02B_PNN,G02B_PNN$Codigo,G02B_PNN$Marea,G02B_PNN$Salinidad,G02B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03A_PNN,G03A_PNN$Codigo,G03A_PNN$Marea,G03A_PNN$Salinidad,G03A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03B_PNN,G03B_PNN$Codigo,G03B_PNN$Marea,G03B_PNN$Salinidad,G03B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04A_PNN,G04A_PNN$Codigo,G04A_PNN$Marea,G04A_PNN$Salinidad,G04A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04B_PNN,G04B_PNN$Codigo,G04B_PNN$Marea,G04B_PNN$Salinidad,G04B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05A_PNN,G05A_PNN$Codigo,G05A_PNN$Marea,G05A_PNN$Salinidad,G05A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05B_PNN,G05B_PNN$Codigo,G05B_PNN$Marea,G05B_PNN$Salinidad,G05B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06A_PNN,G06A_PNN$Codigo,G06A_PNN$Marea,G06A_PNN$Salinidad,G06A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_G06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06B_PNN,G06B_PNN$Codigo,G06B_PNN$Marea,G06B_PNN$Salinidad,G06B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01A_PNN,S01A_PNN$Codigo,S01A_PNN$Marea,S01A_PNN$Salinidad,S01A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01B_PNN,S01B_PNN$Codigo,S01B_PNN$Marea,S01B_PNN$Salinidad,S01B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02A_PNN,S02A_PNN$Codigo,S02A_PNN$Marea,S02A_PNN$Salinidad,S02A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02B_PNN,S02B_PNN$Codigo,S02B_PNN$Marea,S02B_PNN$Salinidad,S02B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03A_PNN,S03A_PNN$Codigo,S03A_PNN$Marea,S03A_PNN$Salinidad,S03A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03B_PNN,S03B_PNN$Codigo,S03B_PNN$Marea,S03B_PNN$Salinidad,S03B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04A_PNN,S04A_PNN$Codigo,S04A_PNN$Marea,S04A_PNN$Salinidad,S04A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04B_PNN,S04B_PNN$Codigo,S04B_PNN$Marea,S04B_PNN$Salinidad,S04B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05A_PNN,S05A_PNN$Codigo,S05A_PNN$Marea,S05A_PNN$Salinidad,S05A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05B_PNN,S05B_PNN$Codigo,S05B_PNN$Marea,S05B_PNN$Salinidad,S05B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06A_PNN,S06A_PNN$Codigo,S06A_PNN$Marea,S06A_PNN$Salinidad,S06A_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
Sal_S06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06B_PNN,S06B_PNN$Codigo,S06B_PNN$Marea,S06B_PNN$Salinidad,S06B_PNN$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')

Den_A01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01A_PNN,A01A_PNN$Codigo,A01A_PNN$Marea,A01A_PNN$Densidad,A01A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A01B_PNN,A01B_PNN$Codigo,A01B_PNN$Marea,A01B_PNN$Densidad,A01B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02A_PNN,A02A_PNN$Codigo,A02A_PNN$Marea,A02A_PNN$Densidad,A02A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A02B_PNN,A02B_PNN$Codigo,A02B_PNN$Marea,A02B_PNN$Densidad,A02B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03A_PNN,A03A_PNN$Codigo,A03A_PNN$Marea,A03A_PNN$Densidad,A03A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A03B_PNN,A03B_PNN$Codigo,A03B_PNN$Marea,A03B_PNN$Densidad,A03B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04A_PNN,A04A_PNN$Codigo,A04A_PNN$Marea,A04A_PNN$Densidad,A04A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A04B_PNN,A04B_PNN$Codigo,A04B_PNN$Marea,A04B_PNN$Densidad,A04B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05A_PNN,A05A_PNN$Codigo,A05A_PNN$Marea,A05A_PNN$Densidad,A05A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A05B_PNN,A05B_PNN$Codigo,A05B_PNN$Marea,A05B_PNN$Densidad,A05B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06A_PNN,A06A_PNN$Codigo,A06A_PNN$Marea,A06A_PNN$Densidad,A06A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_A06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(A06B_PNN,A06B_PNN$Codigo,A06B_PNN$Marea,A06B_PNN$Densidad,A06B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01A_PNN,G01A_PNN$Codigo,G01A_PNN$Marea,G01A_PNN$Densidad,G01A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G01B_PNN,G01B_PNN$Codigo,G01B_PNN$Marea,G01B_PNN$Densidad,G01B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02A_PNN,G02A_PNN$Codigo,G02A_PNN$Marea,G02A_PNN$Densidad,G02A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G02B_PNN,G02B_PNN$Codigo,G02B_PNN$Marea,G02B_PNN$Densidad,G02B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03A_PNN,G03A_PNN$Codigo,G03A_PNN$Marea,G03A_PNN$Densidad,G03A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G03B_PNN,G03B_PNN$Codigo,G03B_PNN$Marea,G03B_PNN$Densidad,G03B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04A_PNN,G04A_PNN$Codigo,G04A_PNN$Marea,G04A_PNN$Densidad,G04A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G04B_PNN,G04B_PNN$Codigo,G04B_PNN$Marea,G04B_PNN$Densidad,G04B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05A_PNN,G05A_PNN$Codigo,G05A_PNN$Marea,G05A_PNN$Densidad,G05A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G05B_PNN,G05B_PNN$Codigo,G05B_PNN$Marea,G05B_PNN$Densidad,G05B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06A_PNN,G06A_PNN$Codigo,G06A_PNN$Marea,G06A_PNN$Densidad,G06A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_G06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(G06B_PNN,G06B_PNN$Codigo,G06B_PNN$Marea,G06B_PNN$Densidad,G06B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S01A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01A_PNN,S01A_PNN$Codigo,S01A_PNN$Marea,S01A_PNN$Densidad,S01A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S01B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S01B_PNN,S01B_PNN$Codigo,S01B_PNN$Marea,S01B_PNN$Densidad,S01B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S02A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02A_PNN,S02A_PNN$Codigo,S02A_PNN$Marea,S02A_PNN$Densidad,S02A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S02B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S02B_PNN,S02B_PNN$Codigo,S02B_PNN$Marea,S02B_PNN$Densidad,S02B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S03A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03A_PNN,S03A_PNN$Codigo,S03A_PNN$Marea,S03A_PNN$Densidad,S03A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S03B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S03B_PNN,S03B_PNN$Codigo,S03B_PNN$Marea,S03B_PNN$Densidad,S03B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S04A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04A_PNN,S04A_PNN$Codigo,S04A_PNN$Marea,S04A_PNN$Densidad,S04A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S04B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S04B_PNN,S04B_PNN$Codigo,S04B_PNN$Marea,S04B_PNN$Densidad,S04B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S05A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05A_PNN,S05A_PNN$Codigo,S05A_PNN$Marea,S05A_PNN$Densidad,S05A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S05B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S05B_PNN,S05B_PNN$Codigo,S05B_PNN$Marea,S05B_PNN$Densidad,S05B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S06A_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06A_PNN,S06A_PNN$Codigo,S06A_PNN$Marea,S06A_PNN$Densidad,S06A_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
Den_S06B_PNN<-IctioExPacificoAnalisisPack::perfil_en_profundidad(S06B_PNN,S06B_PNN$Codigo,S06B_PNN$Marea,S06B_PNN$Densidad,S06B_PNN$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_A01A_PNN, Temp_A01B_PNN, Temp_A02A_PNN,Temp_A02B_PNN, Temp_A03A_PNN, Temp_A03B_PNN, Temp_A04A_PNN, Temp_A04B_PNN, Temp_A05A_PNN,Temp_A05B_PNN, Temp_A06A_PNN, Temp_A06B_PNN, top="Transecto Amarales")
dev.off()

png(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_PNN.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_A01A_PNN, Temp_A01B_PNN, Temp_A02A_PNN,Temp_A02B_PNN, Temp_A03A_PNN, Temp_A03B_PNN, Temp_A04A_PNN, Temp_A04B_PNN, Temp_A05A_PNN,Temp_A05B_PNN, Temp_A06A_PNN, Temp_A06B_PNN, top="Transecto Amarales")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_A01A_PNN, Sal_A01B_PNN, Sal_A02A_PNN,Sal_A02B_PNN, Sal_A03A_PNN, Sal_A03B_PNN, Sal_A04A_PNN, Sal_A04B_PNN, Sal_A05A_PNN,Sal_A05B_PNN, Sal_A06A_PNN, Sal_A06B_PNN, top="Transecto Amarales")
dev.off()


tiff(filename = "./03_Imagenes/Densidad_Transecto_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_A01A_PNN, Den_A01B_PNN, Den_A02A_PNN,Den_A02B_PNN, Den_A03A_PNN, Den_A03B_PNN, Den_A04A_PNN, Den_A04B_PNN, Den_A05A_PNN,Den_A05B_PNN, Den_A06A_PNN, Den_A06B_PNN, top="Transecto Amarales")
dev.off()


tiff(filename = "./03_Imagenes/Temperatura_Transecto_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_S01A_PNN, Temp_S01B_PNN, Temp_S02A_PNN,Temp_S02B_PNN, Temp_S03A_PNN, Temp_S03B_PNN, Temp_S04A_PNN, Temp_S04B_PNN, Temp_S05A_PNN,Temp_S05B_PNN, Temp_S06A_PNN, Temp_S06B_PNN, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_S01A_PNN, Sal_S01B_PNN, Sal_S02A_PNN,Sal_S02B_PNN, Sal_S03A_PNN, Sal_S03B_PNN, Sal_S04A_PNN, Sal_S04B_PNN, Sal_S05A_PNN,Sal_S05B_PNN, Sal_S06A_PNN, Sal_S06B_PNN, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Densidad_Transecto_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_S01A_PNN, Den_S01B_PNN, Den_S02A_PNN,Den_S02B_PNN, Den_S03A_PNN, Den_S03B_PNN, Den_S04A_PNN, Den_S04B_PNN, Den_S05A_PNN,Den_S05B_PNN, Den_S06A_PNN, Den_S06B_PNN, top="Transecto Sanquianga")
dev.off()


tiff(filename = "./03_Imagenes/Temperatura_Transecto_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_G01A_PNN, Temp_G01B_PNN, Temp_G02A_PNN,Temp_G02B_PNN, Temp_G03A_PNN, Temp_G03B_PNN, Temp_G04A_PNN, Temp_G04B_PNN, Temp_G05A_PNN,Temp_G05B_PNN, Temp_G06A_PNN, Temp_G06B_PNN, top="Transecto Guascama")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_G01A_PNN, Sal_G01B_PNN, Sal_G02A_PNN,Sal_G02B_PNN, Sal_G03A_PNN, Sal_G03B_PNN, Sal_G04A_PNN, Sal_G04B_PNN, Sal_G05A_PNN,Sal_G05B_PNN, Sal_G06A_PNN, Sal_G06B_PNN, top="Transecto Guascama")
dev.off()

tiff(filename = "./03_Imagenes/Densidad_Transecto_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_G01A_PNN, Den_G01B_PNN, Den_G02A_PNN,Den_G02B_PNN, Den_G03A_PNN, Den_G03B_PNN, Den_G04A_PNN, Den_G04B_PNN, Den_G05A_PNN,Den_G05B_PNN, Den_G06A_PNN, Den_G06B_PNN, top="Transecto Guascama")
dev.off()




#Temperatura
Temp01_PNN<-grid.arrange(nrow=6, ncol=2,Temp_A01A_PNN, Temp_A01B_PNN, Temp_A02A_PNN,Temp_A02B_PNN, Temp_A03A_PNN, Temp_A03B_PNN, Temp_A04A_PNN, Temp_A04B_PNN, Temp_A05A_PNN,Temp_A05B_PNN, Temp_A06A_PNN, Temp_A06B_PNN, top="Transecto Amarales")
Temp02_PNN<-grid.arrange(nrow=6, ncol=2,Temp_S01A_PNN, Temp_S01B_PNN, Temp_S02A_PNN,Temp_S02B_PNN, Temp_S03A_PNN, Temp_S03B_PNN, Temp_S04A_PNN, Temp_S04B_PNN, Temp_S05A_PNN,Temp_S05B_PNN, Temp_S06A_PNN, Temp_S06B_PNN, top="Transecto Sanquianga")
Temp03_PNN<-grid.arrange(nrow=6, ncol=2,Temp_G01A_PNN, Temp_G01B_PNN, Temp_G02A_PNN,Temp_G02B_PNN, Temp_G03A_PNN, Temp_G03B_PNN, Temp_G04A_PNN, Temp_G04B_PNN, Temp_G05A_PNN,Temp_G05B_PNN, Temp_G06A_PNN, Temp_G06B_PNN, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Temperatura_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Temp01_PNN, Temp02_PNN, Temp03_PNN)
dev.off() 

png(filename = "./03_Imagenes/Temperatura_Perfiles_PNN.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Temp01_PNN, Temp02_PNN, Temp03_PNN)
dev.off() 

Sal01_PNN<-grid.arrange(nrow=6, ncol=2,Sal_S01A_PNN, Sal_S01B_PNN, Sal_S02A_PNN,Sal_S02B_PNN, Sal_S03A_PNN, Sal_S03B_PNN, Sal_S04A_PNN, Sal_S04B_PNN, Sal_S05A_PNN,Sal_S05B_PNN, Sal_S06A_PNN, Sal_S06B_PNN, top="Transecto Sanquianga")
Sal02_PNN<-grid.arrange(nrow=6, ncol=2,Sal_A01A_PNN, Sal_A01B_PNN, Sal_A02A_PNN,Sal_A02B_PNN, Sal_A03A_PNN, Sal_A03B_PNN, Sal_A04A_PNN, Sal_A04B_PNN, Sal_A05A_PNN,Sal_A05B_PNN, Sal_A06A_PNN, Sal_A06B_PNN, top="Transecto Amarales")
Sal03_PNN<-grid.arrange(nrow=6, ncol=2,Sal_G01A_PNN, Sal_G01B_PNN, Sal_G02A_PNN,Sal_G02B_PNN, Sal_G03A_PNN, Sal_G03B_PNN, Sal_G04A_PNN, Sal_G04B_PNN, Sal_G05A_PNN,Sal_G05B_PNN, Sal_G06A_PNN, Sal_G06B_PNN, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Salinidad_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Sal01_PNN, Sal02_PNN, Sal03_PNN)
dev.off()

png(filename = "./03_Imagenes/Salinidad_Perfiles_PNN.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Sal01_PNN, Sal02_PNN, Sal03_PNN)
dev.off()

Den01_PNN<-grid.arrange(nrow=6, ncol=2,Den_S01A_PNN, Den_S01B_PNN, Den_S02A_PNN,Den_S02B_PNN, Den_S03A_PNN, Den_S03B_PNN, Den_S04A_PNN, Den_S04B_PNN, Den_S05A_PNN,Den_S05B_PNN, Den_S06A_PNN, Den_S06B_PNN, top="Transecto Sanquianga")
Den02_PNN<-grid.arrange(nrow=6, ncol=2,Den_A01A_PNN, Den_A01B_PNN, Den_A02A_PNN,Den_A02B_PNN, Den_A03A_PNN, Den_A03B_PNN, Den_A04A_PNN, Den_A04B_PNN, Den_A05A_PNN,Den_A05B_PNN, Den_A06A_PNN, Den_A06B_PNN, top="Transecto Amarales")
Den03_PNN<-grid.arrange(nrow=6, ncol=2,Den_G01A_PNN, Den_G01B_PNN, Den_G02A_PNN,Den_G02B_PNN, Den_G03A_PNN, Den_G03B_PNN, Den_G04A_PNN, Den_G04B_PNN, Den_G05A_PNN,Den_G05B_PNN, Den_G06A_PNN, Den_G06B_PNN, top="Transecto Guascama")

tiff(filename = "./03_Imagenes/Densidad_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,Den01_PNN, Den02_PNN, Den03_PNN)
dev.off()

png(filename = "./03_Imagenes/Densidad_Perfiles_PNN.png", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Den01_PNN, Den02_PNN, Den03_PNN)
dev.off()



