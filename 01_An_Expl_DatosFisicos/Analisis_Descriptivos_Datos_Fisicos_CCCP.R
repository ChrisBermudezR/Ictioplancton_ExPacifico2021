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
library(devtools)
library(dunn.test)


source("../Funciones/boxplot_CTDO.R")
source("../Funciones/KrusckalPotHoc.R")
source("../Funciones/histograma_Transecto.R")
source("../Funciones/perfil_en_profundidad.R")
source("../Funciones/boxplot_profundidad.R")

devtools::install_github("ChrisBermudezR/IctioExPacificoAnalisisPack")
library(IctioExPacificoAnalisisPack)


#####Manejo del conjunto de datos####
#Cargar el archivo de datos
Datos_CTDO_CCCP<-readr::read_csv("./02_DatosFisicos/Datos_CTDO_CCCP.csv")

#Asignar factores al evento de muestreo que en este caso está descrito por la variable "Codigo"
Datos_CTDO_CCCP$No.Estacion<-as.factor(Datos_CTDO_CCCP$No.Estacion)
Datos_CTDO_CCCP$Marea<-as.factor(Datos_CTDO_CCCP$Marea)
Datos_CTDO_CCCP$Codigo<-as.factor(Datos_CTDO_CCCP$Codigo)
Datos_CTDO_CCCP <- Datos_CTDO_CCCP %>% group_by(Codigo)

Datos_CTDO_CCCP<-Datos_CTDO_CCCP %>% mutate(Sector = case_when(
  No.Estacion == "6" ~ "Costero",
  No.Estacion == "5" ~ "Costero",
  No.Estacion == "4" ~ "Costero",
  No.Estacion == "3" ~ "Oceanico",
  No.Estacion == "2" ~ "Oceanico",
  No.Estacion == "1" ~ "Oceanico"
))
Datos_CTDO_CCCP$Sector<-as.factor(Datos_CTDO_CCCP$Sector)
####Estadistica descriptiva####
  
EstadisticasDescrip<-Datos_CTDO_CCCP %>% summarise_each(funs(mean(., na.rm = TRUE),   median(., na.rm = TRUE),n(),sd(., na.rm = TRUE), min(., na.rm = TRUE),max(., na.rm = TRUE)), Temperatura, Salinidad, Oxigeno, Densidad, Profundidad)

write_csv(EstadisticasDescrip, "./01_Resultados/Fisicos_EstadisticasDescrip_CCCP.csv", col_names = TRUE)


library(vegan)
Datos_CTDO_CCCPNA<-na.omit(Datos_CTDO_CCCP[,c(7,9,10,12,13,15,18)])
MRPP_CTDOMarea <- mrpp(dat = na.omit(Datos_CTDO_CCCPNA[,2:5]), grouping = Datos_CTDO_CCCPNA$Marea, permutations = 999)
MRPP_CTDOTransecto <- mrpp(dat = na.omit(Datos_CTDO_CCCPNA[,2:5]), grouping = Datos_CTDO_CCCPNA$Transecto, permutations = 999)
MRPP_CTDOSector <- mrpp(dat = na.omit(Datos_CTDO_CCCPNA[,2:5]), grouping = Datos_CTDO_CCCPNA$Sector, permutations = 999)

capture.output("MRPP Química - Mareas", 
               MRPP_CTDOMarea,
               "MRPP Química - Transecto", 
               MRPP_CTDOTransecto,
               "MRPP Química - Sectores",
               MRPP_CTDOSector,
               file="./01_Resultados/MRPP_CTDO.txt"
)
#########################


Temperatura_Transectos_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Temperatura, "Transectos", "Temperatura (°C)")
Salinidad_Transectos_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Salinidad, "Transectos", "Salinidad (PSU)")
Densidad_Transectos_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Densidad, "Transectos", "Densidad (KG/m3)")
Oxigeno_Transectos_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Oxigeno, "Transectos", "Oxígeno disuelto (mg/L)")


Temperatura_Marea_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Temperatura, "Marea", "Temperatura (°C)")
Salinidad_Marea_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Salinidad, "Marea", "Salinidad (PSU)")
Densidad_Marea_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Densidad, "Marea", "Densidad (KG/m3)")
Oxigeno_Marea_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Oxigeno, "Marea", "Oxígeno disuelto (mg/L)")

Temperatura_Sector_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Temperatura, "Sector", "Temperatura (°C)")
Salinidad_Sector_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Salinidad, "Sector", "Salinidad (PSU)")
Densidad_Sector_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Densidad, "Sector", "Densidad (KG/m3)")
Oxigeno_Sector_CCCP<-boxplot_CTDO(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Oxigeno, "Sector", "Oxígeno disuelto (mg/L)")


png(filename = "./03_Imagenes/01_CTDO_Transectos_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, res = 300)
grid.arrange(nrow=2, ncol=2, 
             Temperatura_Transectos_CCCP, 
             Salinidad_Transectos_CCCP,
             Densidad_Transectos_CCCP,
             Oxigeno_Transectos_CCCP)
dev.off()

png(filename = "./03_Imagenes/01_CTDO_Mareas_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, res = 300)
grid.arrange(nrow=2, ncol=2, 
             Temperatura_Marea_CCCP, 
             Salinidad_Marea_CCCP,
             Densidad_Marea_CCCP,
             Oxigeno_Marea_CCCP)
dev.off()

png(filename = "./03_Imagenes/01_CTDO_Sector_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, res = 300)
grid.arrange(nrow=2, ncol=2, 
             Temperatura_Sector_CCCP, 
             Salinidad_Sector_CCCP,
             Densidad_Sector_CCCP,
             Oxigeno_Sector_CCCP)
dev.off()

Temperatura_Transectos_KrusckalPotHoc<-KrusckalPotHoc(Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Temperatura, "./01_Resultados/Temperatura_Transectos_KrusckalPotHoc.txt", "Temperatura_Transectos_KrusckalPotHoc")
Salinidad_Transectos_KrusckalPotHoc<-KrusckalPotHoc(Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Salinidad, "./01_Resultados/Salinidad_Transectos_KrusckalPotHoc.txt", "Salinidad_Transectos_KrusckalPotHoc")
Densidad_Transectos_KrusckalPotHoc<-KrusckalPotHoc(Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Densidad, "./01_Resultados/Densidad_Transectos_KrusckalPotHoc.txt", "Densidad_Transectos_KrusckalPotHoc")
Oxigeno_Transectos_KrusckalPotHoc<-KrusckalPotHoc(Datos_CTDO_CCCP$Transecto, Datos_CTDO_CCCP$Oxigeno, "./01_Resultados/Oxigeno_Transectos_KrusckalPotHoc.txt", "Oxigeno_Transectos_KrusckalPotHoc")

source("../Funciones/Wilcoxon.R")
Temperatura_Marea_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Temperatura, "./01_Resultados/Temperatura_Marea_Wilcoxon.txt", "Temperatura_Marea_Wilcoxon")
Salinidad_Marea_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Salinidad, "./01_Resultados/Salinidad_Marea_Wilcoxon.txt", "Salinidad_Marea_Wilcoxon")
Densidad_Marea_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Densidad, "./01_Resultados/Densidad_Marea_Wilcoxon.txt", "Densidad_Marea_Wilcoxon")
Oxigeno_Marea_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Oxigeno, "./01_Resultados/Oxigeno_Marea_Wilcoxon.txt", "Oxigeno_Marea_Wilcoxon")

Temperatura_Sector_Wilcoxon<-Wilcoxon( Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Temperatura, "./01_Resultados/Temperatura_Sector_Wilcoxon.txt", "Temperatura_Sector_Wilcoxon")
Salinidad_Sector_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Salinidad, "./01_Resultados/Salinidad_Sector_Wilcoxon.txt", "Salinidad_Sector_Wilcoxon")
Densidad_Sector_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Densidad, "./01_Resultados/Densidad_Sector_Wilcoxon.txt", "Densidad_Sector_Wilcoxon")
Oxigeno_Sector_Wilcoxon<-Wilcoxon(Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Oxigeno, "./01_Resultados/Oxigeno_Sector_Wilcoxon.txt", "Oxigeno_Sector_Wilcoxon")










Temperatura_Hist_CCCP<-histograma_Transecto(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Transecto,Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Temperatura, "Temperatura (°C)")
Salinidad_Hist_CCCP<-histograma_Transecto(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Transecto,Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Salinidad, "Salinidad (PSU)")
Densidad_Hist_CCCP<-histograma_Transecto(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Transecto,Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Densidad, "Densidad (KG/m3)")
Oxigeno_Hist_CCCP<-histograma_Transecto(Datos_CTDO_CCCP, Datos_CTDO_CCCP$Marea, Datos_CTDO_CCCP$Transecto,Datos_CTDO_CCCP$Sector, Datos_CTDO_CCCP$Oxigeno, "Oxígeno disuelto (mg/L)")



png(filename = "./03_Imagenes/02_Histogramas_CCCP.png", width = 40, height = 45, units = "cm", pointsize = 30, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_CCCP, Salinidad_Hist_CCCP, Densidad_Hist_CCCP, Oxigeno_Hist_CCCP)
dev.off()



Temperatura_boxplot_CCCP<-boxplot_profundidad(Datos_CTDO_CCCP,Datos_CTDO_CCCP$No.Estacion,Datos_CTDO_CCCP$Temperatura,"Temperatura (°C)")
Salinidad_boxplot_CCCP<-boxplot_profundidad(Datos_CTDO_CCCP,Datos_CTDO_CCCP$No.Estacion,Datos_CTDO_CCCP$Salinidad,"Salinidad (PSU)")
Densidad_boxplot_CCCP<-boxplot_profundidad(Datos_CTDO_CCCP,Datos_CTDO_CCCP$No.Estacion,Datos_CTDO_CCCP$Densidad,"Densidad (KG/m3)")
Oxigeno_boxplot_CCCP<-boxplot_profundidad(Datos_CTDO_CCCP,Datos_CTDO_CCCP$No.Estacion,Datos_CTDO_CCCP$Oxigeno,"Oxígeno disuelto (mg/L)")




png(filename = "./03_Imagenes/03_Boxplot_CCCP.png", width = 30, height = 35, units = "cm", pointsize = 12, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_CCCP, Salinidad_boxplot_CCCP, 
             Densidad_boxplot_CCCP, Oxigeno_boxplot_CCCP)
dev.off()



#Crear una lista con los niveles del código de evento de muestreo
niveles_Codigo<-as.list(levels(Datos_CTDO_CCCP$Codigo))

#Ciclo para asignar objetos a un filtro de datos por cada evento de muestreo
for(i in seq_along(niveles_Codigo)){
  assign(paste0(niveles_Codigo[[i]], "_CCCP"),dplyr::filter(Datos_CTDO_CCCP, Codigo==niveles_Codigo[[i]]))
}

lista_codigos<- list()

for(i in seq_along(niveles_Codigo)){
  lista_codigos[[i]]<-print(paste0(niveles_Codigo[[i]], "_CCCP"))
}

for (i in 1:36){
  print(paste0("Temp_",lista_codigos[[i]],"<-perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Temperatura,", lista_codigos[[i]],"$Profundidad,"," 'Temperatura - (°C)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Sal_",lista_codigos[[i]],"<-perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Salinidad,", lista_codigos[[i]],"$Profundidad,"," 'Salinidad - (PSU)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Oxi_",lista_codigos[[i]],"<-perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Oxigeno,", lista_codigos[[i]],"$Profundidad,"," 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')"))
}

for (i in 1:36){
  print(paste0("Den_",lista_codigos[[i]],"<-perfil_en_profundidad(",lista_codigos[[i]],",", lista_codigos[[i]],"$Codigo,", lista_codigos[[i]],"$Marea,", lista_codigos[[i]],"$Densidad,", lista_codigos[[i]],"$Profundidad,"," 'Densidad - (kg/m3)', 'Profundidad [m]')"))
}

 Temp_A01A_CCCP<-perfil_en_profundidad(A01A_CCCP,A01A_CCCP$Codigo,A01A_CCCP$Marea,A01A_CCCP$Temperatura,A01A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A01B_CCCP<-perfil_en_profundidad(A01B_CCCP,A01B_CCCP$Codigo,A01B_CCCP$Marea,A01B_CCCP$Temperatura,A01B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A02A_CCCP<-perfil_en_profundidad(A02A_CCCP,A02A_CCCP$Codigo,A02A_CCCP$Marea,A02A_CCCP$Temperatura,A02A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A02B_CCCP<-perfil_en_profundidad(A02B_CCCP,A02B_CCCP$Codigo,A02B_CCCP$Marea,A02B_CCCP$Temperatura,A02B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A03A_CCCP<-perfil_en_profundidad(A03A_CCCP,A03A_CCCP$Codigo,A03A_CCCP$Marea,A03A_CCCP$Temperatura,A03A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A03B_CCCP<-perfil_en_profundidad(A03B_CCCP,A03B_CCCP$Codigo,A03B_CCCP$Marea,A03B_CCCP$Temperatura,A03B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A04A_CCCP<-perfil_en_profundidad(A04A_CCCP,A04A_CCCP$Codigo,A04A_CCCP$Marea,A04A_CCCP$Temperatura,A04A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A04B_CCCP<-perfil_en_profundidad(A04B_CCCP,A04B_CCCP$Codigo,A04B_CCCP$Marea,A04B_CCCP$Temperatura,A04B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A05A_CCCP<-perfil_en_profundidad(A05A_CCCP,A05A_CCCP$Codigo,A05A_CCCP$Marea,A05A_CCCP$Temperatura,A05A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A05B_CCCP<-perfil_en_profundidad(A05B_CCCP,A05B_CCCP$Codigo,A05B_CCCP$Marea,A05B_CCCP$Temperatura,A05B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A06A_CCCP<-perfil_en_profundidad(A06A_CCCP,A06A_CCCP$Codigo,A06A_CCCP$Marea,A06A_CCCP$Temperatura,A06A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_A06B_CCCP<-perfil_en_profundidad(A06B_CCCP,A06B_CCCP$Codigo,A06B_CCCP$Marea,A06B_CCCP$Temperatura,A06B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G01A_CCCP<-perfil_en_profundidad(G01A_CCCP,G01A_CCCP$Codigo,G01A_CCCP$Marea,G01A_CCCP$Temperatura,G01A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G01B_CCCP<-perfil_en_profundidad(G01B_CCCP,G01B_CCCP$Codigo,G01B_CCCP$Marea,G01B_CCCP$Temperatura,G01B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G02A_CCCP<-perfil_en_profundidad(G02A_CCCP,G02A_CCCP$Codigo,G02A_CCCP$Marea,G02A_CCCP$Temperatura,G02A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G02B_CCCP<-perfil_en_profundidad(G02B_CCCP,G02B_CCCP$Codigo,G02B_CCCP$Marea,G02B_CCCP$Temperatura,G02B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G03A_CCCP<-perfil_en_profundidad(G03A_CCCP,G03A_CCCP$Codigo,G03A_CCCP$Marea,G03A_CCCP$Temperatura,G03A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G03B_CCCP<-perfil_en_profundidad(G03B_CCCP,G03B_CCCP$Codigo,G03B_CCCP$Marea,G03B_CCCP$Temperatura,G03B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G04A_CCCP<-perfil_en_profundidad(G04A_CCCP,G04A_CCCP$Codigo,G04A_CCCP$Marea,G04A_CCCP$Temperatura,G04A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G04B_CCCP<-perfil_en_profundidad(G04B_CCCP,G04B_CCCP$Codigo,G04B_CCCP$Marea,G04B_CCCP$Temperatura,G04B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G05A_CCCP<-perfil_en_profundidad(G05A_CCCP,G05A_CCCP$Codigo,G05A_CCCP$Marea,G05A_CCCP$Temperatura,G05A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G05B_CCCP<-perfil_en_profundidad(G05B_CCCP,G05B_CCCP$Codigo,G05B_CCCP$Marea,G05B_CCCP$Temperatura,G05B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G06A_CCCP<-perfil_en_profundidad(G06A_CCCP,G06A_CCCP$Codigo,G06A_CCCP$Marea,G06A_CCCP$Temperatura,G06A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_G06B_CCCP<-perfil_en_profundidad(G06B_CCCP,G06B_CCCP$Codigo,G06B_CCCP$Marea,G06B_CCCP$Temperatura,G06B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S01A_CCCP<-perfil_en_profundidad(S01A_CCCP,S01A_CCCP$Codigo,S01A_CCCP$Marea,S01A_CCCP$Temperatura,S01A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S01B_CCCP<-perfil_en_profundidad(S01B_CCCP,S01B_CCCP$Codigo,S01B_CCCP$Marea,S01B_CCCP$Temperatura,S01B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S02A_CCCP<-perfil_en_profundidad(S02A_CCCP,S02A_CCCP$Codigo,S02A_CCCP$Marea,S02A_CCCP$Temperatura,S02A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S02B_CCCP<-perfil_en_profundidad(S02B_CCCP,S02B_CCCP$Codigo,S02B_CCCP$Marea,S02B_CCCP$Temperatura,S02B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S03A_CCCP<-perfil_en_profundidad(S03A_CCCP,S03A_CCCP$Codigo,S03A_CCCP$Marea,S03A_CCCP$Temperatura,S03A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S03B_CCCP<-perfil_en_profundidad(S03B_CCCP,S03B_CCCP$Codigo,S03B_CCCP$Marea,S03B_CCCP$Temperatura,S03B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S04A_CCCP<-perfil_en_profundidad(S04A_CCCP,S04A_CCCP$Codigo,S04A_CCCP$Marea,S04A_CCCP$Temperatura,S04A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S04B_CCCP<-perfil_en_profundidad(S04B_CCCP,S04B_CCCP$Codigo,S04B_CCCP$Marea,S04B_CCCP$Temperatura,S04B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S05A_CCCP<-perfil_en_profundidad(S05A_CCCP,S05A_CCCP$Codigo,S05A_CCCP$Marea,S05A_CCCP$Temperatura,S05A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S05B_CCCP<-perfil_en_profundidad(S05B_CCCP,S05B_CCCP$Codigo,S05B_CCCP$Marea,S05B_CCCP$Temperatura,S05B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S06A_CCCP<-perfil_en_profundidad(S06A_CCCP,S06A_CCCP$Codigo,S06A_CCCP$Marea,S06A_CCCP$Temperatura,S06A_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')
 Temp_S06B_CCCP<-perfil_en_profundidad(S06B_CCCP,S06B_CCCP$Codigo,S06B_CCCP$Marea,S06B_CCCP$Temperatura,S06B_CCCP$Profundidad, 'Temperatura - [°C]', 'Profundidad [m]')

 Sal_A01A_CCCP<-perfil_en_profundidad(A01A_CCCP,A01A_CCCP$Codigo,A01A_CCCP$Marea,A01A_CCCP$Salinidad,A01A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A01B_CCCP<-perfil_en_profundidad(A01B_CCCP,A01B_CCCP$Codigo,A01B_CCCP$Marea,A01B_CCCP$Salinidad,A01B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A02A_CCCP<-perfil_en_profundidad(A02A_CCCP,A02A_CCCP$Codigo,A02A_CCCP$Marea,A02A_CCCP$Salinidad,A02A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A02B_CCCP<-perfil_en_profundidad(A02B_CCCP,A02B_CCCP$Codigo,A02B_CCCP$Marea,A02B_CCCP$Salinidad,A02B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A03A_CCCP<-perfil_en_profundidad(A03A_CCCP,A03A_CCCP$Codigo,A03A_CCCP$Marea,A03A_CCCP$Salinidad,A03A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A03B_CCCP<-perfil_en_profundidad(A03B_CCCP,A03B_CCCP$Codigo,A03B_CCCP$Marea,A03B_CCCP$Salinidad,A03B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A04A_CCCP<-perfil_en_profundidad(A04A_CCCP,A04A_CCCP$Codigo,A04A_CCCP$Marea,A04A_CCCP$Salinidad,A04A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A04B_CCCP<-perfil_en_profundidad(A04B_CCCP,A04B_CCCP$Codigo,A04B_CCCP$Marea,A04B_CCCP$Salinidad,A04B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A05A_CCCP<-perfil_en_profundidad(A05A_CCCP,A05A_CCCP$Codigo,A05A_CCCP$Marea,A05A_CCCP$Salinidad,A05A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A05B_CCCP<-perfil_en_profundidad(A05B_CCCP,A05B_CCCP$Codigo,A05B_CCCP$Marea,A05B_CCCP$Salinidad,A05B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A06A_CCCP<-perfil_en_profundidad(A06A_CCCP,A06A_CCCP$Codigo,A06A_CCCP$Marea,A06A_CCCP$Salinidad,A06A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_A06B_CCCP<-perfil_en_profundidad(A06B_CCCP,A06B_CCCP$Codigo,A06B_CCCP$Marea,A06B_CCCP$Salinidad,A06B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G01A_CCCP<-perfil_en_profundidad(G01A_CCCP,G01A_CCCP$Codigo,G01A_CCCP$Marea,G01A_CCCP$Salinidad,G01A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G01B_CCCP<-perfil_en_profundidad(G01B_CCCP,G01B_CCCP$Codigo,G01B_CCCP$Marea,G01B_CCCP$Salinidad,G01B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G02A_CCCP<-perfil_en_profundidad(G02A_CCCP,G02A_CCCP$Codigo,G02A_CCCP$Marea,G02A_CCCP$Salinidad,G02A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G02B_CCCP<-perfil_en_profundidad(G02B_CCCP,G02B_CCCP$Codigo,G02B_CCCP$Marea,G02B_CCCP$Salinidad,G02B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G03A_CCCP<-perfil_en_profundidad(G03A_CCCP,G03A_CCCP$Codigo,G03A_CCCP$Marea,G03A_CCCP$Salinidad,G03A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G03B_CCCP<-perfil_en_profundidad(G03B_CCCP,G03B_CCCP$Codigo,G03B_CCCP$Marea,G03B_CCCP$Salinidad,G03B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G04A_CCCP<-perfil_en_profundidad(G04A_CCCP,G04A_CCCP$Codigo,G04A_CCCP$Marea,G04A_CCCP$Salinidad,G04A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G04B_CCCP<-perfil_en_profundidad(G04B_CCCP,G04B_CCCP$Codigo,G04B_CCCP$Marea,G04B_CCCP$Salinidad,G04B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G05A_CCCP<-perfil_en_profundidad(G05A_CCCP,G05A_CCCP$Codigo,G05A_CCCP$Marea,G05A_CCCP$Salinidad,G05A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G05B_CCCP<-perfil_en_profundidad(G05B_CCCP,G05B_CCCP$Codigo,G05B_CCCP$Marea,G05B_CCCP$Salinidad,G05B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G06A_CCCP<-perfil_en_profundidad(G06A_CCCP,G06A_CCCP$Codigo,G06A_CCCP$Marea,G06A_CCCP$Salinidad,G06A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_G06B_CCCP<-perfil_en_profundidad(G06B_CCCP,G06B_CCCP$Codigo,G06B_CCCP$Marea,G06B_CCCP$Salinidad,G06B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S01A_CCCP<-perfil_en_profundidad(S01A_CCCP,S01A_CCCP$Codigo,S01A_CCCP$Marea,S01A_CCCP$Salinidad,S01A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S01B_CCCP<-perfil_en_profundidad(S01B_CCCP,S01B_CCCP$Codigo,S01B_CCCP$Marea,S01B_CCCP$Salinidad,S01B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S02A_CCCP<-perfil_en_profundidad(S02A_CCCP,S02A_CCCP$Codigo,S02A_CCCP$Marea,S02A_CCCP$Salinidad,S02A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S02B_CCCP<-perfil_en_profundidad(S02B_CCCP,S02B_CCCP$Codigo,S02B_CCCP$Marea,S02B_CCCP$Salinidad,S02B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S03A_CCCP<-perfil_en_profundidad(S03A_CCCP,S03A_CCCP$Codigo,S03A_CCCP$Marea,S03A_CCCP$Salinidad,S03A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S03B_CCCP<-perfil_en_profundidad(S03B_CCCP,S03B_CCCP$Codigo,S03B_CCCP$Marea,S03B_CCCP$Salinidad,S03B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S04A_CCCP<-perfil_en_profundidad(S04A_CCCP,S04A_CCCP$Codigo,S04A_CCCP$Marea,S04A_CCCP$Salinidad,S04A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S04B_CCCP<-perfil_en_profundidad(S04B_CCCP,S04B_CCCP$Codigo,S04B_CCCP$Marea,S04B_CCCP$Salinidad,S04B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S05A_CCCP<-perfil_en_profundidad(S05A_CCCP,S05A_CCCP$Codigo,S05A_CCCP$Marea,S05A_CCCP$Salinidad,S05A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S05B_CCCP<-perfil_en_profundidad(S05B_CCCP,S05B_CCCP$Codigo,S05B_CCCP$Marea,S05B_CCCP$Salinidad,S05B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S06A_CCCP<-perfil_en_profundidad(S06A_CCCP,S06A_CCCP$Codigo,S06A_CCCP$Marea,S06A_CCCP$Salinidad,S06A_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')
 Sal_S06B_CCCP<-perfil_en_profundidad(S06B_CCCP,S06B_CCCP$Codigo,S06B_CCCP$Marea,S06B_CCCP$Salinidad,S06B_CCCP$Profundidad, 'Salinidad - (PSU)', 'Profundidad [m]')

 Oxi_A01A_CCCP<-perfil_en_profundidad(A01A_CCCP,A01A_CCCP$Codigo,A01A_CCCP$Marea,A01A_CCCP$Oxigeno,A01A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A01B_CCCP<-perfil_en_profundidad(A01B_CCCP,A01B_CCCP$Codigo,A01B_CCCP$Marea,A01B_CCCP$Oxigeno,A01B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A02A_CCCP<-perfil_en_profundidad(A02A_CCCP,A02A_CCCP$Codigo,A02A_CCCP$Marea,A02A_CCCP$Oxigeno,A02A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A02B_CCCP<-perfil_en_profundidad(A02B_CCCP,A02B_CCCP$Codigo,A02B_CCCP$Marea,A02B_CCCP$Oxigeno,A02B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A03A_CCCP<-perfil_en_profundidad(A03A_CCCP,A03A_CCCP$Codigo,A03A_CCCP$Marea,A03A_CCCP$Oxigeno,A03A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A03B_CCCP<-perfil_en_profundidad(A03B_CCCP,A03B_CCCP$Codigo,A03B_CCCP$Marea,A03B_CCCP$Oxigeno,A03B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A04A_CCCP<-perfil_en_profundidad(A04A_CCCP,A04A_CCCP$Codigo,A04A_CCCP$Marea,A04A_CCCP$Oxigeno,A04A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A04B_CCCP<-perfil_en_profundidad(A04B_CCCP,A04B_CCCP$Codigo,A04B_CCCP$Marea,A04B_CCCP$Oxigeno,A04B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A05A_CCCP<-perfil_en_profundidad(A05A_CCCP,A05A_CCCP$Codigo,A05A_CCCP$Marea,A05A_CCCP$Oxigeno,A05A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A05B_CCCP<-perfil_en_profundidad(A05B_CCCP,A05B_CCCP$Codigo,A05B_CCCP$Marea,A05B_CCCP$Oxigeno,A05B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A06A_CCCP<-perfil_en_profundidad(A06A_CCCP,A06A_CCCP$Codigo,A06A_CCCP$Marea,A06A_CCCP$Oxigeno,A06A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_A06B_CCCP<-perfil_en_profundidad(A06B_CCCP,A06B_CCCP$Codigo,A06B_CCCP$Marea,A06B_CCCP$Oxigeno,A06B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G01A_CCCP<-perfil_en_profundidad(G01A_CCCP,G01A_CCCP$Codigo,G01A_CCCP$Marea,G01A_CCCP$Oxigeno,G01A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G01B_CCCP<-perfil_en_profundidad(G01B_CCCP,G01B_CCCP$Codigo,G01B_CCCP$Marea,G01B_CCCP$Oxigeno,G01B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G02A_CCCP<-perfil_en_profundidad(G02A_CCCP,G02A_CCCP$Codigo,G02A_CCCP$Marea,G02A_CCCP$Oxigeno,G02A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G02B_CCCP<-perfil_en_profundidad(G02B_CCCP,G02B_CCCP$Codigo,G02B_CCCP$Marea,G02B_CCCP$Oxigeno,G02B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G03A_CCCP<-perfil_en_profundidad(G03A_CCCP,G03A_CCCP$Codigo,G03A_CCCP$Marea,G03A_CCCP$Oxigeno,G03A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G03B_CCCP<-perfil_en_profundidad(G03B_CCCP,G03B_CCCP$Codigo,G03B_CCCP$Marea,G03B_CCCP$Oxigeno,G03B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G04A_CCCP<-perfil_en_profundidad(G04A_CCCP,G04A_CCCP$Codigo,G04A_CCCP$Marea,G04A_CCCP$Oxigeno,G04A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G04B_CCCP<-perfil_en_profundidad(G04B_CCCP,G04B_CCCP$Codigo,G04B_CCCP$Marea,G04B_CCCP$Oxigeno,G04B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G05A_CCCP<-perfil_en_profundidad(G05A_CCCP,G05A_CCCP$Codigo,G05A_CCCP$Marea,G05A_CCCP$Oxigeno,G05A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G05B_CCCP<-perfil_en_profundidad(G05B_CCCP,G05B_CCCP$Codigo,G05B_CCCP$Marea,G05B_CCCP$Oxigeno,G05B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G06A_CCCP<-perfil_en_profundidad(G06A_CCCP,G06A_CCCP$Codigo,G06A_CCCP$Marea,G06A_CCCP$Oxigeno,G06A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_G06B_CCCP<-perfil_en_profundidad(G06B_CCCP,G06B_CCCP$Codigo,G06B_CCCP$Marea,G06B_CCCP$Oxigeno,G06B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S01A_CCCP<-perfil_en_profundidad(S01A_CCCP,S01A_CCCP$Codigo,S01A_CCCP$Marea,S01A_CCCP$Oxigeno,S01A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S01B_CCCP<-perfil_en_profundidad(S01B_CCCP,S01B_CCCP$Codigo,S01B_CCCP$Marea,S01B_CCCP$Oxigeno,S01B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S02A_CCCP<-perfil_en_profundidad(S02A_CCCP,S02A_CCCP$Codigo,S02A_CCCP$Marea,S02A_CCCP$Oxigeno,S02A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S02B_CCCP<-perfil_en_profundidad(S02B_CCCP,S02B_CCCP$Codigo,S02B_CCCP$Marea,S02B_CCCP$Oxigeno,S02B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S03A_CCCP<-perfil_en_profundidad(S03A_CCCP,S03A_CCCP$Codigo,S03A_CCCP$Marea,S03A_CCCP$Oxigeno,S03A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S03B_CCCP<-perfil_en_profundidad(S03B_CCCP,S03B_CCCP$Codigo,S03B_CCCP$Marea,S03B_CCCP$Oxigeno,S03B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S04A_CCCP<-perfil_en_profundidad(S04A_CCCP,S04A_CCCP$Codigo,S04A_CCCP$Marea,S04A_CCCP$Oxigeno,S04A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S04B_CCCP<-perfil_en_profundidad(S04B_CCCP,S04B_CCCP$Codigo,S04B_CCCP$Marea,S04B_CCCP$Oxigeno,S04B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S05A_CCCP<-perfil_en_profundidad(S05A_CCCP,S05A_CCCP$Codigo,S05A_CCCP$Marea,S05A_CCCP$Oxigeno,S05A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S05B_CCCP<-perfil_en_profundidad(S05B_CCCP,S05B_CCCP$Codigo,S05B_CCCP$Marea,S05B_CCCP$Oxigeno,S05B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S06A_CCCP<-perfil_en_profundidad(S06A_CCCP,S06A_CCCP$Codigo,S06A_CCCP$Marea,S06A_CCCP$Oxigeno,S06A_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')
 Oxi_S06B_CCCP<-perfil_en_profundidad(S06B_CCCP,S06B_CCCP$Codigo,S06B_CCCP$Marea,S06B_CCCP$Oxigeno,S06B_CCCP$Profundidad, 'Oxígeno disuelto - (mg/L)', 'Profundidad [m]')

 Den_A01A_CCCP<-perfil_en_profundidad(A01A_CCCP,A01A_CCCP$Codigo,A01A_CCCP$Marea,A01A_CCCP$Densidad,A01A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A01B_CCCP<-perfil_en_profundidad(A01B_CCCP,A01B_CCCP$Codigo,A01B_CCCP$Marea,A01B_CCCP$Densidad,A01B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A02A_CCCP<-perfil_en_profundidad(A02A_CCCP,A02A_CCCP$Codigo,A02A_CCCP$Marea,A02A_CCCP$Densidad,A02A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A02B_CCCP<-perfil_en_profundidad(A02B_CCCP,A02B_CCCP$Codigo,A02B_CCCP$Marea,A02B_CCCP$Densidad,A02B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A03A_CCCP<-perfil_en_profundidad(A03A_CCCP,A03A_CCCP$Codigo,A03A_CCCP$Marea,A03A_CCCP$Densidad,A03A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A03B_CCCP<-perfil_en_profundidad(A03B_CCCP,A03B_CCCP$Codigo,A03B_CCCP$Marea,A03B_CCCP$Densidad,A03B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A04A_CCCP<-perfil_en_profundidad(A04A_CCCP,A04A_CCCP$Codigo,A04A_CCCP$Marea,A04A_CCCP$Densidad,A04A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A04B_CCCP<-perfil_en_profundidad(A04B_CCCP,A04B_CCCP$Codigo,A04B_CCCP$Marea,A04B_CCCP$Densidad,A04B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A05A_CCCP<-perfil_en_profundidad(A05A_CCCP,A05A_CCCP$Codigo,A05A_CCCP$Marea,A05A_CCCP$Densidad,A05A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A05B_CCCP<-perfil_en_profundidad(A05B_CCCP,A05B_CCCP$Codigo,A05B_CCCP$Marea,A05B_CCCP$Densidad,A05B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A06A_CCCP<-perfil_en_profundidad(A06A_CCCP,A06A_CCCP$Codigo,A06A_CCCP$Marea,A06A_CCCP$Densidad,A06A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_A06B_CCCP<-perfil_en_profundidad(A06B_CCCP,A06B_CCCP$Codigo,A06B_CCCP$Marea,A06B_CCCP$Densidad,A06B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G01A_CCCP<-perfil_en_profundidad(G01A_CCCP,G01A_CCCP$Codigo,G01A_CCCP$Marea,G01A_CCCP$Densidad,G01A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G01B_CCCP<-perfil_en_profundidad(G01B_CCCP,G01B_CCCP$Codigo,G01B_CCCP$Marea,G01B_CCCP$Densidad,G01B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G02A_CCCP<-perfil_en_profundidad(G02A_CCCP,G02A_CCCP$Codigo,G02A_CCCP$Marea,G02A_CCCP$Densidad,G02A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G02B_CCCP<-perfil_en_profundidad(G02B_CCCP,G02B_CCCP$Codigo,G02B_CCCP$Marea,G02B_CCCP$Densidad,G02B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G03A_CCCP<-perfil_en_profundidad(G03A_CCCP,G03A_CCCP$Codigo,G03A_CCCP$Marea,G03A_CCCP$Densidad,G03A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G03B_CCCP<-perfil_en_profundidad(G03B_CCCP,G03B_CCCP$Codigo,G03B_CCCP$Marea,G03B_CCCP$Densidad,G03B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G04A_CCCP<-perfil_en_profundidad(G04A_CCCP,G04A_CCCP$Codigo,G04A_CCCP$Marea,G04A_CCCP$Densidad,G04A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G04B_CCCP<-perfil_en_profundidad(G04B_CCCP,G04B_CCCP$Codigo,G04B_CCCP$Marea,G04B_CCCP$Densidad,G04B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G05A_CCCP<-perfil_en_profundidad(G05A_CCCP,G05A_CCCP$Codigo,G05A_CCCP$Marea,G05A_CCCP$Densidad,G05A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G05B_CCCP<-perfil_en_profundidad(G05B_CCCP,G05B_CCCP$Codigo,G05B_CCCP$Marea,G05B_CCCP$Densidad,G05B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G06A_CCCP<-perfil_en_profundidad(G06A_CCCP,G06A_CCCP$Codigo,G06A_CCCP$Marea,G06A_CCCP$Densidad,G06A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_G06B_CCCP<-perfil_en_profundidad(G06B_CCCP,G06B_CCCP$Codigo,G06B_CCCP$Marea,G06B_CCCP$Densidad,G06B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S01A_CCCP<-perfil_en_profundidad(S01A_CCCP,S01A_CCCP$Codigo,S01A_CCCP$Marea,S01A_CCCP$Densidad,S01A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S01B_CCCP<-perfil_en_profundidad(S01B_CCCP,S01B_CCCP$Codigo,S01B_CCCP$Marea,S01B_CCCP$Densidad,S01B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S02A_CCCP<-perfil_en_profundidad(S02A_CCCP,S02A_CCCP$Codigo,S02A_CCCP$Marea,S02A_CCCP$Densidad,S02A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S02B_CCCP<-perfil_en_profundidad(S02B_CCCP,S02B_CCCP$Codigo,S02B_CCCP$Marea,S02B_CCCP$Densidad,S02B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S03A_CCCP<-perfil_en_profundidad(S03A_CCCP,S03A_CCCP$Codigo,S03A_CCCP$Marea,S03A_CCCP$Densidad,S03A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S03B_CCCP<-perfil_en_profundidad(S03B_CCCP,S03B_CCCP$Codigo,S03B_CCCP$Marea,S03B_CCCP$Densidad,S03B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S04A_CCCP<-perfil_en_profundidad(S04A_CCCP,S04A_CCCP$Codigo,S04A_CCCP$Marea,S04A_CCCP$Densidad,S04A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S04B_CCCP<-perfil_en_profundidad(S04B_CCCP,S04B_CCCP$Codigo,S04B_CCCP$Marea,S04B_CCCP$Densidad,S04B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S05A_CCCP<-perfil_en_profundidad(S05A_CCCP,S05A_CCCP$Codigo,S05A_CCCP$Marea,S05A_CCCP$Densidad,S05A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S05B_CCCP<-perfil_en_profundidad(S05B_CCCP,S05B_CCCP$Codigo,S05B_CCCP$Marea,S05B_CCCP$Densidad,S05B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S06A_CCCP<-perfil_en_profundidad(S06A_CCCP,S06A_CCCP$Codigo,S06A_CCCP$Marea,S06A_CCCP$Densidad,S06A_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')
 Den_S06B_CCCP<-perfil_en_profundidad(S06B_CCCP,S06B_CCCP$Codigo,S06B_CCCP$Marea,S06B_CCCP$Densidad,S06B_CCCP$Profundidad, 'Densidad - (kg/m3)', 'Profundidad [m]')

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_A01A_CCCP, Temp_A01B_CCCP, Temp_A02A_CCCP,Temp_A02B_CCCP, Temp_A03A_CCCP, Temp_A03B_CCCP, Temp_A04A_CCCP, Temp_A04B_CCCP, Temp_A05A_CCCP,Temp_A05B_CCCP, Temp_A06A_CCCP, Temp_A06B_CCCP, top="Transecto Amarales")
dev.off()

png(filename = "./03_Imagenes/Temperatura_Transecto_Amarales_CCCP.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_A01A_CCCP, Temp_A01B_CCCP, Temp_A02A_CCCP,Temp_A02B_CCCP, Temp_A03A_CCCP, Temp_A03B_CCCP, Temp_A04A_CCCP, Temp_A04B_CCCP, Temp_A05A_CCCP,Temp_A05B_CCCP, Temp_A06A_CCCP, Temp_A06B_CCCP, top="Transecto Amarales")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_A01A_CCCP, Sal_A01B_CCCP, Sal_A02A_CCCP,Sal_A02B_CCCP, Sal_A03A_CCCP, Sal_A03B_CCCP, Sal_A04A_CCCP, Sal_A04B_CCCP, Sal_A05A_CCCP,Sal_A05B_CCCP, Sal_A06A_CCCP, Sal_A06B_CCCP, top="Transecto Amarales")
dev.off()


tiff(filename = "./03_Imagenes/Densidad_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_A01A_CCCP, Den_A01B_CCCP, Den_A02A_CCCP,Den_A02B_CCCP, Den_A03A_CCCP, Den_A03B_CCCP, Den_A04A_CCCP, Den_A04B_CCCP, Den_A05A_CCCP,Den_A05B_CCCP, Den_A06A_CCCP, Den_A06B_CCCP, top="Transecto Amarales")
dev.off()

tiff(filename = "./03_Imagenes/Oxigeno_Transecto_Amarales_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Oxi_A01A_CCCP, Oxi_A01B_CCCP, Oxi_A02A_CCCP,Oxi_A02B_CCCP, Oxi_A03A_CCCP, Oxi_A03B_CCCP, Oxi_A04A_CCCP, Oxi_A04B_CCCP, Oxi_A05A_CCCP,Oxi_A05B_CCCP, Oxi_A06A_CCCP, Oxi_A06B_CCCP, top="Transecto Amarales")
dev.off()

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_S01A_CCCP, Temp_S01B_CCCP, Temp_S02A_CCCP,Temp_S02B_CCCP, Temp_S03A_CCCP, Temp_S03B_CCCP, Temp_S04A_CCCP, Temp_S04B_CCCP, Temp_S05A_CCCP,Temp_S05B_CCCP, Temp_S06A_CCCP, Temp_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_S01A_CCCP, Sal_S01B_CCCP, Sal_S02A_CCCP,Sal_S02B_CCCP, Sal_S03A_CCCP, Sal_S03B_CCCP, Sal_S04A_CCCP, Sal_S04B_CCCP, Sal_S05A_CCCP,Sal_S05B_CCCP, Sal_S06A_CCCP, Sal_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Densidad_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_S01A_CCCP, Den_S01B_CCCP, Den_S02A_CCCP,Den_S02B_CCCP, Den_S03A_CCCP, Den_S03B_CCCP, Den_S04A_CCCP, Den_S04B_CCCP, Den_S05A_CCCP,Den_S05B_CCCP, Den_S06A_CCCP, Den_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Oxigeno_Transecto_Sanquianga_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Oxi_S01A_CCCP, Oxi_S01B_CCCP, Oxi_S02A_CCCP,Oxi_S02B_CCCP, Oxi_S03A_CCCP, Oxi_S03B_CCCP, Oxi_S04A_CCCP, Oxi_S04B_CCCP, Oxi_S05A_CCCP,Oxi_S05B_CCCP, Oxi_S06A_CCCP, Oxi_S06B_CCCP, top="Transecto Sanquianga")
dev.off()

tiff(filename = "./03_Imagenes/Temperatura_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Temp_G01A_CCCP, Temp_G01B_CCCP, Temp_G02A_CCCP,Temp_G02B_CCCP, Temp_G03A_CCCP, Temp_G03B_CCCP, Temp_G04A_CCCP, Temp_G04B_CCCP, Temp_G05A_CCCP,Temp_G05B_CCCP, Temp_G06A_CCCP, Temp_G06B_CCCP, top="Transecto Guascama")
dev.off()

tiff(filename = "./03_Imagenes/Salinidad_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Sal_G01A_CCCP, Sal_G01B_CCCP, Sal_G02A_CCCP,Sal_G02B_CCCP, Sal_G03A_CCCP, Sal_G03B_CCCP, Sal_G04A_CCCP, Sal_G04B_CCCP, Sal_G05A_CCCP,Sal_G05B_CCCP, Sal_G06A_CCCP, Sal_G06B_CCCP, top="Transecto Guascama")
dev.off()

tiff(filename = "./03_Imagenes/Densidad_Transecto_Guascama_CCCP.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=6, ncol=2,Den_G01A_CCCP, Den_G01B_CCCP, Den_G02A_CCCP,Den_G02B_CCCP, Den_G03A_CCCP, Den_G03B_CCCP, Den_G04A_CCCP, Den_G04B_CCCP, Den_G05A_CCCP,Den_G05B_CCCP, Den_G06A_CCCP, Den_G06B_CCCP, top="Transecto Guascama")
dev.off()

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

