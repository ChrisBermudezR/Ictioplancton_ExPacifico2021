#Titulo: Visualización y Análisis descriptivo - Datos Químicos
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos químicos obtenidos de los análisis en laboratorio.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################
if(!require(tidyverse))install.packages("tidyverse")
if(!require(gridExtra))install.packages("gridExtra")
if(!require(gsw))install.packages("gsw")
if(!require(oce))install.packages("oce")
if(!require(lattice))install.packages("lattice")
if(!require(latticeExtra))install.packages("latticeExtra")
if(!require(GGally))install.packages("GGally")
if(!require(devtools))install.packages("devtools")
if(!require(raster))install.packages("raster")
if(!require(rgdal))install.packages("rgdal")
if(!require(dplyr))install.packages("dplyr")
if(!require(vegan))install.packages("vegan")
if(!require(spdep))install.packages("spdep")

source("../Funciones/rasterizar_Variable.R")
source("../Funciones/KruskalPostHoc.R")
source("../Funciones/boxplot_Marea.R")
source("../Funciones/boxplot_Sector.R")
source("../Funciones/boxplot_transecto.R")
source("../Funciones/Wilcoxon.R")
source("../Funciones/graf_lineas.R")



#Carga de datos química
Datos_Quimica<-read.table("./01_Datos_Quimicos/Datos_Quimica.csv", header = TRUE, sep=",")
as.factor(Datos_Quimica$Transecto)->Datos_Quimica$Transecto
as.numeric(Datos_Quimica$No.Estacion)->Datos_Quimica$No.Estacion
as.factor(Datos_Quimica$Codigo)->Datos_Quimica$Codigo
Datos_Quimica<-Datos_Quimica %>% mutate(Sector = case_when(
  No.Estacion == "6" ~ "Costero",
  No.Estacion == "5" ~ "Costero",
  No.Estacion == "4" ~ "Costero",
  No.Estacion == "3" ~ "Oceanico",
  No.Estacion == "2" ~ "Oceanico",
  No.Estacion == "1" ~ "Oceanico"
))
Datos_Quimica$Sector<-as.factor(Datos_Quimica$Sector)

#Cargade datos físicos asignados solo para la superficie
Datos_Fisica_Sup_CCCP<-read.table("../04_Analisis_Combinado/01_Datos/Fisicos_EstadisticasDescrip_CCCP.csv", header = TRUE, sep=",")
Datos_Fisica_Sup_CCCP$Codigo<-as.factor(Datos_Fisica_Sup_CCCP$Codigo)

#Uniendo los dos conjuntos de datos

Datos_Totales_CCCP <- base::merge(Datos_Quimica,Datos_Fisica_Sup_CCCP, by="Codigo")







colnames(Datos_Totales_CCCP)

Datos_Totales_Limpios<-Datos_Totales_CCCP %>% dplyr::select(
  Codigo,
  ID,
  Transecto,
  No.Estacion,
  Estacion,
  Sector,
  latitud ,          
  longitud,
  Fecha,              
  Hora ,              
  Marea ,             
  NO2,                
  NO3,               
  PO4,              
  SiO2,
  pH,
  OD,
  Transparencia,
  SST,
  TSI_SECCHI,
  Temperatura_median,
  Salinidad_median,
  Oxigeno_median,
  Densidad_median,
  Temperatura_IQR,
  Salinidad_IQR,
  Oxigeno_IQR,
  Densidad_IQR,
  Temperatura_Sup,
  Salinidad_Sup,
  Densidad_Sup
)
colnames(Datos_Totales_Limpios)

#Con el siguiente código se cambia el orden de los factores para que coincidan con el mapa
Datos_Totales_Limpios$Transecto <- factor(Datos_Totales_Limpios$Transecto, levels = c("Guascama", "Sanquianga", "Amarales"))
write_csv(Datos_Totales_Limpios, "./01_Datos_Quimicos/Datos_Totales_CCCP.csv", col_names = TRUE)
variables<-colnames(Datos_Totales_Limpios[12:31])



datos_descriptivos_Alta<-as.data.frame(summary(Datos_Totales_Limpios[Datos_Totales_Limpios$Marea == "Alta",]))
datos_descriptivos_Baja<-as.data.frame(summary(Datos_Totales_Limpios[Datos_Totales_Limpios$Marea == "Baja",]))
capture.output("#######################Marea Alta", datos_descriptivos_Alta, file = "datos_descriptivos_Marea_Alta.txt")
capture.output("#####################Marea baja", datos_descriptivos_Baja, file = "datos_descriptivos_Marea_baja.txt")


#Expresiones para las leyendas de las variables

Exp_NO2= expression(paste("[NO"[2]^"-","] [",mu,"M]"))
Exp_NO3= expression(paste("[NO"[3]^"-","] [",mu,"M]"))
Exp_PO4= expression(paste("[PO"[4]^-3,"] [",mu,"M]"))
Exp_SiO2= expression(paste("[SiO"[2],"] [",mu,"M]"))
#Exp_Clorofila=expression(paste("Clorofila [",mu,"g/L]"))
Exp_pH="pH en superficie "
Exp_OD=expression(paste("Ox.D. Sup.[mg O"[2],".L"^-1,"]"))
Exp_Transparencia="Transparencia (m)"
Exp_SST=expression(paste("SST[mg.L"^-1,"]"))
Exp_TSIDiscoSecchi="TSI Disco Secchi (m)"
Exp_medianTemp=expression(paste("Q"[2]," de la Temp. en prof. (°C)"))
Exp_mediansal=expression(paste("Q"[2]," de la Sal. en prof. (PSU)"))
Exp_medianoxi=expression(paste("Q"[2]," del Ox. D. en prof. [mg O"[2],".L"^-1,"]"))
Exp_medianden=expression(paste("Q"[2]," de la Den. (kg.m"^-3,")"))
Exp_IQRTemp=expression(paste("RIC Temp. en prof. (°C)"))
Exp_IQRSal=expression(paste("RIC Sal. en prof. (PSU)"))
Exp_IQROxi=expression(paste("RIC Ox. D. en prof. [mg O"[2],".L"^-1,"]"))
Exp_IQRDen=expression(paste("RIC Den.en prof. (kg.m"^-3,")"))
Exp_TempSup=expression(paste("Temp. Sup. (°C)"))
Exp_SalSup=expression(paste("Sal. Sup. (PSU)"))
Exp_DenSup=expression(paste("Den. Sup.(kg.m"^-3,")"))


MRPP_01_Datos_QuimicosMarea <- vegan::mrpp(dat = Datos_Totales_Limpios[,12:31], grouping = Datos_Totales_Limpios$Marea, permutations = 999)
MRPP_01_Datos_QuimicosTransecto <- vegan::mrpp(dat = Datos_Totales_Limpios[,12:31], grouping = Datos_Totales_Limpios$Transecto, permutations = 999)
MRPP_01_Datos_QuimicosSector <- vegan::mrpp(dat = Datos_Totales_Limpios[,12:31], grouping = Datos_Totales_Limpios$Sector, permutations = 999)

capture.output("MRPP Química - Mareas", 
               MRPP_01_Datos_QuimicosMarea,
               "MRPP Química - Transecto", 
               MRPP_01_Datos_QuimicosTransecto,
               "MRPP Química - Sectores",
               MRPP_01_Datos_QuimicosSector,
               file="./03_Resultados/MRPP_Fisico01_Datos_Quimicos.txt"
               )

####Creación de la visualización 

####Datos Físicos####

#Boxplot entre mareas


#ciclo para imprimir los objetos para ejecutar la función
#Se debe tomar lo que imprime en la consola, copiarlo en el script y borrar el [1] y las comillas que encierran la expresión
for (i in 1:34){
  print(paste0(variables[i], "_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$",variables[i],", ","'",etiqueta_para_y[i],"'",")"))
}



 NO2_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
 NO3_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
 PO4_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
 SiO2_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
 #Clorofila_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
 pH_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
 OD_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
 Transparencia_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
 SST_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
 TSI_SECCHI_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
 Temperatura_median_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
 Salinidad_median_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
 Oxigeno_median_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
 Densidad_median_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
 Temperatura_IQR_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_IQR, Exp_IQRTemp)
 Salinidad_IQR_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_IQR, Exp_IQRSal)
 Oxigeno_IQR_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_IQR, Exp_IQROxi)
 Densidad_IQR_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_IQR, Exp_IQRDen)
 Temperatura_Sup_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_Sup, Exp_TempSup)
 Salinidad_Sup_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_Sup, Exp_SalSup)
 Densidad_Sup_boxplot_mareas<-boxplot_marea(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_Sup, Exp_DenSup)


 
 for (i in 1:34){
   print(paste0(variables[i], "_boxplot_mareas"))
 }
 
 
 
 png(filename = "./02_Imagenes/boxplot_mareas_01.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
 grid.arrange(nrow=2, ncol=2, 
              NO2_boxplot_mareas, 
              NO3_boxplot_mareas, 
              PO4_boxplot_mareas, 
              SiO2_boxplot_mareas 
              )
 dev.off()
 
 

png(filename = "./02_Imagenes/boxplot_mareas_02.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                pH_boxplot_mareas,
                OD_boxplot_mareas,
                Transparencia_boxplot_mareas,
                SST_boxplot_mareas
                )
   dev.off()
 
   
png(filename = "./02_Imagenes/boxplot_mareas_03.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                TSI_SECCHI_boxplot_mareas,
                Temperatura_median_boxplot_mareas,
                Salinidad_median_boxplot_mareas,
                Oxigeno_median_boxplot_mareas
                )
   dev.off()
   

   png(filename = "./02_Imagenes/boxplot_mareas_04.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                Densidad_median_boxplot_mareas,
                Temperatura_IQR_boxplot_mareas,
                Salinidad_IQR_boxplot_mareas,
                Oxigeno_IQR_boxplot_mareas
   )
   dev.off()
   
   png(filename = "./02_Imagenes/boxplot_mareas_05.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                Densidad_IQR_boxplot_mareas,
                Temperatura_Sup_boxplot_mareas,
                Salinidad_Sup_boxplot_mareas,
                Densidad_Sup_boxplot_mareas
   )
   dev.off()
 
 
 #####
 

   NO2_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
   NO3_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
   PO4_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
   SiO2_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
   #Clorofila_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
   pH_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
   OD_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
   Transparencia_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
   SST_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
   TSI_SECCHI_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
   Temperatura_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
   Salinidad_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
   Oxigeno_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
   Densidad_median_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
   Temperatura_IQR_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_IQR, Exp_IQRTemp)
   Salinidad_IQR_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_IQR, Exp_IQRSal)
   Oxigeno_IQR_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_IQR, Exp_IQROxi)
   Densidad_IQR_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_IQR, Exp_IQRDen)
   Temperatura_Sup_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_Sup, Exp_TempSup)
   Salinidad_Sup_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_Sup, Exp_SalSup)
   Densidad_Sup_boxplot_transecto<-boxplot_transecto(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_Sup, Exp_DenSup)
 
   
   png(filename = "./02_Imagenes/boxplot_transecto_01.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                NO2_boxplot_transecto, 
                NO3_boxplot_transecto, 
                PO4_boxplot_transecto, 
                SiO2_boxplot_transecto 
   )
   dev.off()
   
   
   
   png(filename = "./02_Imagenes/boxplot_transecto_02.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                pH_boxplot_transecto,
                OD_boxplot_transecto,
                Transparencia_boxplot_transecto,
                SST_boxplot_transecto
   )
   dev.off()
   
   
   png(filename = "./02_Imagenes/boxplot_transecto_03.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                TSI_SECCHI_boxplot_transecto,
                Temperatura_median_boxplot_transecto,
                Salinidad_median_boxplot_transecto,
                Oxigeno_median_boxplot_transecto
   )
   dev.off()
   
   
   png(filename = "./02_Imagenes/boxplot_transecto_04.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                Densidad_median_boxplot_transecto,
                Temperatura_IQR_boxplot_transecto,
                Salinidad_IQR_boxplot_transecto,
                Oxigeno_IQR_boxplot_transecto
   )
   dev.off()
   
   png(filename = "./02_Imagenes/boxplot_transecto_05.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
   grid.arrange(nrow=2, ncol=2, 
                Densidad_IQR_boxplot_transecto,
                Temperatura_Sup_boxplot_transecto,
                Salinidad_Sup_boxplot_transecto,
                Densidad_Sup_boxplot_transecto
   )
   dev.off()
 
 



NO2_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO2, Exp_NO2)
NO3_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$NO3, Exp_NO3)
PO4_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$PO4, Exp_PO4)
SiO2_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SiO2, Exp_SiO2)
#Clorofila_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Clorofila, Exp_Clorofila)
pH_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$pH, Exp_pH)
OD_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$OD, Exp_OD)
Transparencia_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Transparencia, Exp_Transparencia)
SST_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$SST, Exp_SST)
TSI_SECCHI_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$TSI_SECCHI, Exp_TSIDiscoSecchi)
Temperatura_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_median, Exp_medianTemp)
Salinidad_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_median, Exp_mediansal)
Oxigeno_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_median, Exp_medianoxi)
Densidad_median_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_median, Exp_medianden)
Temperatura_IQR_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_IQR, Exp_IQRTemp)
Salinidad_IQR_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_IQR, Exp_IQRSal)
Oxigeno_IQR_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Oxigeno_IQR, Exp_IQROxi)
Densidad_IQR_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_IQR, Exp_IQRDen)
Temperatura_Sup_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Temperatura_Sup, Exp_TempSup)
Salinidad_Sup_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Salinidad_Sup, Exp_SalSup)
Densidad_Sup_graf_lineas<-graf_lineas(Datos_Totales_Limpios, Datos_Totales_Limpios$Densidad_Sup, Exp_DenSup)


png(filename = "./02_Imagenes/graf_lineas_01.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, 
             NO2_graf_lineas, 
             NO3_graf_lineas, 
             PO4_graf_lineas, 
             SiO2_graf_lineas 
)
dev.off()



png(filename = "./02_Imagenes/graf_lineas_02.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, 
             pH_graf_lineas,
             OD_graf_lineas,
             Transparencia_graf_lineas,
             SST_graf_lineas
)
dev.off()


png(filename = "./02_Imagenes/graf_lineas_03.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, 
             TSI_SECCHI_graf_lineas,
             Temperatura_median_graf_lineas,
             Salinidad_median_graf_lineas,
             Oxigeno_median_graf_lineas
)
dev.off()


png(filename = "./02_Imagenes/graf_lineas_04.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, 
             Densidad_median_graf_lineas,
             Temperatura_IQR_graf_lineas,
             Salinidad_IQR_graf_lineas,
             Oxigeno_IQR_graf_lineas
)
dev.off()

png(filename = "./02_Imagenes/graf_lineas_05.png", width = 20, height = 12, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, 
             Densidad_IQR_graf_lineas,
             Temperatura_Sup_graf_lineas,
             Salinidad_Sup_graf_lineas,
             Densidad_Sup_graf_lineas
)
dev.off()
 
 














 

 





png(filename = "./02_Imagenes/GRaf_01.png", width = 20, height = 20, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=4, ncol=2, 
             NO2_boxplot_mareas,
             NO2_graf_lineas,
             NO3_boxplot_mareas,
             NO3_graf_lineas, 
             PO4_boxplot_mareas,
             PO4_graf_lineas,
             SiO2_boxplot_mareas,
             SiO2_graf_lineas 
)
dev.off()






png(filename = "./02_Imagenes/GRaf_02.png", width = 20, height = 20, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=4, ncol=2, 
             pH_boxplot_mareas,
             pH_graf_lineas,
             OD_boxplot_mareas,
             OD_graf_lineas,
             Transparencia_boxplot_mareas,
             Transparencia_graf_lineas,
             SST_boxplot_mareas,
             SST_graf_lineas
)
dev.off()







png(filename = "./02_Imagenes/GRaf_03.png", width = 20, height = 20, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=4, ncol=2, 
             TSI_SECCHI_boxplot_mareas,
             TSI_SECCHI_graf_lineas,
             Temperatura_median_boxplot_mareas,
             Temperatura_median_graf_lineas,
             Salinidad_median_boxplot_mareas,
             Salinidad_median_graf_lineas,
             Oxigeno_median_boxplot_mareas,
             Oxigeno_median_graf_lineas
)
dev.off()







png(filename = "./02_Imagenes/GRaf_04.png", width = 20, height = 20, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=4, ncol=2,  
             Densidad_median_boxplot_mareas,
             Densidad_median_graf_lineas,
             Temperatura_IQR_boxplot_mareas,
             Temperatura_IQR_graf_lineas,
             Salinidad_IQR_boxplot_mareas,
             Salinidad_IQR_graf_lineas,
             Oxigeno_IQR_boxplot_mareas,
             Oxigeno_IQR_graf_lineas
)
dev.off()







png(filename = "./02_Imagenes/GRaf_05.png", width = 20, height = 20, units = "cm", pointsize = 10, bg = "white", res = 300)
grid.arrange(nrow=4, ncol=2, 
             Densidad_IQR_boxplot_mareas,
             Densidad_IQR_graf_lineas,
             Temperatura_Sup_boxplot_mareas,
             Temperatura_Sup_graf_lineas,
             Salinidad_Sup_boxplot_mareas,
             Salinidad_Sup_graf_lineas,
             Densidad_Sup_boxplot_mareas,
             Densidad_Sup_graf_lineas
)
dev.off()







library(spdep)
marea_alta<-Datos_Totales_Limpios%>% filter(Marea=="Alta")
marea_baja<-Datos_Totales_Limpios%>% filter(Marea=="Baja")
#Calculo de distancias

install.packages("ape")
library(ape)




geo_dis<-as.matrix(dist(cbind(marea_alta$longitud, marea_alta$latitud)))
geo_Inv_Dist<-1/geo_dis
diag(geo_Inv_Dist)<-0


library(gstat)


source("../Funciones/Wilcoxon.R")
source("../Funciones/KrusckalPotHoc.R")
NO2_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$ NO2, "./03_Resultados/NO2_Transectos_KrusckalPotHoc.txt", " NO2_Transectos_KrusckalPotHoc")
NO2_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$NO2, "./03_Resultados/NO2_Marea_Wilcoxon.txt", "NO2_Marea_Wilcoxon")
NO2_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$NO2, "./03_Resultados/NO2_Sector_Wilcoxon.txt", "NO2_Sector_Wilcoxon")

moran_Baja_NO2<-Moran.I(marea_baja$NO2, geo_Inv_Dist)
moran_Alta_NO2<-Moran.I(marea_alta$NO2, geo_Inv_Dist)


NO3_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$NO3, "./03_Resultados/NO3_Transectos_KrusckalPotHoc.txt", " NO3_Transectos_KrusckalPotHoc")
NO3_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$NO3, "./03_Resultados/NO3_Marea_Wilcoxon.txt", "NO3_Marea_Wilcoxon")
NO3_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$NO3, "./03_Resultados/NO3_Sector_Wilcoxon.txt", "NO3_Sector_Wilcoxon")

moran_Baja_NO3<-Moran.I(marea_baja$NO3, geo_Inv_Dist)
moran_Alta_NO3<-Moran.I(marea_alta$NO3, geo_Inv_Dist)

PO4_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$PO4, "./03_Resultados/PO4_Transectos_KrusckalPotHoc.txt", " PO4_Transectos_KrusckalPotHoc")
PO4_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$PO4, "./03_Resultados/PO4_Marea_Wilcoxon.txt", "PO4_Marea_Wilcoxon")
PO4_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$PO4, "./03_Resultados/PO4_Sector_Wilcoxon.txt", "PO4_Sector_Wilcoxon")

moran_Baja_PO4<-Moran.I(marea_baja$PO4, geo_Inv_Dist)
moran_Alta_PO4<-Moran.I(marea_alta$PO4, geo_Inv_Dist)

SiO2_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$SiO2, "./03_Resultados/SiO2_Transectos_KrusckalPotHoc.txt", " SiO2_Transectos_KrusckalPotHoc")
SiO2_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$SiO2, "./03_Resultados/SiO2_Marea_Wilcoxon.txt", "SiO2_Marea_Wilcoxon")
SiO2_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$SiO2, "./03_Resultados/SiO2_Sector_Wilcoxon.txt", "SiO2_Sector_Wilcoxon")

moran_Baja_SiO2<-Moran.I(marea_baja$SiO2, geo_Inv_Dist)
moran_Alta_SiO2<-Moran.I(marea_alta$SiO2, geo_Inv_Dist)

pH_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$pH, "./03_Resultados/pH_Transectos_KrusckalPotHoc.txt", " pH_Transectos_KrusckalPotHoc")
pH_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$pH, "./03_Resultados/pH_Marea_Wilcoxon.txt", "pH_Marea_Wilcoxon")
pH_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$pH, "./03_Resultados/pH_Sector_Wilcoxon.txt", "pH_Sector_Wilcoxon")

moran_Baja_pH<-Moran.I(marea_baja$pH, geo_Inv_Dist)
moran_Alta_pH<-Moran.I(marea_alta$pH, geo_Inv_Dist)


OD_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$OD, "./03_Resultados/OD_Transectos_KrusckalPotHoc.txt", " OD_Transectos_KrusckalPotHoc")
OD_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$OD, "./03_Resultados/OD_Marea_Wilcoxon.txt", "OD_Marea_Wilcoxon")
OD_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$OD, "./03_Resultados/OD_Sector_Wilcoxon.txt", "OD_Sector_Wilcoxon")

moran_Baja_OD<-Moran.I(marea_baja$OD, geo_Inv_Dist)
moran_Alta_OD<-Moran.I(marea_alta$OD, geo_Inv_Dist)



Transparencia_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Transparencia, "./03_Resultados/Transparencia_Transectos_KrusckalPotHoc.txt", " Transparencia_Transectos_KrusckalPotHoc")
Transparencia_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Transparencia, "./03_Resultados/Transparencia_Marea_Wilcoxon.txt", "Transparencia_Marea_Wilcoxon")
Transparencia_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Transparencia, "./03_Resultados/Transparencia_Sector_Wilcoxon.txt", "Transparencia_Sector_Wilcoxon")

moran_Baja_Transparencia<-Moran.I(marea_baja$Transparencia, geo_Inv_Dist)
moran_Alta_Transparencia<-Moran.I(marea_alta$Transparencia, geo_Inv_Dist)

SST_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$SST, "./03_Resultados/SST_Transectos_KrusckalPotHoc.txt", " SST_Transectos_KrusckalPotHoc")
SST_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$SST, "./03_Resultados/SST_Marea_Wilcoxon.txt", "SST_Marea_Wilcoxon")
SST_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$SST, "./03_Resultados/SST_Sector_Wilcoxon.txt", "SST_Sector_Wilcoxon")

moran_Baja_SST<-Moran.I(marea_baja$SST, geo_Inv_Dist)
moran_Alta_SST<-Moran.I(marea_alta$SST, geo_Inv_Dist)


TSI_SECCHI_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$TSI_SECCHI, "./03_Resultados/TSI_SECCHI_Transectos_KrusckalPotHoc.txt", " TSI_SECCHI_Transectos_KrusckalPotHoc")
TSI_SECCHI_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$TSI_SECCHI, "./03_Resultados/TSI_SECCHI_Marea_Wilcoxon.txt", "TSI_SECCHI_Marea_Wilcoxon")
TSI_SECCHI_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$TSI_SECCHI, "./03_Resultados/TSI_SECCHI_Sector_Wilcoxon.txt", "TSI_SECCHI_Sector_Wilcoxon")

moran_Baja_TSI_SECCHI<-Moran.I(marea_baja$TSI_SECCHI, geo_Inv_Dist)
moran_Alta_TSI_SECCHI<-Moran.I(marea_alta$TSI_SECCHI, geo_Inv_Dist)





Temperatura_median_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Temperatura_median, "./03_Resultados/Temperatura_median_Transectos_KrusckalPotHoc.txt", " Temperatura_median_Transectos_KrusckalPotHoc")
Temperatura_median_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Temperatura_median, "./03_Resultados/Temperatura_median_Marea_Wilcoxon.txt", "Temperatura_median_Marea_Wilcoxon")
Temperatura_median_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Temperatura_median, "./03_Resultados/Temperatura_median_Sector_Wilcoxon.txt", "Temperatura_median_Sector_Wilcoxon")

moran_Baja_Temperatura_median<-Moran.I(marea_baja$Temperatura_median, geo_Inv_Dist)
moran_Alta_Temperatura_median<-Moran.I(marea_alta$Temperatura_median, geo_Inv_Dist)

Salinidad_median_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Salinidad_median, "./03_Resultados/Salinidad_median_Transectos_KrusckalPotHoc.txt", " Salinidad_median_Transectos_KrusckalPotHoc")
Salinidad_median_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Salinidad_median, "./03_Resultados/Salinidad_median_Marea_Wilcoxon.txt", "Salinidad_median_Marea_Wilcoxon")
Salinidad_median_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Salinidad_median, "./03_Resultados/Salinidad_median_Sector_Wilcoxon.txt", "Salinidad_median_Sector_Wilcoxon")

moran_Baja_Salinidad_median<-Moran.I(marea_baja$Salinidad_median, geo_Inv_Dist)
moran_Alta_Salinidad_median<-Moran.I(marea_alta$Salinidad_median, geo_Inv_Dist)


Oxigeno_median_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Oxigeno_median, "./03_Resultados/Oxigeno_median_Transectos_KrusckalPotHoc.txt", " Oxigeno_median_Transectos_KrusckalPotHoc")
Oxigeno_median_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Oxigeno_median, "./03_Resultados/Oxigeno_median_Marea_Wilcoxon.txt", "Oxigeno_median_Marea_Wilcoxon")
Oxigeno_median_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Oxigeno_median, "./03_Resultados/Oxigeno_median_Sector_Wilcoxon.txt", "Oxigeno_median_Sector_Wilcoxon")

moran_Baja_Oxigeno_median<-Moran.I(marea_baja$Oxigeno_median, geo_Inv_Dist)
moran_Alta_Oxigeno_median<-Moran.I(marea_alta$Oxigeno_median, geo_Inv_Dist)



Densidad_median_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Densidad_median, "./03_Resultados/Densidad_median_Transectos_KrusckalPotHoc.txt", " Densidad_median_Transectos_KrusckalPotHoc")
Densidad_median_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Densidad_median, "./03_Resultados/Densidad_median_Marea_Wilcoxon.txt", "Densidad_median_Marea_Wilcoxon")
Densidad_median_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Densidad_median, "./03_Resultados/Densidad_median_Sector_Wilcoxon.txt", "Densidad_median_Sector_Wilcoxon")

moran_Baja_Densidad_median<-Moran.I(marea_baja$Densidad_median, geo_Inv_Dist)
moran_Alta_Densidad_median<-Moran.I(marea_alta$Densidad_median, geo_Inv_Dist)


Temperatura_IQR_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Temperatura_IQR, "./03_Resultados/Temperatura_IQR_Transectos_KrusckalPotHoc.txt", " Temperatura_IQR_Transectos_KrusckalPotHoc")
Temperatura_IQR_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Temperatura_IQR, "./03_Resultados/Temperatura_IQR_Marea_Wilcoxon.txt", "Temperatura_IQR_Marea_Wilcoxon")
Temperatura_IQR_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Temperatura_IQR, "./03_Resultados/Temperatura_IQR_Sector_Wilcoxon.txt", "Temperatura_IQR_Sector_Wilcoxon")

moran_Baja_Temperatura_IQR<-Moran.I(marea_baja$Temperatura_IQR, geo_Inv_Dist)
moran_Alta_Temperatura_IQR<-Moran.I(marea_alta$Temperatura_IQR, geo_Inv_Dist)

Salinidad_IQR_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Salinidad_IQR , "./03_Resultados/Salinidad_IQR _Transectos_KrusckalPotHoc.txt", " Salinidad_IQR _Transectos_KrusckalPotHoc")
Salinidad_IQR_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Salinidad_IQR , "./03_Resultados/Salinidad_IQR _Marea_Wilcoxon.txt", "Salinidad_IQR _Marea_Wilcoxon")
Salinidad_IQR_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Salinidad_IQR , "./03_Resultados/Salinidad_IQR _Sector_Wilcoxon.txt", "Salinidad_IQR _Sector_Wilcoxon")

moran_Baja_Salinidad_IQR <-Moran.I(marea_baja$Salinidad_IQR , geo_Inv_Dist)
moran_Alta_Salinidad_IQR <-Moran.I(marea_alta$Salinidad_IQR , geo_Inv_Dist)

Oxigeno_IQR_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Oxigeno_IQR, "./03_Resultados/Oxigeno_IQR_Transectos_KrusckalPotHoc.txt", " Oxigeno_IQR_Transectos_KrusckalPotHoc")
Oxigeno_IQR_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Oxigeno_IQR, "./03_Resultados/Oxigeno_IQR_Marea_Wilcoxon.txt", "Oxigeno_IQR_Marea_Wilcoxon")
Oxigeno_IQR_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Oxigeno_IQR, "./03_Resultados/Oxigeno_IQR_Sector_Wilcoxon.txt", "Oxigeno_IQR_Sector_Wilcoxon")

moran_Baja_Oxigeno_IQR<-Moran.I(marea_baja$Oxigeno_IQR, geo_Inv_Dist)
moran_Alta_Oxigeno_IQR<-Moran.I(marea_alta$Oxigeno_IQR, geo_Inv_Dist)


Densidad_IQR_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Densidad_IQR, "./03_Resultados/Densidad_IQR_Transectos_KrusckalPotHoc.txt", " Densidad_IQR_Transectos_KrusckalPotHoc")
Densidad_IQR_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Densidad_IQR, "./03_Resultados/Densidad_IQR_Marea_Wilcoxon.txt", "Densidad_IQR_Marea_Wilcoxon")
Densidad_IQR_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Densidad_IQR, "./03_Resultados/Densidad_IQR_Sector_Wilcoxon.txt", "Densidad_IQR_Sector_Wilcoxon")

moran_Baja_Densidad_IQR<-Moran.I(marea_baja$Densidad_IQR, geo_Inv_Dist)
moran_Alta_Densidad_IQR<-Moran.I(marea_alta$Densidad_IQR, geo_Inv_Dist)


Temperatura_Sup_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Temperatura_Sup, "./03_Resultados/Temperatura_Sup_Transectos_KrusckalPotHoc.txt", " Temperatura_Sup_Transectos_KrusckalPotHoc")
Temperatura_Sup_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Temperatura_Sup, "./03_Resultados/Temperatura_Sup_Marea_Wilcoxon.txt", "Temperatura_Sup_Marea_Wilcoxon")
Temperatura_Sup_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Temperatura_Sup, "./03_Resultados/Temperatura_Sup_Sector_Wilcoxon.txt", "Temperatura_Sup_Sector_Wilcoxon")

moran_Baja_Temperatura_Sup<-Moran.I(marea_baja$Temperatura_Sup, geo_Inv_Dist)
moran_Alta_Temperatura_Sup<-Moran.I(marea_alta$Temperatura_Sup, geo_Inv_Dist)



Salinidad_Sup_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Salinidad_Sup , "./03_Resultados/Salinidad_Sup _Transectos_KrusckalPotHoc.txt", " Salinidad_Sup _Transectos_KrusckalPotHoc")
Salinidad_Sup_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Salinidad_Sup , "./03_Resultados/Salinidad_Sup _Marea_Wilcoxon.txt", "Salinidad_Sup _Marea_Wilcoxon")
Salinidad_Sup_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Salinidad_Sup , "./03_Resultados/Salinidad_Sup _Sector_Wilcoxon.txt", "Salinidad_Sup _Sector_Wilcoxon")

moran_Baja_Salinidad_Sup <-Moran.I(marea_baja$Salinidad_Sup , geo_Inv_Dist)
moran_Alta_Salinidad_Sup <-Moran.I(marea_alta$Salinidad_Sup , geo_Inv_Dist)


Densidad_Sup_KrusckalPotHoc_mareas<-KrusckalPotHoc(Datos_Totales_Limpios$Transecto, Datos_Totales_Limpios$Densidad_Sup, "./03_Resultados/Densidad_Sup_Transectos_KrusckalPotHoc.txt", " Densidad_Sup_Transectos_KrusckalPotHoc")
Densidad_Sup_Marea_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Marea, Datos_Totales_Limpios$Densidad_Sup, "./03_Resultados/Densidad_Sup_Marea_Wilcoxon.txt", "Densidad_Sup_Marea_Wilcoxon")
Densidad_Sup_Sector_Wilcoxon<-Wilcoxon(Datos_Totales_Limpios$Sector, Datos_Totales_Limpios$Densidad_Sup, "./03_Resultados/Densidad_Sup_Sector_Wilcoxon.txt", "Densidad_Sup_Sector_Wilcoxon")

moran_Baja_Densidad_Sup<-Moran.I(marea_baja$Densidad_Sup, geo_Inv_Dist)
moran_Alta_Densidad_Sup<-Moran.I(marea_alta$Densidad_Sup, geo_Inv_Dist)


spdep::moran.test(marea_baja$Densidad_Sup, geo_Inv_Dist)


####Captura de Información####
capture.output(
  "######NO2####",
  
  "#Baja",
  moran_Baja_NO2,
  "#Alta",
  moran_Alta_NO2,
  "######NO3####",
  "#Baja",
  moran_Baja_NO3,
  "#Alta",
  moran_Alta_NO3,
  
  "######PO4####",
  "#Baja",
  moran_Baja_PO4,
  "#Alta",
  moran_Alta_PO4,
  
  "######SiO2####",
  "#Baja",
  moran_Baja_SiO2,
  "#Alta",
  moran_Alta_SiO2,
  
  "######pH####",
  "#Baja",
  moran_Baja_pH,
  "#Alta",
  moran_Alta_pH,
  
  
"######OD####",
  "#Baja",
  moran_Baja_OD,
  "#Alta",
  moran_Alta_OD,
  
"######Transparencia####",
  "#Baja",
  moran_Baja_Transparencia,
  "#Alta",
  moran_Alta_Transparencia,
  
  "######SST####",
  "#Baja",
  moran_Baja_SST,
  "#Alta",
  moran_Alta_SST,
  
  "######TSI####",
  "#Baja",
  moran_Baja_TSI_SECCHI,
  "#Alta",
  moran_Alta_TSI_SECCHI,
  
  "######Temperatura_median####",
  "#Baja",
  moran_Baja_Temperatura_median,
  "#Alta",
  moran_Alta_Temperatura_median,
  
  "######Salinidad_median####",
  "#Baja",
  moran_Baja_Salinidad_median,
  "#Alta",
  moran_Alta_Salinidad_median,
  
  "######Oxigeno_median####",
  "#Baja",
  moran_Baja_Oxigeno_median,
  "#Alta",
  moran_Alta_Oxigeno_median,
  
  "######Densidad_median####",
  "#Baja",
  moran_Baja_Densidad_median,
  "#Alta",
  moran_Alta_Densidad_median,
  
  
  "######Temperatura_IQR####",
  "#Baja",
  moran_Baja_Temperatura_IQR,
  "#Alta",
  moran_Alta_Temperatura_IQR,
  
  
  "######Salinidad_IQR####",
  "#Baja",
  moran_Baja_Salinidad_IQR,
  "#Alta",
  moran_Alta_Salinidad_IQR,
  
  
  "######Oxigeno_IQR####",
  "#Baja",
  moran_Baja_Oxigeno_IQR,
  "#Alta",
  moran_Alta_Oxigeno_IQR,
  
  "######Densidad_IQR####",
  "#Baja",
  moran_Baja_Densidad_IQR,
  "#Alta",
  moran_Alta_Densidad_IQR,
  
  "######Temperatura_Sup####",
  "#Baja",
  moran_Baja_Temperatura_Sup,
  "#Alta",
  moran_Alta_Temperatura_Sup,
  
  
  "######Salinidad_Sup####",
  "#Baja",
  moran_Baja_Salinidad_Sup,
  "#Alta",
  moran_Alta_Salinidad_Sup,
  
  "######Densidad_Sup####",
  "#Baja",
  moran_Baja_Densidad_Sup,
  "#Alta",
  moran_Alta_Densidad_Sup,
  
  "######Densidad_Sup####",
  "#Baja",
  moran_Baja_Densidad_Sup,
  "#Alta",
  moran_Alta_Densidad_Sup,
  
  
  file="Autocorrelacion_Espacial_Quimica.txt"  
)



 ################################################

   
   marea_alta<-Datos_Totales_Limpios%>% filter(Marea=="Alta")
   marea_baja<-Datos_Totales_Limpios%>% filter(Marea=="Baja")
   
   costa<-readOGR("../SIG_Datos/costa.shp")
   rios<-readOGR("../SIG_Datos/rios_wgs84.shp")
   estaciones<-readOGR("../SIG_Datos/estaciones.shp")
   areas_protegidas<-readOGR("../SIG_Datos/areas_protegidas.shp")
 

 rasterizar_Variable("NO2", marea_baja$longitud, marea_baja$latitud, marea_baja$NO2, "Baja",Exp_NO2)
 

 for (i in colnames(marea_baja)[11:44]){
   print(paste0(i, "_BajaGrid<-rasterizar_Variable('",i,"', marea_baja$longitud, marea_baja$latitud, marea_baja$",i,", 'Baja',  Exp_", i, ")"))
   
 }
 
 Exp_NO2= expression(paste("[NO"[2]^"-","] [",mu,"M]"))
 Exp_NO3= expression(paste("[NO"[3]^"-","] [",mu,"M]"))
 Exp_PO4= expression(paste("[PO"[4]^-3,"] [",mu,"M]"))
 Exp_SiO2= expression(paste("[SiO"[2],"] [",mu,"M]"))
 #Exp_Clorofila=expression(paste("Clorofila [",mu,"g/L]"))
 Exp_pH="pH en superficie "
 Exp_OD=expression(paste("Ox.D. Sup.[mg O"[2],".L"^-1,"]"))
 Exp_Transparencia="Transparencia (m)"
 Exp_SST=expression(paste("SST[mg.L"^-1,"]"))
 Exp_TSIDiscoSecchi="TSI Disco Secchi (m)"
 Exp_medianTemp=expression(paste("Q"[2]," de la Temp. en prof. (°C)"))
 Exp_mediansal=expression(paste("Q"[2]," de la Sal. en prof. (PSU)"))
 Exp_medianoxi=expression(paste("Q"[2]," del Ox. D. en prof. [mg O"[2],".L"^-1,"]"))
 Exp_medianden=expression(paste("Q"[2]," de la Den. (kg.m"^-3,")"))
 Exp_IQRTemp=expression(paste("RIC Temp. en prof. (°C)"))
 Exp_IQRSal=expression(paste("RIC Sal. en prof. (PSU)"))
 Exp_IQROxi=expression(paste("RIC Ox. D. en prof. [mg O"[2],".L"^-1,"]"))
 Exp_IQRDen=expression(paste("RIC Den.en prof. (kg.m"^-3,")"))
 Exp_TempSup=expression(paste("Temp. Sup. (°C)"))
 Exp_SalSup=expression(paste("Sal. Sup. (PSU)"))
 Exp_DenSup=expression(paste("Den. Sup.(kg.m"^-3,")"))
 
 NO2_BajaGrid<-rasterizar_Variable('NO2', marea_baja$longitud, marea_baja$latitud, marea_baja$NO2, 'Baja',  Exp_NO2, "Nitritos - Baja")
 NO3_BajaGrid<-rasterizar_Variable('NO3', marea_baja$longitud, marea_baja$latitud, marea_baja$NO3, 'Baja',  Exp_NO3, "Nitratos - Baja")
 PO4_BajaGrid<-rasterizar_Variable('PO4', marea_baja$longitud, marea_baja$latitud, marea_baja$PO4, 'Baja',  Exp_PO4, "Fosfatos - Baja")
 SiO2_BajaGrid<-rasterizar_Variable('SiO2', marea_baja$longitud, marea_baja$latitud, marea_baja$SiO2, 'Baja',  Exp_SiO2, "Silicatos - Baja")
 #Clorofila_BajaGrid<-rasterizar_Variable('Clorofila', marea_baja$longitud, marea_baja$latitud, marea_baja$Clorofila, 'Baja',  Exp_Clorofila2)
 pH_BajaGrid<-rasterizar_Variable('pH', marea_baja$longitud, marea_baja$latitud, marea_baja$pH, 'Baja',  "pH", "pH")
 OD_BajaGrid<-rasterizar_Variable('OD', marea_baja$longitud, marea_baja$latitud, marea_baja$OD, 'Baja',  expression(paste("[mg.L"^-1,"]")),"Oxi. Sup. - Baja")
 Transparencia_BajaGrid<-rasterizar_Variable('Transparencia', marea_baja$longitud, marea_baja$latitud, marea_baja$Transparencia, 'Baja',  "(m)", 'Transparencia - Baja')
 SST_BajaGrid<-rasterizar_Variable('SST', marea_baja$longitud, marea_baja$latitud, marea_baja$SST, 'Baja',  expression(paste("[mg.L"^-1,"]")), 'SST - Baja')
 TSI_SECCHI_BajaGrid<-rasterizar_Variable('TSI_SECCHI', marea_baja$longitud, marea_baja$latitud, marea_baja$TSI_SECCHI, 'Baja',  "(m)", 'TSI_SECCHI - Baja')
 Temperatura_median_BajaGrid<-rasterizar_Variable("Temperatura_median", marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_median, 'Baja',  "(°C)", expression(paste("Q"[2]," Temp - Baja")))
 Salinidad_median_BajaGrid<-rasterizar_Variable('Salinidad_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_median, 'Baja',  "(PSU)", expression(paste("Q"[2]," Sal - Baja")))
 Oxigeno_median_BajaGrid<-rasterizar_Variable('Oxigeno_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_median, 'Baja',  expression(paste("[mg.L"^-1,"]")), expression(paste("Q"[2]," Oxi - Baja")))
 Densidad_median_BajaGrid<-rasterizar_Variable('Densidad_median', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_median, 'Baja',  expression(paste("(kg.m"^-3,")")), expression(paste("Q"[2]," Den - Baja")))
 Temperatura_IQR_BajaGrid<-rasterizar_Variable('Temperatura_IQR', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_IQR, 'Baja',  "(°C)", "RIC Temp - Baja")
 Salinidad_IQR_BajaGrid<-rasterizar_Variable('Salinidad_IQR', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_IQR, 'Baja',  "(PSU)", "RIC Sal - Baja")
 Oxigeno_IQR_BajaGrid<-rasterizar_Variable('Oxigeno_IQR', marea_baja$longitud, marea_baja$latitud, marea_baja$Oxigeno_IQR, 'Baja',  expression(paste("[mg.L"^-1,"]")), "RIC Oxi - Baja")
 Densidad_IQR_BajaGrid<-rasterizar_Variable('Densidad_IQR', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_IQR, 'Baja',  expression(paste("(kg.m"^-3,")")), "RIC Den - Baja")
 Temperatura_Sup_BajaGrid<-rasterizar_Variable('Temperatura_Sup', marea_baja$longitud, marea_baja$latitud, marea_baja$Temperatura_Sup, 'Baja',  "(°C)", "Temp. Sup - Baja")
 Salinidad_Sup_BajaGrid<-rasterizar_Variable('Salinidad_Sup', marea_baja$longitud, marea_baja$latitud, marea_baja$Salinidad_Sup, 'Baja',  "(PSU)", "Sal. Sup. - Baja")
 Densidad_Sup_BajaGrid<-rasterizar_Variable('Densidad_Sup', marea_baja$longitud, marea_baja$latitud, marea_baja$Densidad_Sup, 'Baja',  expression(paste("(kg.m"^-3,")")), "Den. Sup. - Baja")
 
 
 
 NO2_AltaGrid<-rasterizar_Variable('NO2', marea_alta$longitud, marea_alta$latitud, marea_alta$NO2, 'Alta',  Exp_NO2, "Nitritos - Alta")
 NO3_AltaGrid<-rasterizar_Variable('NO3', marea_alta$longitud, marea_alta$latitud, marea_alta$NO3, 'Alta',  Exp_NO3, "Nitratos - Alta")
 PO4_AltaGrid<-rasterizar_Variable('PO4', marea_alta$longitud, marea_alta$latitud, marea_alta$PO4, 'Alta',  Exp_PO4, "Fosfatos - Alta")
 SiO2_AltaGrid<-rasterizar_Variable('SiO2', marea_alta$longitud, marea_alta$latitud, marea_alta$SiO2, 'Alta',  Exp_SiO2, "Silicatos - Alta")
 #Clorofila_AltaGrid<-rasterizar_Variable('Clorofila', marea_alta$longitud, marea_alta$latitud, marea_alta$Clorofila, 'Alta',  Exp_Clorofila2)
 pH_AltaGrid<-rasterizar_Variable('pH', marea_alta$longitud, marea_alta$latitud, marea_alta$pH, 'Alta',  "pH", "pH")
 OD_AltaGrid<-rasterizar_Variable('OD', marea_alta$longitud, marea_alta$latitud, marea_alta$OD, 'Alta',  expression(paste("[mg.L"^-1,"]")),"Oxi. Sup. - Alta")
 Transparencia_AltaGrid<-rasterizar_Variable('Transparencia', marea_alta$longitud, marea_alta$latitud, marea_alta$Transparencia, 'Alta',  "(m)", 'Transparencia - Alta')
 SST_AltaGrid<-rasterizar_Variable('SST', marea_alta$longitud, marea_alta$latitud, marea_alta$SST, 'Alta',  expression(paste("[mg.L"^-1,"]")), 'SST - Alta')
 TSI_SECCHI_AltaGrid<-rasterizar_Variable('TSI_SECCHI', marea_alta$longitud, marea_alta$latitud, marea_alta$TSI_SECCHI, 'Alta',  "(m)", 'TSI_SECCHI - Alta')
 Temperatura_median_AltaGrid<-rasterizar_Variable("Temperatura_median", marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_median, 'Alta',  "(°C)", expression(paste("Q"[2]," Temp - Alta")))
 Salinidad_median_AltaGrid<-rasterizar_Variable('Salinidad_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_median, 'Alta',  "(PSU)", expression(paste("Q"[2]," Sal - Baja")))
 Oxigeno_median_AltaGrid<-rasterizar_Variable('Oxigeno_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_median, 'Alta',  expression(paste("[mg.L"^-1,"]")), expression(paste("Q"[2]," Oxi - Alta")))
 Densidad_median_AltaGrid<-rasterizar_Variable('Densidad_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_median, 'Alta',  expression(paste("(kg.m"^-3,")")), expression(paste("Q"[2]," Den - Alta")))
 Temperatura_IQR_AltaGrid<-rasterizar_Variable('Temperatura_IQR', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_IQR, 'Alta',  "(°C)", "RIC Temp - Alta")
 Salinidad_IQR_AltaGrid<-rasterizar_Variable('Salinidad_IQR', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_IQR, 'Alta',  "(PSU)", "RIC Sal - Alta")
 Oxigeno_IQR_AltaGrid<-rasterizar_Variable('Oxigeno_IQR', marea_alta$longitud, marea_alta$latitud, marea_alta$Oxigeno_IQR, 'Alta',  expression(paste("[mg.L"^-1,"]")), "RIC Oxi - Alta")
 Densidad_IQR_AltaGrid<-rasterizar_Variable('Densidad_IQR', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_IQR, 'Alta',  expression(paste("(kg.m"^-3,")")), "RIC Den - Alta")
 Temperatura_Sup_AltaGrid<-rasterizar_Variable('Temperatura_Sup', marea_alta$longitud, marea_alta$latitud, marea_alta$Temperatura_Sup, 'Alta',  "(°C)", "Temp. Sup - Alta")
 Salinidad_Sup_AltaGrid<-rasterizar_Variable('Salinidad_Sup', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_Sup, 'Alta',  "(PSU)", "Sal. Sup. - Alta")
 Densidad_Sup_AltaGrid<-rasterizar_Variable('Densidad_Sup', marea_alta$longitud, marea_alta$latitud, marea_alta$Densidad_Sup, 'Alta',  expression(paste("(kg.m"^-3,")")), "Den. Sup. - Alta")
 
 
 
 png(filename = "./02_Imagenes/grid_grafica_01.png", width = 14, height = 24, units = "cm", pointsize = 4, bg = "white", res = 300)
 
 grid.arrange(nrow=4, ncol=2, 
              NO2_AltaGrid, NO2_BajaGrid,
              NO3_AltaGrid, NO3_BajaGrid,
              PO4_AltaGrid, PO4_BajaGrid,
              SiO2_AltaGrid, SiO2_BajaGrid)
 dev.off()

 
 
 png(filename = "./02_Imagenes/grid_grafica_02.png", width = 14, height = 24, units = "cm", pointsize = 4, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              pH_AltaGrid, pH_BajaGrid,
              OD_AltaGrid, OD_BajaGrid,
              Transparencia_AltaGrid, Transparencia_BajaGrid,
              SST_AltaGrid, SST_BajaGrid
              )
 dev.off()
 
 png(filename = "./02_Imagenes/grid_grafica_03.png", width = 14, height = 24, units = "cm", pointsize = 4, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              TSI_SECCHI_AltaGrid, TSI_SECCHI_BajaGrid,
              Temperatura_median_AltaGrid, Temperatura_median_BajaGrid,
              Salinidad_median_AltaGrid, Salinidad_median_BajaGrid,
              Oxigeno_median_AltaGrid, Oxigeno_median_BajaGrid)
 dev.off()
 
 png(filename = "./02_Imagenes/grid_grafica_04.png", width = 14, height = 24, units = "cm", pointsize = 4, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              Densidad_median_AltaGrid, Densidad_median_BajaGrid,
              Temperatura_IQR_AltaGrid, Temperatura_IQR_BajaGrid,
              Salinidad_IQR_AltaGrid, Salinidad_IQR_BajaGrid,
              Oxigeno_IQR_AltaGrid, Oxigeno_IQR_BajaGrid)
 dev.off()
 
 png(filename = "./02_Imagenes/grid_grafica_05.png", width = 14, height = 24, units = "cm", pointsize = 4, bg = "white", res = 300)
 grid.arrange(nrow=4, ncol=2, 
              Densidad_IQR_AltaGrid, Densidad_IQR_BajaGrid,
              Temperatura_Sup_AltaGrid, Temperatura_Sup_BajaGrid,
              Salinidad_Sup_AltaGrid, Salinidad_Sup_BajaGrid,
              Densidad_Sup_AltaGrid, Densidad_Sup_BajaGrid)
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




