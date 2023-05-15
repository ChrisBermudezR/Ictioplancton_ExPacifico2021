library(vegan)
library(ggplot2)
library(dplyr)
if(!require(sp))install.packages("sp ")
if(!require(spdep))install.packages("spdep ")
if(!require(oce))install.packages("oce")
if(!require(sp))install.packages("sp ")
if(!require(spdep))install.packages("spdep ")
if(!require(oce))install.packages("oce")
if(!require(rgdal))install.packages("rgdal")
if(!require(gridExtra))install.packages("gridExtra")


source("../Funciones/Analisis_autocorrelacion.R")
source("../Funciones/rasterizar_Autocorrelacion.R")
source("../Funciones/rasterizar_Variable.R")

coordenadas_fuente<-read.table("../Sig_Datos/loglat_Estaciones.csv", header = TRUE, sep = ",")
FitoData<-read.table("./Resultados/Fito_Diversidad_Estaciones.csv", sep=",", header = TRUE)
IctioData<-read.table("./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", header = TRUE)


colnames(FitoData)<-c(
  "Estaciones",  "Transecto",   "Sector",      "Marea",       "Fito_S",           "Fito_Simpson",     "Fito_Shannon",    
  "Fito_Pielou",      "Fito_q0",          "Fito_q1",          "Fito_q2",          "Fito_Densidad",    "Clorofila",   "No.Estacion"
)
colnames(IctioData)<-c(
  "Estaciones",  "Transecto",   "Sector",      "Marea",       "Ictio_S",           "Ictio_Simpson",     "Ictio_Shannon",    
  "Ictio_Pielou",      "Ictio_q0",          "Ictio_q1",          "Ictio_q2",          "Ictio_Densidad"
)

dataTotal<-cbind(FitoData,IctioData[,5:12])


dataTotal_alta<-dataTotal%>% filter(Marea=="Alta")
dataTotal_baja<-dataTotal%>% filter(Marea=="Baja")



marea_altacoor<-cbind(dataTotal_alta, coordenadas_fuente)
marea_bajacoor<-cbind(dataTotal_baja, coordenadas_fuente)

row.names(marea_bajacoor)<-dataTotal_alta$Estaciones
row.names(marea_altacoor)<-dataTotal_baja$Estaciones

coordinates(marea_bajacoor) <- c("longitud", "latitud")
coordinates(marea_altacoor) <- c("longitud", "latitud")

coordinates(coordenadas_fuente) <- c("longitud", "latitud")

costa<-readOGR("../SIG_Datos/costa.shp")
rios<-readOGR("../SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("../SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("../SIG_Datos/areas_protegidas.shp")



Analisis_autocorrelacion("Fito_q0", coordenadas_fuente)
Fito_q0_AltaGrid<-rasterizar_Variable('Fito_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_alta$Fito_q0, 'Alta',  "Exp_q0", "q0 - Alta")
Fito_q0_BajaGrid<-rasterizar_Variable('Fito_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_baja$Fito_q0, 'Baja',  "Exp_q0", "q0 - Baja")

rasterizar_Autocorrelacion('Fito_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, q0_localMoran_Alta_DF$`Var.Ii`, q0_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q0 - Alta")
rasterizar_Autocorrelacion('Fito_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, q0_localMoran_Baja_DF$`Var.Ii`, q0_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q0 - Baja")


png(filename = "./Imagenes/Fito_q0.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Fito_q0_AltaGrid,
             Fito_q0_BajaGrid,
             Fito_q0_Alta_moran_plot,
             Fito_q0_Baja_moran_plot,
             Fito_q0_Alta_moranProba_plot,
             Fito_q0_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


Analisis_autocorrelacion("Fito_q1")
Fito_q1_AltaGrid<-rasterizar_Variable('q1', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_alta$Fito_q1, 'Alta',  "Exp_q1", "q1 - Alta")
Fito_q1_BajaGrid<-rasterizar_Variable('q1', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_baja$Fito_q1, 'Baja',  "Exp_q1", "q1 - Baja")

rasterizar_Autocorrelacion('Fito_q1', marea_bajacoor$longitud, marea_bajacoor$latitud, q1_localMoran_Alta_DF$`Var.Ii`, q1_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q1 - Alta")
rasterizar_Autocorrelacion('Fito_q1', marea_bajacoor$longitud, marea_bajacoor$latitud, q1_localMoran_Baja_DF$`Var.Ii`, q1_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q1 - Baja")


png(filename = "./Imagenes/Fito_q1.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Fito_q1_AltaGrid,
             Fito_q1_BajaGrid,
             Fito_q1_Alta_moran_plot,
             Fito_q1_Baja_moran_plot,
             Fito_q1_Alta_moranProba_plot,
             Fito_q1_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


Analisis_autocorrelacion("Fito_q2")
Fito_q2_AltaGrid<-rasterizar_Variable('q2', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_alta$Fito_q2, 'Alta',  "Exp_q2", "q2 - Alta")
Fito_q2_BajaGrid<-rasterizar_Variable('q2', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_baja$Fito_q2, 'Baja',  "Exp_q2", "q2 - Baja")

rasterizar_Autocorrelacion('Fito_q2', marea_bajacoor$longitud, marea_bajacoor$latitud, q2_localMoran_Alta_DF$`Var.Ii`, q2_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q2 - Alta")
rasterizar_Autocorrelacion('Fito_q2', marea_bajacoor$longitud, marea_bajacoor$latitud, q2_localMoran_Baja_DF$`Var.Ii`, q2_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q2 - Baja")


png(filename = "./Imagenes/Fito_q2.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Fito_q2_AltaGrid,
             Fito_q2_BajaGrid,
             Fito_q2_Alta_moran_plot,
             Fito_q2_Baja_moran_plot,
             Fito_q2_Alta_moranProba_plot,
             Fito_q2_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()



Analisis_autocorrelacion("Densidad")
Fito_Densidad_AltaGrid<-rasterizar_Variable('Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_alta$Densidad, 'Alta',  "Exp_Densidad", "Densidad - Alta")
Fito_Densidad_BajaGrid<-rasterizar_Variable('Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_baja$Densidad, 'Baja',  "Exp_Densidad", "Densidad - Baja")

rasterizar_Autocorrelacion('Fito_Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Densidad_localMoran_Alta_DF$`Var.Ii`, Densidad_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Densidad - Alta")
rasterizar_Autocorrelacion('Fito_Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Densidad_localMoran_Baja_DF$`Var.Ii`, Densidad_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Densidad - Baja")


png(filename = "./Imagenes/Fito_Densidad.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Fito_Densidad_AltaGrid,
             Fito_Densidad_BajaGrid,
             Fito_Densidad_Alta_moran_plot,
             Fito_Densidad_Baja_moran_plot,
             Fito_Densidad_Alta_moranProba_plot,
             Fito_Densidad_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()




Analisis_autocorrelacion("Clorofila")
Fito_Clorofila_AltaGrid<-rasterizar_Variable('Clorofila', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_alta$Clorofila, 'Alta',  "Exp_Clorofila", "Clorofila - Alta")
Fito_Clorofila_BajaGrid<-rasterizar_Variable('Clorofila', marea_bajacoor$longitud, marea_bajacoor$latitud, Fito_marea_baja$Clorofila, 'Baja',  "Exp_Clorofila", "Clorofila - Baja")

rasterizar_Autocorrelacion('Fito_Clorofila', marea_bajacoor$longitud, marea_bajacoor$latitud, Clorofila_localMoran_Alta_DF$`Var.Ii`, Clorofila_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Clorofila - Alta")
rasterizar_Autocorrelacion('Fito_Clorofila', marea_bajacoor$longitud, marea_bajacoor$latitud, Clorofila_localMoran_Baja_DF$`Var.Ii`, Clorofila_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Clorofila - Baja")


png(filename = "./Imagenes/Fito_Clorofila.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Fito_Clorofila_AltaGrid,
             Fito_Clorofila_BajaGrid,
             Fito_Clorofila_Alta_moran_plot,
             Fito_Clorofila_Baja_moran_plot,
             Fito_Clorofila_Alta_moranProba_plot,
             Fito_Clorofila_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()



Ictio_marea_alta<-IctioData%>% filter(Marea=="Alta")
Ictio_marea_baja<-IctioData%>% filter(Marea=="Baja")

marea_altacoor<-Ictio_marea_alta
marea_bajacoor<-Ictio_marea_baja


row.names(marea_bajacoor)<-Ictio_marea_alta$Estacion
row.names(marea_altacoor)<-Ictio_marea_baja$Estacion

coordenadas_fuente

marea_bajacoor<-cbind(marea_bajacoor, coordenadas_fuente)
marea_altacoor<-cbind(marea_altacoor, coordenadas_fuente)

coordinates(marea_bajacoor) <- c("longitud", "latitud")
coordinates(marea_altacoor) <- c("longitud", "latitud")

costa<-readOGR("../SIG_Datos/costa.shp")
rios<-readOGR("../SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("../SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("../SIG_Datos/areas_protegidas.shp")

Analisis_autocorrelacion("q0")
Ictio_q0_AltaGrid<-rasterizar_Variable('q0', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_alta$q0, 'Alta',  "Exp_q0", "q0 - Alta")
Ictio_q0_BajaGrid<-rasterizar_Variable('q0', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_baja$q0, 'Baja',  "Exp_q0", "q0 - Baja")

rasterizar_Autocorrelacion('Ictio_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, q0_localMoran_Alta_DF$`Var.Ii`, q0_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q0 - Alta")
rasterizar_Autocorrelacion('Ictio_q0', marea_bajacoor$longitud, marea_bajacoor$latitud, q0_localMoran_Baja_DF$`Var.Ii`, q0_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q0 - Baja")


png(filename = "./Imagenes/Ictio_q0.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Ictio_q0_AltaGrid,
             Ictio_q0_BajaGrid,
             Ictio_q0_Alta_moran_plot,
             Ictio_q0_Baja_moran_plot,
             Ictio_q0_Alta_moranProba_plot,
             Ictio_q0_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


Analisis_autocorrelacion("q1")
Ictio_q1_AltaGrid<-rasterizar_Variable('q1', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_alta$q1, 'Alta',  "Exp_q1", "q1 - Alta")
Ictio_q1_BajaGrid<-rasterizar_Variable('q1', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_baja$q1, 'Baja',  "Exp_q1", "q1 - Baja")

rasterizar_Autocorrelacion('Ictio_q1', marea_bajacoor$longitud, marea_bajacoor$latitud, q1_localMoran_Alta_DF$`Var.Ii`, q1_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q1 - Alta")
rasterizar_Autocorrelacion('Ictio_q1', marea_bajacoor$longitud, marea_bajacoor$latitud, q1_localMoran_Baja_DF$`Var.Ii`, q1_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q1 - Baja")


png(filename = "./Imagenes/Ictio_q1.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Ictio_q1_AltaGrid,
             Ictio_q1_BajaGrid,
             Ictio_q1_Alta_moran_plot,
             Ictio_q1_Baja_moran_plot,
             Ictio_q1_Alta_moranProba_plot,
             Ictio_q1_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


Analisis_autocorrelacion("q2")
Ictio_q2_AltaGrid<-rasterizar_Variable('q2', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_alta$q2, 'Alta',  "Exp_q2", "q2 - Alta")
Ictio_q2_BajaGrid<-rasterizar_Variable('q2', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_baja$q2, 'Baja',  "Exp_q2", "q2 - Baja")

rasterizar_Autocorrelacion('Ictio_q2', marea_bajacoor$longitud, marea_bajacoor$latitud, q2_localMoran_Alta_DF$`Var.Ii`, q2_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "q2 - Alta")
rasterizar_Autocorrelacion('Ictio_q2', marea_bajacoor$longitud, marea_bajacoor$latitud, q2_localMoran_Baja_DF$`Var.Ii`, q2_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "q2 - Baja")


png(filename = "./Imagenes/Ictio_q2.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Ictio_q2_AltaGrid,
             Ictio_q2_BajaGrid,
             Ictio_q2_Alta_moran_plot,
             Ictio_q2_Baja_moran_plot,
             Ictio_q2_Alta_moranProba_plot,
             Ictio_q2_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()



Analisis_autocorrelacion("Densidad")
Ictio_Densidad_AltaGrid<-rasterizar_Variable('Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_alta$Densidad, 'Alta',  "Exp_Densidad", "Densidad - Alta")
Ictio_Densidad_BajaGrid<-rasterizar_Variable('Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Ictio_marea_baja$Densidad, 'Baja',  "Exp_Densidad", "Densidad - Baja")

rasterizar_Autocorrelacion('Ictio_Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Densidad_localMoran_Alta_DF$`Var.Ii`, Densidad_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Densidad - Alta")
rasterizar_Autocorrelacion('Ictio_Densidad', marea_bajacoor$longitud, marea_bajacoor$latitud, Densidad_localMoran_Baja_DF$`Var.Ii`, Densidad_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Densidad - Baja")


png(filename = "./Imagenes/Ictio_Densidad.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Ictio_Densidad_AltaGrid,
             Ictio_Densidad_BajaGrid,
             Ictio_Densidad_Alta_moran_plot,
             Ictio_Densidad_Baja_moran_plot,
             Ictio_Densidad_Alta_moranProba_plot,
             Ictio_Densidad_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()




