if(!require(sp))install.packages("sp ")
if(!require(spdep))install.packages("spdep ")
if(!require(oce))install.packages("oce")


Datos_Totales_Limpios<-read.table( "./01_Datos_Quimicos/Datos_Totales_CCCP.csv", header =  TRUE, sep = ",")
marea_alta<-Datos_Totales_Limpios%>% filter(Marea=="Alta")
marea_baja<-Datos_Totales_Limpios%>% filter(Marea=="Baja")

################################################


marea_alta<-Datos_Totales_Limpios%>% filter(Marea=="Alta")
marea_baja<-Datos_Totales_Limpios%>% filter(Marea=="Baja")

costa<-readOGR("../SIG_Datos/costa.shp")
rios<-readOGR("../SIG_Datos/rios_wgs84.shp")
estaciones<-readOGR("../SIG_Datos/estaciones.shp")
areas_protegidas<-readOGR("../SIG_Datos/areas_protegidas.shp")



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
Salinidad_median_AltaGrid<-rasterizar_Variable('Salinidad_median', marea_alta$longitud, marea_alta$latitud, marea_alta$Salinidad_median, 'Alta',  "(PSU)", expression(paste("Q"[2]," Sal - Alta")))
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

#############################

row.names(marea_alta)<-marea_alta$Estacion
row.names(marea_baja)<-marea_baja$Estacion

marea_bajacoor<-marea_baja
marea_altacoor<-marea_alta

coordinates(marea_bajacoor) <- c("longitud", "latitud")
coordinates(marea_altacoor) <- c("longitud", "latitud")










# Create spatial weights matrix using k nearest neighbors
nombre_variable<-"NO3"


Analisis_autocorrelacion("NO2")
Analisis_autocorrelacion("NO3")
Analisis_autocorrelacion("PO4")
Analisis_autocorrelacion("SiO2")
Analisis_autocorrelacion("pH")
Analisis_autocorrelacion("OD")
Analisis_autocorrelacion("Transparencia")
Analisis_autocorrelacion("SST")
Analisis_autocorrelacion("TSI_SECCHI")
Analisis_autocorrelacion("Temperatura_median")
Analisis_autocorrelacion("Salinidad_median")
Analisis_autocorrelacion("Oxigeno_median")
Analisis_autocorrelacion("Densidad_median")
Analisis_autocorrelacion("Temperatura_IQR")
Analisis_autocorrelacion("Salinidad_IQR")
Analisis_autocorrelacion("Oxigeno_IQR")
Analisis_autocorrelacion("Densidad_IQR")
Analisis_autocorrelacion("Temperatura_Sup")
Analisis_autocorrelacion("Salinidad_Sup")
Analisis_autocorrelacion("Densidad_Sup")


source("../Funciones/rasterizar_Autocorrelacion.R")
rasterizar_Autocorrelacion('NO2', marea_baja$longitud, marea_baja$latitud, NO2_localMoran_Alta_DF$`Var.Ii`, NO2_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Nitritos - Alta")
rasterizar_Autocorrelacion('NO2', marea_baja$longitud, marea_baja$latitud, NO2_localMoran_Baja_DF$`Var.Ii`, NO2_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Nitritos - Baja")

png(filename = "./02_Imagenes/NO2.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(NO2_AltaGrid,
             NO2_BajaGrid,
             NO2_Alta_moran_plot,
             NO2_Baja_moran_plot,
             NO2_Alta_moranProba_plot,
             NO2_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()




rasterizar_Autocorrelacion('NO3', marea_baja$longitud, marea_baja$latitud, NO3_localMoran_Alta_DF$`Var.Ii`, NO3_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Nitratos - Alta")
rasterizar_Autocorrelacion('NO3', marea_baja$longitud, marea_baja$latitud, NO3_localMoran_Baja_DF$`Var.Ii`, NO3_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Nitratos - Baja")

png(filename = "./02_Imagenes/NO3.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(NO3_AltaGrid,
             NO3_BajaGrid,
             NO3_Alta_moran_plot,
             NO3_Baja_moran_plot,
             NO3_Alta_moranProba_plot,
             NO3_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('PO4', marea_baja$longitud, marea_baja$latitud, PO4_localMoran_Alta_DF$`Var.Ii`, PO4_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Fosfatos - Alta")
rasterizar_Autocorrelacion('PO4', marea_baja$longitud, marea_baja$latitud, PO4_localMoran_Baja_DF$`Var.Ii`, PO4_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Fosfatos - Baja")

png(filename = "./02_Imagenes/PO4.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(PO4_AltaGrid,
             PO4_BajaGrid,
             PO4_Alta_moran_plot,
             PO4_Baja_moran_plot,
             PO4_Alta_moranProba_plot,
             PO4_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('SiO2', marea_baja$longitud, marea_baja$latitud, SiO2_localMoran_Alta_DF$`Var.Ii`, SiO2_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Silicatos - Alta")
rasterizar_Autocorrelacion('SiO2', marea_baja$longitud, marea_baja$latitud, SiO2_localMoran_Baja_DF$`Var.Ii`, SiO2_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Silicatos - Baja")

png(filename = "./02_Imagenes/SiO2.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(SiO2_AltaGrid,
             SiO2_BajaGrid,
             SiO2_Alta_moran_plot,
             SiO2_Baja_moran_plot,
             SiO2_Alta_moranProba_plot,
             SiO2_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()

rasterizar_Autocorrelacion('pH', marea_baja$longitud, marea_baja$latitud, pH_localMoran_Alta_DF$`Var.Ii`, pH_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "pH - Alta")
rasterizar_Autocorrelacion('pH', marea_baja$longitud, marea_baja$latitud, pH_localMoran_Baja_DF$`Var.Ii`, pH_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "pH - Baja")

png(filename = "./02_Imagenes/pH.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(pH_AltaGrid,
             pH_BajaGrid,
             pH_Alta_moran_plot,
             pH_Baja_moran_plot,
             pH_Alta_moranProba_plot,
             pH_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('OD', marea_baja$longitud, marea_baja$latitud, OD_localMoran_Alta_DF$`Var.Ii`, OD_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Oxí. Dis. Sup - Alta")
rasterizar_Autocorrelacion('OD', marea_baja$longitud, marea_baja$latitud, OD_localMoran_Baja_DF$`Var.Ii`, OD_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Oxí. Dis. Sup - Baja")

png(filename = "./02_Imagenes/OD.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(OD_AltaGrid,
             OD_BajaGrid,
             OD_Alta_moran_plot,
             OD_Baja_moran_plot,
             OD_Alta_moranProba_plot,
             OD_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Transparencia', marea_baja$longitud, marea_baja$latitud, Transparencia_localMoran_Alta_DF$`Var.Ii`, Transparencia_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Transparencia - Alta")
rasterizar_Autocorrelacion('Transparencia', marea_baja$longitud, marea_baja$latitud, Transparencia_localMoran_Baja_DF$`Var.Ii`, Transparencia_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Transparencia - Baja")

png(filename = "./02_Imagenes/Transparencia.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Transparencia_AltaGrid,
             Transparencia_BajaGrid,
             Transparencia_Alta_moran_plot,
             Transparencia_Baja_moran_plot,
             Transparencia_Alta_moranProba_plot,
             Transparencia_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('SST', marea_baja$longitud, marea_baja$latitud, SST_localMoran_Alta_DF$`Var.Ii`, SST_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "SST - Alta")
rasterizar_Autocorrelacion('SST', marea_baja$longitud, marea_baja$latitud, SST_localMoran_Baja_DF$`Var.Ii`, SST_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "SST - Baja")

png(filename = "./02_Imagenes/SST.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(SST_AltaGrid,
             SST_BajaGrid,
             SST_Alta_moran_plot,
             SST_Baja_moran_plot,
             SST_Alta_moranProba_plot,
             SST_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('TSI_SECCHI', marea_baja$longitud, marea_baja$latitud, TSI_SECCHI_localMoran_Alta_DF$`Var.Ii`, TSI_SECCHI_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "TSI - Alta")
rasterizar_Autocorrelacion('TSI_SECCHI', marea_baja$longitud, marea_baja$latitud, TSI_SECCHI_localMoran_Baja_DF$`Var.Ii`, TSI_SECCHI_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "TSI - Baja")

png(filename = "./02_Imagenes/TSI_SECCHI.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(TSI_SECCHI_AltaGrid,
             TSI_SECCHI_BajaGrid,
             TSI_SECCHI_Alta_moran_plot,
             TSI_SECCHI_Baja_moran_plot,
             TSI_SECCHI_Alta_moranProba_plot,
             TSI_SECCHI_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Temperatura_median', marea_baja$longitud, marea_baja$latitud, Temperatura_median_localMoran_Alta_DF$`Var.Ii`, Temperatura_median_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", expression(paste("Q"[2]," Temp.- Alta")))
rasterizar_Autocorrelacion('Temperatura_median', marea_baja$longitud, marea_baja$latitud, Temperatura_median_localMoran_Baja_DF$`Var.Ii`, Temperatura_median_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", expression(paste("Q"[2]," Temp.- Baja")))

png(filename = "./02_Imagenes/Temperatura_median.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Temperatura_median_AltaGrid,
             Temperatura_median_BajaGrid,
             Temperatura_median_Alta_moran_plot,
             Temperatura_median_Baja_moran_plot,
             Temperatura_median_Alta_moranProba_plot,
             Temperatura_median_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Salinidad_median', marea_baja$longitud, marea_baja$latitud, Salinidad_median_localMoran_Alta_DF$`Var.Ii`, Salinidad_median_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", expression(paste("Q"[2]," Sal. - Alta")))
rasterizar_Autocorrelacion('Salinidad_median', marea_baja$longitud, marea_baja$latitud, Salinidad_median_localMoran_Baja_DF$`Var.Ii`, Salinidad_median_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", expression(paste("Q"[2]," Sal. - Baja")))

png(filename = "./02_Imagenes/Salinidad_median.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Salinidad_median_AltaGrid,
             Salinidad_median_BajaGrid,
             Salinidad_median_Alta_moran_plot,
             Salinidad_median_Baja_moran_plot,
             Salinidad_median_Alta_moranProba_plot,
             Salinidad_median_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Oxigeno_median', marea_baja$longitud, marea_baja$latitud, Oxigeno_median_localMoran_Alta_DF$`Var.Ii`, Oxigeno_median_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", expression(paste("Q"[2]," Oxi. - Alta")))
rasterizar_Autocorrelacion('Oxigeno_median', marea_baja$longitud, marea_baja$latitud, Oxigeno_median_localMoran_Baja_DF$`Var.Ii`, Oxigeno_median_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", expression(paste("Q"[2]," Oxi. - Baja")))

png(filename = "./02_Imagenes/Oxigeno_median.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Oxigeno_median_AltaGrid,
             Oxigeno_median_BajaGrid,
             Oxigeno_median_Alta_moran_plot,
             Oxigeno_median_Baja_moran_plot,
             Oxigeno_median_Alta_moranProba_plot,
             Oxigeno_median_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Densidad_median', marea_baja$longitud, marea_baja$latitud, Densidad_median_localMoran_Alta_DF$`Var.Ii`, Densidad_median_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", expression(paste("Q"[2]," Den. - Alta")))
rasterizar_Autocorrelacion('Densidad_median', marea_baja$longitud, marea_baja$latitud, Densidad_median_localMoran_Baja_DF$`Var.Ii`, Densidad_median_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", expression(paste("Q"[2]," Den. - Baja")))

png(filename = "./02_Imagenes/Densidad_median.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Densidad_median_AltaGrid,
             Densidad_median_BajaGrid,
             Densidad_median_Alta_moran_plot,
             Densidad_median_Baja_moran_plot,
             Densidad_median_Alta_moranProba_plot,
             Densidad_median_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Temperatura_IQR', marea_baja$longitud, marea_baja$latitud, Temperatura_IQR_localMoran_Alta_DF$`Var.Ii`, Temperatura_IQR_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "RIC Temp. - Alta")
rasterizar_Autocorrelacion('Temperatura_IQR', marea_baja$longitud, marea_baja$latitud, Temperatura_IQR_localMoran_Baja_DF$`Var.Ii`, Temperatura_IQR_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "RIC Temp. - Baja")

png(filename = "./02_Imagenes/Temperatura_IQR.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Temperatura_IQR_AltaGrid,
             Temperatura_IQR_BajaGrid,
             Temperatura_IQR_Alta_moran_plot,
             Temperatura_IQR_Baja_moran_plot,
             Temperatura_IQR_Alta_moranProba_plot,
             Temperatura_IQR_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()

rasterizar_Autocorrelacion('Salinidad_IQR', marea_baja$longitud, marea_baja$latitud, Salinidad_IQR_localMoran_Alta_DF$`Var.Ii`, Salinidad_IQR_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "RIC Sal. - Alta")
rasterizar_Autocorrelacion('Salinidad_IQR', marea_baja$longitud, marea_baja$latitud, Salinidad_IQR_localMoran_Baja_DF$`Var.Ii`, Salinidad_IQR_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "RIC Sal. - Baja")

png(filename = "./02_Imagenes/Salinidad_IQR.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Salinidad_IQR_AltaGrid,
             Salinidad_IQR_BajaGrid,
             Salinidad_IQR_Alta_moran_plot,
             Salinidad_IQR_Baja_moran_plot,
             Salinidad_IQR_Alta_moranProba_plot,
             Salinidad_IQR_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Oxigeno_IQR', marea_baja$longitud, marea_baja$latitud, Oxigeno_IQR_localMoran_Alta_DF$`Var.Ii`, Oxigeno_IQR_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "RIC Oxi. - Alta")
rasterizar_Autocorrelacion('Oxigeno_IQR', marea_baja$longitud, marea_baja$latitud, Oxigeno_IQR_localMoran_Baja_DF$`Var.Ii`, Oxigeno_IQR_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "RIC Oxi. - Baja")

png(filename = "./02_Imagenes/Oxigeno_IQR.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Oxigeno_IQR_AltaGrid,
             Oxigeno_IQR_BajaGrid,
             Oxigeno_IQR_Alta_moran_plot,
             Oxigeno_IQR_Baja_moran_plot,
             Oxigeno_IQR_Alta_moranProba_plot,
             Oxigeno_IQR_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Densidad_IQR', marea_baja$longitud, marea_baja$latitud, Densidad_IQR_localMoran_Alta_DF$`Var.Ii`, Densidad_IQR_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "RIC Den. - Alta")
rasterizar_Autocorrelacion('Densidad_IQR', marea_baja$longitud, marea_baja$latitud, Densidad_IQR_localMoran_Baja_DF$`Var.Ii`, Densidad_IQR_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "RIC Den. - Baja")

png(filename = "./02_Imagenes/Densidad_IQR.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Densidad_IQR_AltaGrid,
             Densidad_IQR_BajaGrid,
             Densidad_IQR_Alta_moran_plot,
             Densidad_IQR_Baja_moran_plot,
             Densidad_IQR_Alta_moranProba_plot,
             Densidad_IQR_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Temperatura_Sup', marea_baja$longitud, marea_baja$latitud, Temperatura_Sup_localMoran_Alta_DF$`Var.Ii`, Temperatura_Sup_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Temp. sup. - Alta")
rasterizar_Autocorrelacion('Temperatura_Sup', marea_baja$longitud, marea_baja$latitud, Temperatura_Sup_localMoran_Baja_DF$`Var.Ii`, Temperatura_Sup_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Temp. sup. - Baja")

png(filename = "./02_Imagenes/Temperatura_Sup.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Temperatura_Sup_AltaGrid,
             Temperatura_Sup_BajaGrid,
             Temperatura_Sup_Alta_moran_plot,
             Temperatura_Sup_Baja_moran_plot,
             Temperatura_Sup_Alta_moranProba_plot,
             Temperatura_Sup_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Salinidad_Sup', marea_baja$longitud, marea_baja$latitud, Salinidad_Sup_localMoran_Alta_DF$`Var.Ii`, Salinidad_Sup_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Sal. sup. - Alta")
rasterizar_Autocorrelacion('Salinidad_Sup', marea_baja$longitud, marea_baja$latitud, Salinidad_Sup_localMoran_Baja_DF$`Var.Ii`, Salinidad_Sup_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Sal. sup. - Baja")

png(filename = "./02_Imagenes/Salinidad_Sup.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Salinidad_Sup_AltaGrid,
             Salinidad_Sup_BajaGrid,
             Salinidad_Sup_Alta_moran_plot,
             Salinidad_Sup_Baja_moran_plot,
             Salinidad_Sup_Alta_moranProba_plot,
             Salinidad_Sup_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()


rasterizar_Autocorrelacion('Densidad_Sup', marea_baja$longitud, marea_baja$latitud, Densidad_Sup_localMoran_Alta_DF$`Var.Ii`, Densidad_Sup_localMoran_Alta_DF$`Pr(z != E(Ii))`,'Alta',  "Var.", "Sig.", "Dens. sup. - Alta")
rasterizar_Autocorrelacion('Densidad_Sup', marea_baja$longitud, marea_baja$latitud, Densidad_Sup_localMoran_Baja_DF$`Var.Ii`, Densidad_Sup_localMoran_Baja_DF$`Pr(z != E(Ii))`,'Baja',  "Var.", "Sig.", "Dens. sup. - Baja")

png(filename = "./02_Imagenes/Densidad_Sup.png", width = 20, height = 30, units = "cm", pointsize =10, bg = "white", res = 300)
grid.arrange(Densidad_Sup_AltaGrid,
             Densidad_Sup_BajaGrid,
             Densidad_Sup_Alta_moran_plot,
             Densidad_Sup_Baja_moran_plot,
             Densidad_Sup_Alta_moranProba_plot,
             Densidad_Sup_Baja_moranProba_plot,
             nrow=3, 
             ncol=2)
dev.off()
