#Titulo: Visualización y Análisis descriptivo - PNN
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos físicos obtenidos de la sonda CastAWAY General Oceanics 19v PLUS.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################


library(tidyverse)
library(gridExtra)


Datos_CTDO_List<-list.files("./ctd_BOCAS_DE SANQUIANGA_2021", pattern =".csv$", full.names = TRUE)
Datos_CTDO_names<-list.files("./ctd_BOCAS_DE SANQUIANGA_2021", pattern =".csv$", full.names = FALSE)


for(Archivos in 1:length(Datos_CTDO_names)) assign(Datos_CTDO_names[Archivos], read.table(Datos_CTDO_List[Archivos], header = TRUE, sep = ',',quote ="", fill = TRUE ))

Datos_CTDO_PNN<-rbind(CC1404006_20210429_152542.csv, CC1404006_20210429_161317.csv, CC1404006_20210429_210545.csv, CC1404006_20210429_215804.csv, 
                      CC1404006_20210430_113001.csv, CC1404006_20210430_120427.csv, CC1404006_20210430_165824.csv, CC1404006_20210430_174538.csv, 
                      CC1404006_20210501_114507.csv, CC1404006_20210501_120432.csv, CC1404006_20210501_123136.csv, CC1404006_20210501_175834.csv, 
                      CC1404006_20210501_182239.csv, CC1404006_20210501_185340.csv, CC1404006_20210502_123624.csv, CC1404006_20210502_125742.csv, 
                      CC1404006_20210502_132230.csv, CC1404006_20210502_185059.csv, CC1404006_20210502_191939.csv, CC1404006_20210502_194349.csv, 
                      CC1404006_20210503_132454.csv, CC1404006_20210503_134226.csv, CC1404006_20210503_140418.csv, CC1404006_20210503_142615.csv, 
                      CC1404006_20210503_192013.csv, CC1404006_20210503_194704.csv, CC1404006_20210503_201354.csv, CC1404006_20210503_204023.csv, 
                      CC1404006_20210504_143258.csv, CC1404006_20210504_145158.csv, CC1404006_20210504_152900.csv, CC1404006_20210504_155646.csv, 
                      CC1404006_20210504_203322.csv, CC1404006_20210504_210020.csv, CC1404006_20210504_213110.csv, CC1404006_20210504_215433.csv )
colnames(Datos_CTDO_PNN)<-c("Presion",             "Profundidad",                "Temperatura",         
                             "Conductividad" ,        "Conductancia", "Salinidad",            
                             "Vel_Sonido",       "Densidad",              "Latitud",             
                             "Longitud",             "Fecha",                "Hora",                
                             "Estacion", "Marea")


Datos_CTDO_PNN$Separar<-Datos_CTDO_PNN$Estacion
Datos_CTDO_PNN<-separate(Datos_CTDO_PNN, Separar, c("Boca", "No.Estacion"), sep = "0" )
as.factor(Datos_CTDO_PNN$Boca)->Datos_CTDO_PNN$Boca
as.factor(Datos_CTDO_PNN$No.Estacion)->Datos_CTDO_PNN$No.Estacion




Datos_CTDO_PNN$Boca <- recode_factor(Datos_CTDO_PNN$Boca, 
                                     A = "Amarales", 
                                     S = "Sanquianga", 
                                G = "Guamales")
write.table(Datos_CTDO_PNN, "Datos_CTDO_PNN.csv", col.names = TRUE, sep=",")


#####BoxPlot


Temperatura_Total_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Boca, y=Temperatura)) + 
  geom_boxplot()+ 
  labs( y = "Temperatura [°C]", x = "Boca de los ríos")+
  theme_classic()+geom_jitter(width=0.1,alpha=0.2) 
Salnidad_Total_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Boca, y=Salinidad)) + 
  geom_boxplot()+ 
  labs( y = "Salinidad [PSU]", x = "Boca de los ríos")+
  theme_classic()+geom_jitter(width=0.1,alpha=0.2) 
Densidad_Total_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Boca, y=Densidad)) + 
  geom_boxplot()+  
  labs( y = "Densidad [kg/m3]", x = "Boca de los ríos")+
  theme_classic()+geom_jitter(width=0.1,alpha=0.2) 

tiff(filename = "01_Datos_Totales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2, Temperatura_Total, 
             Salnidad_Total,Densidad_Total, 
             top="Datos totales")
dev.off()

Temperatura_Hist_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Temperatura)) + 
  geom_histogram(aes(group=Marea))+  
    labs(title = "Histograma de la Temperatura [°C]",
       subtitle = "(Distribuido por las bocas de los ríos entre las mareas)",
       y = "Frecuencia", x = "[°C]")+
  facet_grid(Marea~Boca)
Salinidad_Hist_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Salinidad)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma de la Salinidad [PSU]",
       subtitle = "(Distribuido por las bocas de los ríos entre las mareas)",
       y = "Frecuencia", x = "[PSU]")+
  facet_grid(Marea~Boca)
Densidad_Hist_PNN<-ggplot(Datos_CTDO_PNN, aes(x=Densidad)) + 
  geom_histogram(aes(group=Marea))+  
  labs(title = "Histograma de la Densidad [kg/m3]",
       subtitle = "(Distribuido por las bocas de los ríos entre las mareas)",
       y = "Frecuencia", x = " [kg/m3]")+
  facet_grid(Marea~Boca)

tiff(filename = "02_Histogramas_PNN.tif", width = 40, height = 30, units = "cm", pointsize = 30, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_Hist_PNN, Salinidad_Hist_PNN, 
             Densidad_Hist_PNN)
dev.off()

Temperatura_boxplot_PNN<-ggplot(Datos_CTDO_PNN) + 
  geom_boxplot(aes(x=No.Estacion, y=Temperatura))+ 
  theme_bw()+
  labs(title = "Boxplot de la Temperatura [°C]",
       y = "Temperatura [°C]", x = "Estaciones")+
  facet_grid(Marea~Boca)

Salinidad_boxplot_PNN<-ggplot(Datos_CTDO_PNN) + 
  geom_boxplot(aes(x=No.Estacion, y=Salinidad))+ 
  theme_bw()+
  labs(title = "Boxplot de la Salinidad [PSU]",
       y = "Salinidad [PSU]", x = "Estaciones")+
    facet_grid(Marea~Boca)

Densidad_boxplot_PNN<-ggplot(Datos_CTDO_PNN) + 
  geom_boxplot(aes(x=No.Estacion, y=Densidad))+ 
  theme_bw()+
  labs(title = "Boxplot de la Densidad [Kg/m3]",
       y = "Densidad [Kg/m3]", x = "Estaciones")+
  facet_grid(Marea~Boca)

tiff(filename = "03_Boxplot_PNN.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300)
grid.arrange(nrow=2, ncol=2,Temperatura_boxplot_PNN, Salinidad_boxplot_PNN, 
             Densidad_boxplot_PNN)
dev.off()



#####Función para filtrado de datos

Filtrado<-function(Set_Datos, Value1, Value2){
filter(Set_Datos, Estacion == Value1 & Marea == Value2)
  }


A06A_PNN<-Filtrado(Datos_CTDO_PNN,"A06","Alta")
A05A_PNN<-Filtrado(Datos_CTDO_PNN,"A05","Alta")
A04A_PNN<-Filtrado(Datos_CTDO_PNN,"A04","Alta")
A03A_PNN<-Filtrado(Datos_CTDO_PNN,"A03","Alta")
A02A_PNN<-Filtrado(Datos_CTDO_PNN,"A02","Alta")
A01A_PNN<-Filtrado(Datos_CTDO_PNN,"A01","Alta")

A06B_PNN<-Filtrado(Datos_CTDO_PNN,"A06","Baja")
A05B_PNN<-Filtrado(Datos_CTDO_PNN,"A05","Baja")
A04B_PNN<-Filtrado(Datos_CTDO_PNN,"A04","Baja")
A03B_PNN<-Filtrado(Datos_CTDO_PNN,"A03","Baja")
A02B_PNN<-Filtrado(Datos_CTDO_PNN,"A02","Baja")
A01B_PNN<-Filtrado(Datos_CTDO_PNN,"A01","Baja")


S06A_PNN<-Filtrado(Datos_CTDO_PNN,"S06","Alta")
S05A_PNN<-Filtrado(Datos_CTDO_PNN,"S05","Alta")
S04A_PNN<-Filtrado(Datos_CTDO_PNN,"S04","Alta")
S03A_PNN<-Filtrado(Datos_CTDO_PNN,"S03","Alta")
S02A_PNN<-Filtrado(Datos_CTDO_PNN,"S02","Alta")
S01A_PNN<-Filtrado(Datos_CTDO_PNN,"S01","Alta")

S06B_PNN<-Filtrado(Datos_CTDO_PNN,"S06","Baja")
S05B_PNN<-Filtrado(Datos_CTDO_PNN,"S05","Baja")
S04B_PNN<-Filtrado(Datos_CTDO_PNN,"S04","Baja")
S03B_PNN<-Filtrado(Datos_CTDO_PNN,"S03","Baja")
S02B_PNN<-Filtrado(Datos_CTDO_PNN,"S02","Baja")
S01B_PNN<-Filtrado(Datos_CTDO_PNN,"S01","Baja")

G06A_PNN<-Filtrado(Datos_CTDO_PNN,"G06","Alta")
G05A_PNN<-Filtrado(Datos_CTDO_PNN,"G05","Alta")
G04A_PNN<-Filtrado(Datos_CTDO_PNN,"G04","Alta")
G03A_PNN<-Filtrado(Datos_CTDO_PNN,"G03","Alta")
G02A_PNN<-Filtrado(Datos_CTDO_PNN,"G02","Alta")
G01A_PNN<-Filtrado(Datos_CTDO_PNN,"G01","Alta")

G06B_PNN<-Filtrado(Datos_CTDO_PNN,"G06","Baja")
G05B_PNN<-Filtrado(Datos_CTDO_PNN,"G05","Baja")
G04B_PNN<-Filtrado(Datos_CTDO_PNN,"G04","Baja")
G03B_PNN<-Filtrado(Datos_CTDO_PNN,"G03","Baja")
G02B_PNN<-Filtrado(Datos_CTDO_PNN,"G02","Baja")
G01B_PNN<-Filtrado(Datos_CTDO_PNN,"G01","Baja")


Temp_A06A_PNN<-Estandar_15(A06A_PNN, "A06 - Marea Alta", A06A_PNN$Temperatura, A06A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A06B_PNN<-Estandar_15(A06B_PNN, "A06 - Marea Baja", A06B_PNN$Temperatura, A06B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05A_PNN<-Estandar_15(A05A_PNN, "A05 - Marea Alta", A05A_PNN$Temperatura, A05A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05B_PNN<-Estandar_15(A05B_PNN, "A05 - Marea Baja", A05B_PNN$Temperatura, A05B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04A_PNN<-Estandar_15(A04A_PNN, "A04 - Marea Alta", A04A_PNN$Temperatura, A04A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04B_PNN<-Estandar_15(A04B_PNN, "A04 - Marea Baja", A04B_PNN$Temperatura, A04B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03A_PNN<-Estandar_40(A03A_PNN, "A03 - Marea Alta", A03A_PNN$Temperatura, A03A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03B_PNN<-Estandar_40(A03B_PNN, "A03 - Marea Baja", A03B_PNN$Temperatura, A03B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02A_PNN<-Estandar_75(A02A_PNN, "A02 - Marea Alta", A02A_PNN$Temperatura, A02A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02B_PNN<-Estandar_75(A02B_PNN, "A02 - Marea Baja", A02B_PNN$Temperatura, A02B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01A_PNN<-Estandar_75(A01A_PNN, "A01 - Marea Alta", A01A_PNN$Temperatura, A01A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01B_PNN<-Estandar_75(A01B_PNN, "A01 - Marea Baja", A01B_PNN$Temperatura, A01B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_A01A_PNN, Temp_A01B_PNN, Temp_A02A_PNN,Temp_A02B_PNN, Temp_A03A_PNN, Temp_A03B_PNN, Temp_A04A_PNN, Temp_A04B_PNN, Temp_A05A_PNN,Temp_A05B_PNN, Temp_A06A_PNN, Temp_A06B_PNN, top="Boca Amarales")
dev.off()


Sal_A06A_PNN<-Estandar_15(A06A_PNN, "A06 - Marea Alta", A06A_PNN$Salinidad, A06A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A06B_PNN<-Estandar_15(A06B_PNN, "A06 - Marea Baja", A06B_PNN$Salinidad, A06B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05A_PNN<-Estandar_15(A05A_PNN, "A05 - Marea Alta", A05A_PNN$Salinidad, A05A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05B_PNN<-Estandar_15(A05B_PNN, "A05 - Marea Baja", A05B_PNN$Salinidad, A05B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04A_PNN<-Estandar_15(A04A_PNN, "A04 - Marea Alta", A04A_PNN$Salinidad, A04A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04B_PNN<-Estandar_15(A04B_PNN, "A04 - Marea Baja", A04B_PNN$Salinidad, A04B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03A_PNN<-Estandar_40(A03A_PNN, "A03 - Marea Alta", A03A_PNN$Salinidad, A03A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03B_PNN<-Estandar_40(A03B_PNN, "A03 - Marea Baja", A03B_PNN$Salinidad, A03B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02A_PNN<-Estandar_75(A02A_PNN, "A02 - Marea Alta", A02A_PNN$Salinidad, A02A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02B_PNN<-Estandar_75(A02B_PNN, "A02 - Marea Baja", A02B_PNN$Salinidad, A02B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01A_PNN<-Estandar_75(A01A_PNN, "A01 - Marea Alta", A01A_PNN$Salinidad, A01A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01B_PNN<-Estandar_75(A01B_PNN, "A01 - Marea Baja", A01B_PNN$Salinidad, A01B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "Salinidad_Boca_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_A01A_PNN, Sal_A01B_PNN, Sal_A02A_PNN,Sal_A02B_PNN, Sal_A03A_PNN, Sal_A03B_PNN, Sal_A04A_PNN, Sal_A04B_PNN, Sal_A05A_PNN,Sal_A05B_PNN, Sal_A06A_PNN, Sal_A06B_PNN, top="Boca Amarales")
dev.off()

Den_A06A_PNN<-Estandar_15(A06A_PNN, "A06 - Marea Alta", A06A_PNN$Densidad, A06A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A06B_PNN<-Estandar_15(A06B_PNN, "A06 - Marea Baja", A06B_PNN$Densidad, A06B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05A_PNN<-Estandar_15(A05A_PNN, "A05 - Marea Alta", A05A_PNN$Densidad, A05A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05B_PNN<-Estandar_15(A05B_PNN, "A05 - Marea Baja", A05B_PNN$Densidad, A05B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04A_PNN<-Estandar_15(A04A_PNN, "A04 - Marea Alta", A04A_PNN$Densidad, A04A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04B_PNN<-Estandar_15(A04B_PNN, "A04 - Marea Baja", A04B_PNN$Densidad, A04B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03A_PNN<-Estandar_40(A03A_PNN, "A03 - Marea Alta", A03A_PNN$Densidad, A03A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03B_PNN<-Estandar_40(A03B_PNN, "A03 - Marea Baja", A03B_PNN$Densidad, A03B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02A_PNN<-Estandar_75(A02A_PNN, "A02 - Marea Alta", A02A_PNN$Densidad, A02A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02B_PNN<-Estandar_75(A02B_PNN, "A02 - Marea Baja", A02B_PNN$Densidad, A02B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01A_PNN<-Estandar_75(A01A_PNN, "A01 - Marea Alta", A01A_PNN$Densidad, A01A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01B_PNN<-Estandar_75(A01B_PNN, "A01 - Marea Baja", A01B_PNN$Densidad, A01B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "Densidad_Boca_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_A01A_PNN, Den_A01B_PNN, Den_A02A_PNN,Den_A02B_PNN, Den_A03A_PNN, Den_A03B_PNN, Den_A04A_PNN, Den_A04B_PNN, Den_A05A_PNN,Den_A05B_PNN, Den_A06A_PNN, Den_A06B_PNN, top="Boca Amarales")
dev.off()

Oxi_A06A_PNN<-Estandar_15(A06A_PNN, "A06 - Marea Alta", A06A_PNN$Oxigeno, A06A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A06B_PNN<-Estandar_15(A06B_PNN, "A06 - Marea Baja", A06B_PNN$Oxigeno, A06B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05A_PNN<-Estandar_15(A05A_PNN, "A05 - Marea Alta", A05A_PNN$Oxigeno, A05A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05B_PNN<-Estandar_15(A05B_PNN, "A05 - Marea Baja", A05B_PNN$Oxigeno, A05B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04A_PNN<-Estandar_15(A04A_PNN, "A04 - Marea Alta", A04A_PNN$Oxigeno, A04A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04B_PNN<-Estandar_15(A04B_PNN, "A04 - Marea Baja", A04B_PNN$Oxigeno, A04B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03A_PNN<-Estandar_40(A03A_PNN, "A03 - Marea Alta", A03A_PNN$Oxigeno, A03A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03B_PNN<-Estandar_40(A03B_PNN, "A03 - Marea Baja", A03B_PNN$Oxigeno, A03B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02A_PNN<-Estandar_75(A02A_PNN, "A02 - Marea Alta", A02A_PNN$Oxigeno, A02A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02B_PNN<-Estandar_75(A02B_PNN, "A02 - Marea Baja", A02B_PNN$Oxigeno, A02B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01A_PNN<-Estandar_75(A01A_PNN, "A01 - Marea Alta", A01A_PNN$Oxigeno, A01A_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01B_PNN<-Estandar_75(A01B_PNN, "A01 - Marea Baja", A01B_PNN$Oxigeno, A01B_PNN$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")


tiff(filename = "Oxigeno_Boca_Amarales_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Oxi_A01A_PNN, Oxi_A01B_PNN, Oxi_A02A_PNN,Oxi_A02B_PNN, Oxi_A03A_PNN, Oxi_A03B_PNN, Oxi_A04A_PNN, Oxi_A04B_PNN, Oxi_A05A_PNN,Oxi_A05B_PNN, Oxi_A06A_PNN, Oxi_A06B_PNN, top="Boca Amarales")
dev.off()



Temp_S06A_PNN<-Estandar_15(S06A_PNN, "S06 - Marea Alta", S06A_PNN$Temperatura, S06A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S06B_PNN<-Estandar_15(S06B_PNN, "S06 - Marea Baja", S06B_PNN$Temperatura, S06B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05A_PNN<-Estandar_15(S05A_PNN, "S05 - Marea Alta", S05A_PNN$Temperatura, S05A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05B_PNN<-Estandar_15(S05B_PNN, "S05 - Marea Baja", S05B_PNN$Temperatura, S05B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04A_PNN<-Estandar_15(S04A_PNN, "S04 - Marea Alta", S04A_PNN$Temperatura, S04A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04B_PNN<-Estandar_15(S04B_PNN, "S04 - Marea Baja", S04B_PNN$Temperatura, S04B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03A_PNN<-Estandar_40(S03A_PNN, "S03 - Marea Alta", S03A_PNN$Temperatura, S03A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03B_PNN<-Estandar_40(S03B_PNN, "S03 - Marea Baja", S03B_PNN$Temperatura, S03B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02A_PNN<-Estandar_75(S02A_PNN, "S02 - Marea Alta", S02A_PNN$Temperatura, S02A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02B_PNN<-Estandar_75(S02B_PNN, "S02 - Marea Baja", S02B_PNN$Temperatura, S02B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01A_PNN<-Estandar_75(S01A_PNN, "S01 - Marea Alta", S01A_PNN$Temperatura, S01A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01B_PNN<-Estandar_75(S01B_PNN, "S01 - Marea Baja", S01B_PNN$Temperatura, S01B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_S01A_PNN, Temp_S01B_PNN, Temp_S02A_PNN,Temp_S02B_PNN, Temp_S03A_PNN, Temp_S03B_PNN, Temp_S04A_PNN, Temp_S04B_PNN, Temp_S05A_PNN,Temp_S05B_PNN, Temp_S06A_PNN, Temp_S06B_PNN, top="Boca Sanquianga")
dev.off()

Sal_S06A_PNN<-Estandar_15(S06A_PNN, "S06 - Marea Alta", S06A_PNN$Salinidad, S06A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S06B_PNN<-Estandar_15(S06B_PNN, "S06 - Marea Baja", S06B_PNN$Salinidad, S06B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05A_PNN<-Estandar_15(S05A_PNN, "S05 - Marea Alta", S05A_PNN$Salinidad, S05A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05B_PNN<-Estandar_15(S05B_PNN, "S05 - Marea Baja", S05B_PNN$Salinidad, S05B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04A_PNN<-Estandar_15(S04A_PNN, "S04 - Marea Alta", S04A_PNN$Salinidad, S04A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04B_PNN<-Estandar_15(S04B_PNN, "S04 - Marea Baja", S04B_PNN$Salinidad, S04B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03A_PNN<-Estandar_40(S03A_PNN, "S03 - Marea Alta", S03A_PNN$Salinidad, S03A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03B_PNN<-Estandar_40(S03B_PNN, "S03 - Marea Baja", S03B_PNN$Salinidad, S03B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02A_PNN<-Estandar_75(S02A_PNN, "S02 - Marea Alta", S02A_PNN$Salinidad, S02A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02B_PNN<-Estandar_75(S02B_PNN, "S02 - Marea Baja", S02B_PNN$Salinidad, S02B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01A_PNN<-Estandar_75(S01A_PNN, "S01 - Marea Alta", S01A_PNN$Salinidad, S01A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01B_PNN<-Estandar_75(S01B_PNN, "S01 - Marea Baja", S01B_PNN$Salinidad, S01B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "Salinidad_Boca_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_S01A_PNN, Sal_S01B_PNN, Sal_S02A_PNN,Sal_S02B_PNN, Sal_S03A_PNN, Sal_S03B_PNN, Sal_S04A_PNN, Sal_S04B_PNN, Sal_S05A_PNN,Sal_S05B_PNN, Sal_S06A_PNN, Sal_S06B_PNN, top="Boca Sanquianga")
dev.off()

Den_S06A_PNN<-Estandar_15(S06A_PNN, "S06 - Marea Alta", S06A_PNN$Densidad, S06A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S06B_PNN<-Estandar_15(S06B_PNN, "S06 - Marea Baja", S06B_PNN$Densidad, S06B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05A_PNN<-Estandar_15(S05A_PNN, "S05 - Marea Alta", S05A_PNN$Densidad, S05A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05B_PNN<-Estandar_15(S05B_PNN, "S05 - Marea Baja", S05B_PNN$Densidad, S05B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04A_PNN<-Estandar_15(S04A_PNN, "S04 - Marea Alta", S04A_PNN$Densidad, S04A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04B_PNN<-Estandar_15(S04B_PNN, "S04 - Marea Baja", S04B_PNN$Densidad, S04B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03A_PNN<-Estandar_40(S03A_PNN, "S03 - Marea Alta", S03A_PNN$Densidad, S03A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03B_PNN<-Estandar_40(S03B_PNN, "S03 - Marea Baja", S03B_PNN$Densidad, S03B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02A_PNN<-Estandar_75(S02A_PNN, "S02 - Marea Alta", S02A_PNN$Densidad, S02A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02B_PNN<-Estandar_75(S02B_PNN, "S02 - Marea Baja", S02B_PNN$Densidad, S02B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01A_PNN<-Estandar_75(S01A_PNN, "S01 - Marea Alta", S01A_PNN$Densidad, S01A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01B_PNN<-Estandar_75(S01B_PNN, "S01 - Marea Baja", S01B_PNN$Densidad, S01B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")


tiff(filename = "Densidad_Boca_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_S01A_PNN, Den_S01B_PNN, Den_S02A_PNN,Den_S02B_PNN, Den_S03A_PNN, Den_S03B_PNN, Den_S04A_PNN, Den_S04B_PNN, Den_S05A_PNN,Den_S05B_PNN, Den_S06A_PNN, Den_S06B_PNN, top="Boca Sanquianga")
dev.off()


Oxi_S06A_PNN<-Estandar_15(S06A_PNN, "S06 - Marea Alta", S06A_PNN$Oxigeno, S06A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S06B_PNN<-Estandar_15(S06B_PNN, "S06 - Marea Baja", S06B_PNN$Oxigeno, S06B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05A_PNN<-Estandar_15(S05A_PNN, "S05 - Marea Alta", S05A_PNN$Oxigeno, S05A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05B_PNN<-Estandar_15(S05B_PNN, "S05 - Marea Baja", S05B_PNN$Oxigeno, S05B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04A_PNN<-Estandar_15(S04A_PNN, "S04 - Marea Alta", S04A_PNN$Oxigeno, S04A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04B_PNN<-Estandar_15(S04B_PNN, "S04 - Marea Baja", S04B_PNN$Oxigeno, S04B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03A_PNN<-Estandar_40(S03A_PNN, "S03 - Marea Alta", S03A_PNN$Oxigeno, S03A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03B_PNN<-Estandar_40(S03B_PNN, "S03 - Marea Baja", S03B_PNN$Oxigeno, S03B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02A_PNN<-Estandar_75(S02A_PNN, "S02 - Marea Alta", S02A_PNN$Oxigeno, S02A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02B_PNN<-Estandar_75(S02B_PNN, "S02 - Marea Baja", S02B_PNN$Oxigeno, S02B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01A_PNN<-Estandar_75(S01A_PNN, "S01 - Marea Alta", S01A_PNN$Oxigeno, S01A_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01B_PNN<-Estandar_75(S01B_PNN, "S01 - Marea Baja", S01B_PNN$Oxigeno, S01B_PNN$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")

tiff(filename = "Oxigeno_Boca_Sanquianga_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Oxi_S01A_PNN, Oxi_S01B_PNN, Oxi_S02A_PNN,Oxi_S02B_PNN, Oxi_S03A_PNN, Oxi_S03B_PNN, Oxi_S04A_PNN, Oxi_S04B_PNN, Oxi_S05A_PNN,Oxi_S05B_PNN, Oxi_S06A_PNN, Oxi_S06B_PNN, top="Boca Sanquianga")
dev.off()


Temp_G06A_PNN<-Estandar_15(G06A_PNN, "G06 - Marea Alta", G06A_PNN$Temperatura, G06A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G06B_PNN<-Estandar_15(G06B_PNN, "G06 - Marea Baja", G06B_PNN$Temperatura, G06B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05A_PNN<-Estandar_15(G05A_PNN, "G05 - Marea Alta", G05A_PNN$Temperatura, G05A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05B_PNN<-Estandar_15(G05B_PNN, "G05 - Marea Baja", G05B_PNN$Temperatura, G05B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04A_PNN<-Estandar_15(G04A_PNN, "G04 - Marea Alta", G04A_PNN$Temperatura, G04A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04B_PNN<-Estandar_15(G04B_PNN, "G04 - Marea Baja", G04B_PNN$Temperatura, G04B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03A_PNN<-Estandar_40(G03A_PNN, "G03 - Marea Alta", G03A_PNN$Temperatura, G03A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03B_PNN<-Estandar_40(G03B_PNN, "G03 - Marea Baja", G03B_PNN$Temperatura, G03B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02A_PNN<-Estandar_75(G02A_PNN, "G02 - Marea Alta", G02A_PNN$Temperatura, G02A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02B_PNN<-Estandar_75(G02B_PNN, "G02 - Marea Baja", G02B_PNN$Temperatura, G02B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01A_PNN<-Estandar_75(G01A_PNN, "G01 - Marea Alta", G01A_PNN$Temperatura, G01A_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01B_PNN<-Estandar_75(G01B_PNN, "G01 - Marea Baja", G01B_PNN$Temperatura, G01B_PNN$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_G01A_PNN, Temp_G01B_PNN, Temp_G02A_PNN,Temp_G02B_PNN, Temp_G03A_PNN, Temp_G03B_PNN, Temp_G04A_PNN, Temp_G04B_PNN, Temp_G05A_PNN,Temp_G05B_PNN, Temp_G06A_PNN, Temp_G06B_PNN, top="Boca Guascama")
dev.off()

Sal_G06A_PNN<-Estandar_15(G06A_PNN, "G06 - Marea Alta", G06A_PNN$Salinidad, G06A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G06B_PNN<-Estandar_15(G06B_PNN, "G06 - Marea Baja", G06B_PNN$Salinidad, G06B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05A_PNN<-Estandar_15(G05A_PNN, "G05 - Marea Alta", G05A_PNN$Salinidad, G05A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05B_PNN<-Estandar_15(G05B_PNN, "G05 - Marea Baja", G05B_PNN$Salinidad, G05B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04A_PNN<-Estandar_15(G04A_PNN, "G04 - Marea Alta", G04A_PNN$Salinidad, G04A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04B_PNN<-Estandar_15(G04B_PNN, "G04 - Marea Baja", G04B_PNN$Salinidad, G04B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03A_PNN<-Estandar_40(G03A_PNN, "G03 - Marea Alta", G03A_PNN$Salinidad, G03A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03B_PNN<-Estandar_40(G03B_PNN, "G03 - Marea Baja", G03B_PNN$Salinidad, G03B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02A_PNN<-Estandar_75(G02A_PNN, "G02 - Marea Alta", G02A_PNN$Salinidad, G02A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02B_PNN<-Estandar_75(G02B_PNN, "G02 - Marea Baja", G02B_PNN$Salinidad, G02B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01A_PNN<-Estandar_75(G01A_PNN, "G01 - Marea Alta", G01A_PNN$Salinidad, G01A_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01B_PNN<-Estandar_75(G01B_PNN, "G01 - Marea Baja", G01B_PNN$Salinidad, G01B_PNN$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")


tiff(filename = "Salinidad_Boca_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_G01A_PNN, Sal_G01B_PNN, Sal_G02A_PNN,Sal_G02B_PNN, Sal_G03A_PNN, Sal_G03B_PNN, Sal_G04A_PNN, Sal_G04B_PNN, Sal_G05A_PNN,Sal_G05B_PNN, Sal_G06A_PNN, Sal_G06B_PNN, top="Boca Guascama")
dev.off()


Den_G06A_PNN<-Estandar_15(G06A_PNN, "G06 - Marea Alta", G06A_PNN$Densidad, G06A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G06B_PNN<-Estandar_15(G06B_PNN, "G06 - Marea Baja", G06B_PNN$Densidad, G06B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05A_PNN<-Estandar_15(G05A_PNN, "G05 - Marea Alta", G05A_PNN$Densidad, G05A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05B_PNN<-Estandar_15(G05B_PNN, "G05 - Marea Baja", G05B_PNN$Densidad, G05B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04A_PNN<-Estandar_15(G04A_PNN, "G04 - Marea Alta", G04A_PNN$Densidad, G04A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04B_PNN<-Estandar_15(G04B_PNN, "G04 - Marea Baja", G04B_PNN$Densidad, G04B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03A_PNN<-Estandar_40(G03A_PNN, "G03 - Marea Alta", G03A_PNN$Densidad, G03A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03B_PNN<-Estandar_40(G03B_PNN, "G03 - Marea Baja", G03B_PNN$Densidad, G03B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02A_PNN<-Estandar_75(G02A_PNN, "G02 - Marea Alta", G02A_PNN$Densidad, G02A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02B_PNN<-Estandar_75(G02B_PNN, "G02 - Marea Baja", G02B_PNN$Densidad, G02B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01A_PNN<-Estandar_75(G01A_PNN, "G01 - Marea Alta", G01A_PNN$Densidad, G01A_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01B_PNN<-Estandar_75(G01B_PNN, "G01 - Marea Baja", G01B_PNN$Densidad, G01B_PNN$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "Densidad_Boca_Guascama_PNN.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_G01A_PNN, Den_G01B_PNN, Den_G02A_PNN,Den_G02B_PNN, Den_G03A_PNN, Den_G03B_PNN, Den_G04A_PNN, Den_G04B_PNN, Den_G05A_PNN,Den_G05B_PNN, Den_G06A_PNN, Den_G06B_PNN, top="Boca Guascama")
dev.off()




#Temperatura
Temp01_PNN<-grid.arrange(nrow=6, ncol=2,Temp_A01A_PNN, Temp_A01B_PNN, Temp_A02A_PNN,Temp_A02B_PNN, Temp_A03A_PNN, Temp_A03B_PNN, Temp_A04A_PNN, Temp_A04B_PNN, Temp_A05A_PNN,Temp_A05B_PNN, Temp_A06A_PNN, Temp_A06B_PNN, top="Boca Amarales")
Temp02_PNN<-grid.arrange(nrow=6, ncol=2,Temp_S01A_PNN, Temp_S01B_PNN, Temp_S02A_PNN,Temp_S02B_PNN, Temp_S03A_PNN, Temp_S03B_PNN, Temp_S04A_PNN, Temp_S04B_PNN, Temp_S05A_PNN,Temp_S05B_PNN, Temp_S06A_PNN, Temp_S06B_PNN, top="Boca Sanquianga")
Temp03_PNN<-grid.arrange(nrow=6, ncol=2,Temp_G01A_PNN, Temp_G01B_PNN, Temp_G02A_PNN,Temp_G02B_PNN, Temp_G03A_PNN, Temp_G03B_PNN, Temp_G04A_PNN, Temp_G04B_PNN, Temp_G05A_PNN,Temp_G05B_PNN, Temp_G06A_PNN, Temp_G06B_PNN, top="Boca Guascama")
tiff(filename = "Temperatura_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Temp01_PNN, Temp02_PNN, Temp03_PNN)
dev.off() 


Sal01_PNN<-grid.arrange(nrow=6, ncol=2,Sal_S01A_PNN, Sal_S01B_PNN, Sal_S02A_PNN,Sal_S02B_PNN, Sal_S03A_PNN, Sal_S03B_PNN, Sal_S04A_PNN, Sal_S04B_PNN, Sal_S05A_PNN,Sal_S05B_PNN, Sal_S06A_PNN, Sal_S06B_PNN, top="Boca Sanquianga")
Sal02_PNN<-grid.arrange(nrow=6, ncol=2,Sal_A01A_PNN, Sal_A01B_PNN, Sal_A02A_PNN,Sal_A02B_PNN, Sal_A03A_PNN, Sal_A03B_PNN, Sal_A04A_PNN, Sal_A04B_PNN, Sal_A05A_PNN,Sal_A05B_PNN, Sal_A06A_PNN, Sal_A06B_PNN, top="Boca Amarales")
Sal03_PNN<-grid.arrange(nrow=6, ncol=2,Sal_G01A_PNN, Sal_G01B_PNN, Sal_G02A_PNN,Sal_G02B_PNN, Sal_G03A_PNN, Sal_G03B_PNN, Sal_G04A_PNN, Sal_G04B_PNN, Sal_G05A_PNN,Sal_G05B_PNN, Sal_G06A_PNN, Sal_G06B_PNN, top="Boca Guascama")

tiff(filename = "Salinidad_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Sal01_PNN, Sal02_PNN, Sal03_PNN)
dev.off()

Den01_PNN<-grid.arrange(nrow=6, ncol=2,Den_S01A_PNN, Den_S01B_PNN, Den_S02A_PNN,Den_S02B_PNN, Den_S03A_PNN, Den_S03B_PNN, Den_S04A_PNN, Den_S04B_PNN, Den_S05A_PNN,Den_S05B_PNN, Den_S06A_PNN, Den_S06B_PNN, top="Boca Sanquianga")
Den02_PNN<-grid.arrange(nrow=6, ncol=2,Den_A01A_PNN, Den_A01B_PNN, Den_A02A_PNN,Den_A02B_PNN, Den_A03A_PNN, Den_A03B_PNN, Den_A04A_PNN, Den_A04B_PNN, Den_A05A_PNN,Den_A05B_PNN, Den_A06A_PNN, Den_A06B_PNN, top="Boca Amarales")
Den03_PNN<-grid.arrange(nrow=6, ncol=2,Den_G01A_PNN, Den_G01B_PNN, Den_G02A_PNN,Den_G02B_PNN, Den_G03A_PNN, Den_G03B_PNN, Den_G04A_PNN, Den_G04B_PNN, Den_G05A_PNN,Den_G05B_PNN, Den_G06A_PNN, Den_G06B_PNN, top="Boca Guascama")

tiff(filename = "Densidad_Perfiles_PNN.tif", width = 50, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=1, ncol=3,Den01_PNN, Den02_PNN, Den03_PNN)
dev.off()

