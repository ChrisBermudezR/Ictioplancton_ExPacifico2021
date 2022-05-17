library(tidyverse)
library(gridExtra)

Datos_Quimica<-read.table("Quimica.csv", header = TRUE, sep=",")


Datos_Quimica$Separar<-Datos_Quimica$Estacion
Datos_Quimica<-separate(Datos_Quimica, Separar, c("Boca", "No.Estacion"), sep = "0" )
as.factor(Datos_Quimica$Boca)->Datos_Quimica$Boca
as.numeric(Datos_Quimica$No.Estacion)->Datos_Quimica$No.Estacion



Datos_Quimica$Boca <- recode_factor(Datos_Quimica$Boca, 
                                     A = "Amarales", 
                                     S = "Sanquianga", 
                                     G = "Guamales")


write.table(Datos_Quimica, "Datos_Quimica.csv", col.names = TRUE, sep=",")

Datos_Quimica$Boca2 <- with(Datos_Quimica, relevel(Boca, "Amarales"))

NO2_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=NO2, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "NO2 [µM]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 
NO3_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=NO3, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "NO3 [µM]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 

PO4_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=PO4, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "PO4 [µM]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SiO2_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=SiO2, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "SiO2 [µM]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 
Clorofila_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=Clorofila, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "Clorofila [mg/m3]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Salinidad_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=Salinidad, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "Salinidad [PSU]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
pH_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=pH, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "pH", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
OD_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=OD, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "Oxígeno Disuelto [mg/L]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Transparencia_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=Transparencia, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "Transparencia [m]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SST_Quimica<-ggplot(Datos_Quimica, aes(x=Boca, y=SST, color=Marea)) + 
  geom_boxplot()+ 
  labs( y = "SST [mg/L]", x = "Boca de los ríos")+
  theme_classic()+
  geom_point(position = position_jitterdodge())

tiff(filename = "01_Datos_Quimica.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica, NO3_Quimica, PO4_Quimica,SiO2_Quimica,Clorofila_Quimica,Salinidad_Quimica,pH_Quimica,OD_Quimica,Transparencia_Quimica,SST_Quimica,
             top="Datos totales")
dev.off()



NO2_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=NO2)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "NO2 [µM]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
   theme_bw()+
  facet_grid(Marea~Boca)

NO3_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=NO3)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "NO3 [µM]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
PO4_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=PO4)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "PO4 [µM]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
SiO2_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=SiO2)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "SiO2 [µM]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
Clorofila_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Clorofila)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Clorofila [mg/m3]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
Salinidad_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Salinidad)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Salinidad [PSU]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
pH_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=pH)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "pH")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
OD_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=OD)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Oxígeno Disuelto [mg/L]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
Transparencia_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Transparencia)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Transparencia [m]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)
SST_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=SST)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "SST [mg/L]")+
  scale_x_discrete(name ="Boca de los ríos", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Boca)


tiff(filename = "02_Quimica_Linea.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_line, NO3_line, PO4_line,SiO2_line,Clorofila_line,Salinidad_line,pH_line,OD_line,Transparencia_line,SST_line
             )
dev.off()
