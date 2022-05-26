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



Datos_Quimica<-read.table("./02_Datos/Quimicos/Datos_Quimica.csv", header = TRUE, sep=",")
as.factor(Datos_Quimica$Transecto)->Datos_Quimica$Transecto
as.numeric(Datos_Quimica$No.Estacion)->Datos_Quimica$No.Estacion

Datos_Fisica_Sup<-readr::read_csv("./01_Resultados/Fisicos_EstadisticasDescrip_CCCP.csv")
as.factor(Datos_Fisica_Sup$Transecto)->Datos_Fisica_Sup$Transecto
as.factor(Datos_Fisica_Sup$No.Estacion)->Datos_Fisica_Sup$No.Estacion

Datos_Fisica_Sup$Codigo2<-Datos_Fisica_Sup$Codigo

Datos_Fisica_Sup<-Datos_Fisica_Sup%>%
  separate(Codigo, c("Codigo", "Marea"), sep=3)

Datos_Fisica_Sup<-Datos_Fisica_Sup%>%
  separate(Codigo, c("Transecto", "No.Estacion"), sep=1)

Datos_Fisica_Sup$Marea<- recode_factor(Datos_Fisica_Sup$Marea, B ="Baja", A = "Alta")
Datos_Fisica_Sup$Transecto<- recode_factor(Datos_Fisica_Sup$Transecto, A ="Amarales", G = "Guascama", S="Sanquianga")
Datos_Fisica_Sup$No.Estacion<- recode_factor(Datos_Fisica_Sup$No.Estacion, "01"=1, "02"=2, "03"=3, "04"=4, "05"=5, "06"=6)


####Datos Físicos####
Temp_mean_Quimica_Total<-ggplot(Datos_Fisica_Sup, aes(x=Marea, y=Temperatura_mean, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "Temperatura [°C]", x = "Marea")+
  theme_classic()
geom_point(position = position_jitterdodge()) 


Temp_mean_Quimica<-ggplot(Datos_Fisica_Sup, aes(x=Transecto, y=Temperatura_mean, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = Temperatura_mean, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "Temperatura [°C]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 


####Datos Químicos####

NO2_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=NO2, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "NO2 [µM]", x = "Marea")+
  theme_classic()
  geom_point(position = position_jitterdodge()) 
NO3_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=NO3, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "NO3 [µM]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 

PO4_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=PO4, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "PO4 [µM]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SiO2_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=SiO2, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "SiO2 [µM]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 
Clorofila_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=Clorofila, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "Clorofila [mg/m3]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Salinidad_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=Salinidad, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "Salinidad [PSU]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
pH_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=pH, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "pH", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
OD_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=OD, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "Oxígeno Disuelto [mg/L]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Transparencia_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=Transparencia, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "Transparencia [m]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SST_Quimica_Total<-ggplot(Datos_Quimica, aes(x=Marea, y=SST, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  labs( y = "SST [mg/L]", x = "Marea")+
  theme_classic()+
  geom_point(position = position_jitterdodge())

tiff(filename = "01_Datos_Quimica_Total.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica_Total, NO3_Quimica_Total, PO4_Quimica_Total, SiO2_Quimica_Total ,Clorofila_Quimica_Total, Salinidad_Quimica_Total,pH_Quimica_Total,OD_Quimica_Total,Transparencia_Quimica_Total,SST_Quimica_Total,
             top="Datos totales")
dev.off()

png(filename = "./03_Imagenes/01_Datos_Quimica_Total.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica_Total, NO3_Quimica_Total, PO4_Quimica_Total, SiO2_Quimica_Total ,Clorofila_Quimica_Total, Salinidad_Quimica_Total,pH_Quimica_Total,OD_Quimica_Total,Transparencia_Quimica_Total,SST_Quimica_Total,
             top="Datos totales")
dev.off()

NO2_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=NO2, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = NO2, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
   labs( y = "NO2 [µM]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 
NO3_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=NO3, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = NO3, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "NO3 [µM]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 

PO4_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=PO4, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = PO4, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "PO4 [µM]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SiO2_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=SiO2, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = SiO2, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "SiO2 [µM]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge()) 
Clorofila_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=Clorofila, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = Clorofila, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "Clorofila [mg/m3]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Salinidad_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=Salinidad, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = Salinidad, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) + 
  labs( y = "Salinidad [PSU]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
pH_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=pH, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = pH, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) + 
  labs( y = "pH", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
OD_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=OD, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = OD, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) + 
  labs( y = "Oxígeno Disuelto [mg/L]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
Transparencia_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=Transparencia, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = Transparencia, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) + 
  labs( y = "Transparencia [m]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())
SST_Quimica<-ggplot(Datos_Quimica, aes(x=Transecto, y=SST, color=Marea)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, aes(y = SST, group=Marea), geom="point", shape=20, size=3, color="blue", position = position_dodge(width = 0.8)) +
  labs( y = "SST [mg/L]", x = "Transecto")+
  theme_classic()+
  geom_point(position = position_jitterdodge())

tiff(filename = "01_Datos_Quimica.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica, NO3_Quimica, PO4_Quimica,SiO2_Quimica,Clorofila_Quimica,Salinidad_Quimica,pH_Quimica,OD_Quimica,Transparencia_Quimica,SST_Quimica,
             top="Datos totales")
dev.off()

png(filename = "./03_Imagenes/01_Datos_Quimica.png", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=5, ncol=2, NO2_Quimica, NO3_Quimica, PO4_Quimica,SiO2_Quimica,Clorofila_Quimica,Salinidad_Quimica,pH_Quimica,OD_Quimica,Transparencia_Quimica,SST_Quimica,
             top="Datos totales")
dev.off()

NO2_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=NO2)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "NO2 [µM]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
   theme_bw()+
  facet_grid(Marea~Transecto)

NO3_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=NO3)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "NO3 [µM]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
PO4_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=PO4)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "PO4 [µM]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
SiO2_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=SiO2)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "SiO2 [µM]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
Clorofila_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Clorofila)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Clorofila [mg/m3]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
Salinidad_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Salinidad)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Salinidad [PSU]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
pH_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=pH)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "pH")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
OD_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=OD)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Oxígeno Disuelto [mg/L]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
Transparencia_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=Transparencia)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "Transparencia [m]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)
SST_line<-ggplot(Datos_Quimica, aes(x=No.Estacion, y=SST)) + 
  geom_line()+ 
  geom_point()+ 
  labs( y = "SST [mg/L]")+
  scale_x_discrete(name ="Transecto", 
                   limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  facet_grid(Marea~Transecto)


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
