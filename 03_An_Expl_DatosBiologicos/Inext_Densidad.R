

install.packages("entropart")
install.packages("iNEXT")
install.packages("vegan")
install.packages("dplyr")
install.packages("tidyr")
install.packages("C:/Users/chris/Downloads/entropart_1.6-11.zip", repos = NULL, type = "source")

library(entropart)
library(iNEXT)
library(vegan)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(parallel)

Codigo_fito_Densidad<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Densidad.csv", sep=",", header = TRUE)

#Evaluación de la composición entre transectos sectores y mareas
MRPP_Transecto_Densidad<-vegan::mrpp(dat = Codigo_fito_Densidad[,5:145],  Codigo_fito_Densidad$Transecto, permutations = 2000)
MRPP_Marea_Densidad<-vegan::mrpp(dat = Codigo_fito_Densidad[,5:145],  Codigo_fito_Densidad$Marea, permutations = 2000)
MRPP_Sector_Densidad<-vegan::mrpp(dat = Codigo_fito_Densidad[,5:145],  Codigo_fito_Densidad$Sector, permutations = 2000)


capture.output("MRPP - Transectos",
  MRPP_Transecto_Densidad,
  "MRPP - Marea",
  MRPP_Marea_Densidad, 
  "MRPP - Sector",
  MRPP_Sector_Densidad, 
  file = "./Resultados/mrpp_resultados_ComposicionBasadaDensidad.txt")


#Transectos####

Amarales_Densidad<-filter(Codigo_fito_Densidad, Transecto=="Amarales")
Sanquianga_Densidad<-filter(Codigo_fito_Densidad, Transecto=="Sanquianga")
Guascama_Densidad<-filter(Codigo_fito_Densidad, Transecto=="Guascama")

amaSum_Densidad<-as.data.frame(t(Amarales_Densidad[,6:145]%>% summarise_all(sum)))
sanSum_Densidad<-as.data.frame(t(Sanquianga_Densidad[,6:145]%>% summarise_all(sum)))
guaSum_Densidad<-as.data.frame(t(Guascama_Densidad[,6:145]%>% summarise_all(sum)))

amaDiv_Densidad<-filter(amaSum_Densidad, V1>0)
sanDiv_Densidad<-filter(sanSum_Densidad, V1>0)
guaDiv_Densidad<-filter(guaSum_Densidad, V1>0)

amaVec_Densidad<-as.vector(amaDiv_Densidad$V1)
sanVec_Densidad<-as.vector(sanDiv_Densidad$V1)
guaVec_Densidad<-as.vector(guaDiv_Densidad$V1)


Transectos_Densidad<-list(amaVec_Densidad, sanVec_Densidad, guaVec_Densidad)
names(Transectos_Densidad)<-c("Amarales", "Sanquianga", "Guascama")

#Inext###





# Comando general de iNEXT 
Transectos_Plot_Densidad <- iNEXT(Transectos_Densidad, 
                   q=c(0,1,2), 
                   datatype = "abundance",
                   endpoint = 10000) # q = 0 es la riqueza 

Transectos_Plot_Densidad_Data_Info<-Transectos_Plot_Densidad$DataInfo # showing basic data information.
Transectos_Plot_Densidad_AsyEst<-Transectos_Plot_Densidad$AsyEst # showing asymptotic diversity estimates.

capture.output("Datos del analisis de Densidad por Transecto",Transectos_Plot_Densidad_Data_Info, "Estimadores para Q0, Q1, Q2",Transectos_Plot_Densidad_AsyEst, file="./Resultados/Transectos_Densidad_Resultados.txt")
Transecto_Data_Densidad<- Transectos_Plot_Densidad$iNextEst$size_based 
write.csv(Transecto_Data_Densidad, file = "./Resultados/Transecto_Data_Densidad.csv",  row.names = FALSE)

dataprueba_Densidad<-Transecto_Data_Densidad %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)


MRPP_Transecto_Densidad<-vegan::mrpp(dat = dataprueba_Densidad,  Transecto_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Transecto_Densidad, file="./Resultados/MRPP_Transecto_Densidad.txt")



#Marea

Alta_Densidad<-filter(Codigo_fito_Densidad, Marea=="Alta")
Baja_Densidad<-filter(Codigo_fito_Densidad, Marea=="Baja")


AltaSum_Densidad<-as.data.frame(t(Alta_Densidad[,6:145]%>% summarise_all(sum)))
BajaSum_Densidad<-as.data.frame(t(Baja_Densidad[,6:145]%>% summarise_all(sum)))


AltaDiv_Densidad<-filter(AltaSum_Densidad, V1>0)
BajaDiv_Densidad<-filter(BajaSum_Densidad, V1>0)


AltaVec_Densidad<-as.vector(AltaDiv_Densidad$V1)
BajaVec_Densidad<-as.vector(BajaDiv_Densidad$V1)


AltaVec_Densidad<-append(AltaVec_Densidad,18,0)
BajaVec_Densidad<-append(BajaVec_Densidad,18,0)


Marea_Densidad<-list(AltaVec_Densidad, BajaVec_Densidad)
names(Marea_Densidad)<-c("Alta", "Baja")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Marea_Plot_Densidad <- iNEXT(Marea_Densidad, 
                         q=c(0,1,2), 
                         datatype = "abundance",
                         endpoint = 10000) # q = 0 es la riqueza (diversidades verdaderas)



Marea_Plot_Densidad_Data_Info<-Marea_Plot_Densidad$DataInfo # showing basic data information.
Marea_Plot_Densidad_AsyEst<-Marea_Plot_Densidad$AsyEst # showing asymptotic diversity estimates.
capture.output("Datos del analisis de Densidad por Transecto",Marea_Plot_Densidad_Data_Info, "Estimadores para Q0, Q1, Q2",Marea_Plot_Densidad_AsyEst, file="./Resultados/Marea_Densidad_Resultados.txt")

Marea_Data_Densidad<- Marea_Plot_Densidad$iNextEst$size_based 
write.csv(Marea_Data_Densidad, file = "./Resultados/Marea_Data_Densidad.csv",  row.names = FALSE)


dataprueba_Densidad<-Marea_Data_Densidad %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)




MRPP_Marea_Densidad<-vegan::mrpp(dat = dataprueba_Densidad,  Marea_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Densidad, file="./Resultados/MRPP_Marea_Densidad.txt")



#Sector

Oceanico_Densidad<-filter(Codigo_fito_Densidad, Sector=="Oceanico")
Costero_Densidad<-filter(Codigo_fito_Densidad, Sector=="Costero")


OceanicoSum_Densidad<-as.data.frame(t(Oceanico_Densidad[,6:145]%>% summarise_all(sum)))
CosteroSum_Densidad<-as.data.frame(t(Costero_Densidad[,6:145]%>% summarise_all(sum)))


OceanicoDiv_Densidad<-filter(OceanicoSum_Densidad, V1>0)
CosteroDiv_Densidad<-filter(CosteroSum_Densidad, V1>0)


OceanicoVec_Densidad<-as.vector(OceanicoDiv_Densidad$V1)
CosteroVec_Densidad<-as.vector(CosteroDiv_Densidad$V1)


OceanicoVec_Densidad<-append(OceanicoVec_Densidad,18,0)
CosteroVec_Densidad<-append(CosteroVec_Densidad,18,0)


Sector_Densidad<-list(OceanicoVec_Densidad, CosteroVec_Densidad)
names(Sector_Densidad)<-c("Oceanico", "Costero")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Sector_Plot_Densidad <- iNEXT(Sector_Densidad, 
                    q=c(0,1,2), 
                    datatype = "abundance",
                    endpoint = 10000) # q = 0 es la riqueza (diversidades verdaderas)



Sector_Plot_Densidad_Data_Info<-Sector_Plot_Densidad$DataInfo # showing basic data information.
Sector_Plot_Densidad_AsyEst<-Sector_Plot_Densidad$AsyEst # showing asymptotic diversity estimates.
capture.output("Datos del analisis de Densidad por Transecto",Sector_Plot_Densidad_Data_Info, "Estimadores para Q0, Q1, Q2",Sector_Plot_Densidad_AsyEst, file="./Resultados/Sector_Densidad_Resultados.txt")

Sector_Data_Densidad<- Sector_Plot_Densidad$iNextEst$size_based 
write.csv(Sector_Data_Densidad, file = "./Resultados/Sector_Data_Densidad.csv",  row.names = FALSE)


Sector_dataprueba_Densidad<-Marea_Data_Densidad %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)



MRPP_Sector_Densidad<-vegan::mrpp(dat = Sector_dataprueba_Densidad,  Sector_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Densidad, file = "./Resultados/MRPP_Sector_Densidad.txt")


Transectos_Plot_Hill_Densidad<-ggiNEXT(Transectos_Plot_Densidad, type=1,facet.var = "Order.q")
Marea_Plot_Hill_Densidad<-ggiNEXT(Marea_Plot_Densidad, type=1,facet.var = "Order.q")
Sector_Plot_Hill_Densidad<-ggiNEXT(Sector_Plot_Densidad, type=1,facet.var = "Order.q")

Transectos_Plot_Coverage_Densidad<-ggiNEXT(Transectos_Plot_Densidad, type=3,facet.var = "Order.q")
Marea_Plot_Coverage_Densidad<-ggiNEXT(Marea_Plot_Densidad, type=3,facet.var = "Order.q")
Sector_Plot_Coverage_Densidad<-ggiNEXT(Sector_Plot_Densidad, type=3,facet.var = "Order.q")


png(filename="./Imagenes/01_Densidad_Total_Plot_hill_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Hill_Densidad,Marea_Plot_Hill_Densidad,Sector_Plot_Hill_Densidad, ncol=1)
dev.off()



png(filename="./Imagenes/02_Densidad_Total_Plot_coverage_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Coverage_Densidad,Marea_Plot_Coverage_Densidad,Sector_Plot_Coverage_Densidad, ncol=1)
dev.off()


