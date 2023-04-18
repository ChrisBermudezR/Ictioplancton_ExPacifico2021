

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

Codigo_fito_incidencia<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Incidencia.csv", sep=",", header = TRUE)


#Evaluación de la composición entre transectos sectores y mareas


MRPP_Transecto_incidencia<-vegan::mrpp(dat = Codigo_fito_incidencia[,5:145],  Codigo_fito_incidencia$Transecto, permutations = 2000)
MRPP_Marea_incidencia<-vegan::mrpp(dat = Codigo_fito_incidencia[,5:145],  Codigo_fito_incidencia$Marea, permutations = 2000)
MRPP_Sector_incidencia<-vegan::mrpp(dat = Codigo_fito_incidencia[,5:145],  Codigo_fito_incidencia$Sector, permutations = 2000)


capture.output(MRPP_Transecto_incidencia,MRPP_Marea_incidencia, MRPP_Sector_incidencia, file = "./Resultados/mrpp_resultados_ComposicionBasadaincidencia.txt")


#Transectos####

Amarales_incidencia<-filter(Codigo_fito_incidencia, Transecto=="Amarales")
Sanquianga_incidencia<-filter(Codigo_fito_incidencia, Transecto=="Sanquianga")
Guascama_incidencia<-filter(Codigo_fito_incidencia, Transecto=="Guascama")

amaSum_incidencia<-as.data.frame(t(Amarales_incidencia[,6:145]%>% summarise_all(sum)))
sanSum_incidencia<-as.data.frame(t(Sanquianga_incidencia[,6:145]%>% summarise_all(sum)))
guaSum_incidencia<-as.data.frame(t(Guascama_incidencia[,6:145]%>% summarise_all(sum)))

amaDiv_incidencia<-filter(amaSum_incidencia, V1>0)
sanDiv_incidencia<-filter(sanSum_incidencia, V1>0)
guaDiv_incidencia<-filter(guaSum_incidencia, V1>0)

amaVec_incidencia<-as.vector(amaDiv_incidencia$V1)
sanVec_incidencia<-as.vector(sanDiv_incidencia$V1)
guaVec_incidencia<-as.vector(guaDiv_incidencia$V1)

amaVec_incidencia<-append(amaVec_incidencia,12,0)
sanVec_incidencia<-append(sanVec_incidencia,12,0)
guaVec_incidencia<-append(guaVec_incidencia,12,0)

Muestras_incidencia<-round(seq(1, 100, length.out=100))

Transectos_incidencia<-list(amaVec_incidencia, sanVec_incidencia, guaVec_incidencia)
names(Transectos_incidencia)<-c("Amarales", "Sanquianga", "Guascama")

#Inext###

# Comando general de iNEXT (calcula muchas cosas)
Transectos_Plot_incidencia <- iNEXT(Transectos_incidencia, 
                         q=c(0), 
                         datatype = "incidence_freq",
                         endpoint = 25,
                         size=Muestras_incidencia, 
                         se=TRUE) # q = 0 es la riqueza (diversidades verdaderas)



Transectos_Plot_incidencia_Data_Info<-Transectos_Plot_incidencia$DataInfo # showing basic data information.
Transectos_Plot_incidencia_AsyEst<-Transectos_Plot_incidencia$AsyEst # showing asymptotic diversity estimates.


capture.output("Datos del analisis de abundancia por Transecto",Transectos_Plot_incidencia_Data_Info, "Estimadores para Q0, Q1, Q2",Transectos_Plot_incidencia_AsyEst, file="./Resultados/Transectos_Abundancia_Resultados.txt")
Transecto_Data_incidencia<- Transectos_Plot_incidencia$iNextEst$size_based
write.csv(Transecto_Data_incidencia, file = "./Resultados/Transecto_Data_incidencia.csv",  row.names = FALSE)

Transecto_dataprueba_incidencia<-Transecto_Data_incidencia %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)



MRPP_Transecto_incidencia<-vegan::mrpp(dat = Transecto_dataprueba_incidencia,  Transecto_Data_incidencia$Assemblage, permutations = 2000)
capture.output(MRPP_Transecto_incidencia, file = "./Resultados/MRPP_Transecto_incidencia.txt")



#Marea

Alta_incidencia<-filter(Codigo_fito_incidencia, Marea=="Alta")
Baja_incidencia<-filter(Codigo_fito_incidencia, Marea=="Baja")


AltaSum_incidencia<-as.data.frame(t(Alta_incidencia[,5:145]%>% summarise_all(sum)))
BajaSum_incidencia<-as.data.frame(t(Baja_incidencia[,5:145]%>% summarise_all(sum)))


AltaDiv_incidencia<-filter(AltaSum_incidencia, V1>0)
BajaDiv_incidencia<-filter(BajaSum_incidencia, V1>0)


AltaVec_incidencia<-as.vector(AltaDiv_incidencia$V1)
BajaVec_incidencia<-as.vector(BajaDiv_incidencia$V1)


AltaVec_incidencia<-append(AltaVec_incidencia,18,0)
BajaVec_incidencia<-append(BajaVec_incidencia,18,0)


Marea_incidencia<-list(AltaVec_incidencia, BajaVec_incidencia)
names(Marea_incidencia)<-c("Alta", "Baja")

#Inext####

Marea_Plot_incidencia <- iNEXT(Marea_incidencia, 
                                    q=c(0), 
                                    datatype = "incidence_freq",
                                    endpoint = 25,
                                    size=Muestras_incidencia, 
                                    se=TRUE) # q = 0 es la riqueza (diversidades verdaderas)



Marea_Plot_incidencia_Data_Info<-Marea_Plot_incidencia$DataInfo # showing basic data information.
Marea_Plot_incidencia_AsyEst<-Marea_Plot_incidencia$AsyEst # showing asymptotic diversity estimates.
Marea_Data_incidencia<- Marea_Plot_incidencia$iNextEst$size_based 

Marea_dataprueba_incidencia<-Marea_Data_incidencia %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)

MRPP_Marea_incidencia<-vegan::mrpp(dat = Marea_dataprueba_incidencia,  Marea_Data_incidencia$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_incidencia, file = "./Resultados/MRPP_Marea_incidencia.txt")






#Sector

Oceanico_incidencia<-filter(Codigo_fito_incidencia, Sector=="Oceanico")
Costero_incidencia<-filter(Codigo_fito_incidencia, Sector=="Costero")


OceanicoSum_incidencia<-as.data.frame(t(Oceanico_incidencia[,5:145]%>% summarise_all(sum)))
CosteroSum_incidencia<-as.data.frame(t(Costero_incidencia[,5:145]%>% summarise_all(sum)))


OceanicoDiv_incidencia<-filter(OceanicoSum_incidencia, V1>0)
CosteroDiv_incidencia<-filter(CosteroSum_incidencia, V1>0)


OceanicoVec_incidencia<-as.vector(OceanicoDiv_incidencia$V1)
CosteroVec_incidencia<-as.vector(CosteroDiv_incidencia$V1)


OceanicoVec_incidencia<-append(OceanicoVec_incidencia,18,0)
CosteroVec_incidencia<-append(CosteroVec_incidencia,18,0)


Sector_incidencia<-list(OceanicoVec_incidencia, CosteroVec_incidencia)
names(Sector_incidencia)<-c("Oceanico", "Costero")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)


Sector_Plot_incidencia <- iNEXT(Sector_incidencia, 
                               q=c(0), 
                               datatype = "incidence_freq",
                               endpoint = 25,
                               size=Muestras_incidencia, 
                               se=TRUE) # q = 0 es la riqueza (diversidades verdaderas)



Sector_Plot_incidencia_Data_Info<-Sector_Plot_incidencia$DataInfo # showing basic data information.
Sector_Plot_incidencia_AsyEst<-Sector_Plot_incidencia$AsyEst # showing asymptotic diversity estimates.
Sector_Data_incidencia<- Sector_Plot_incidencia$iNextEst$size_based 



Sector_dataprueba_incidencia<-Marea_Data_incidencia %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)



MRPP_Sector_incidencia<-vegan::mrpp(dat = Sector_dataprueba_incidencia,  Sector_Data_incidencia$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_incidencia, file = "./Resultados/MRPP_Sector_incidencia.txt")





Transectos_Plot_Hill_incidencia<-ggiNEXT(Transectos_Plot_incidencia, type=1)
Marea_Plot_Hill_incidencia<-ggiNEXT(Marea_Plot_incidencia, type=1)
Sector_Plot_Hill_incidencia<-ggiNEXT(Sector_Plot_incidencia, type=1)

Transectos_Plot_Coverage_incidencia<-ggiNEXT(Transectos_Plot_incidencia, type=3)
Marea_Plot_Coverage_incidencia<-ggiNEXT(Marea_Plot_incidencia, type=3)
Sector_Plot_Coverage_incidencia<-ggiNEXT(Sector_Plot_incidencia, type=3)


png(filename="./Imagenes/01_Incidencia_Total_Plot_hill_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Hill_incidencia,Marea_Plot_Hill_incidencia,Sector_Plot_Hill_incidencia, ncol=1)
dev.off()



 png(filename="./Imagenes/02_Incidencia_Total_Plot_coverage_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Coverage_incidencia,Marea_Plot_Coverage_incidencia,Sector_Plot_Coverage_incidencia, ncol=1)
dev.off()


