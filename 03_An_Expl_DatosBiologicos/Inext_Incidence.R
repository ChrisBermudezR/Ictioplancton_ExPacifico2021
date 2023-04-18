

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
Transecto_Data_incidencia<- Transectos_Plot_incidencia$iNextEst$size_based 



Transecto_dataprueba_incidencia<-Transecto_Data_incidencia %>% select(qD)



MRPP_Transecto_incidencia<-vegan::mrpp(dat = Transecto_dataprueba_incidencia,  Transecto_Data_incidencia$Assemblage, permutations = 2000)
capture.output(MRPP_Transecto_incidencia, file = "./Resultados/MRPP_Transecto_incidencia.txt")

# Sample-size-based R/E curves, separating plots by "site"


png(filename="./Imagenes/Incidencia_Transectos_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot_incidencia, type=3,)
dev.off()

png(filename="./Imagenes/Incidencia_Transectos_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot_incidencia, type=1)
dev.off()







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



Marea_dataprueba_incidencia<-Marea_Data_incidencia %>% select(qD)



MRPP_Marea_incidencia<-vegan::mrpp(dat = Marea_dataprueba_incidencia,  Marea_Data_incidencia$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_incidencia, file = "./Resultados/MRPP_Marea_incidencia.txt")







# Sample-size-based R/E curves, separating plots by "site"

png(filename="./Imagenes/Incidencia_Marea_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot_incidencia, type=1)
dev.off()

png(filename="./Imagenes/Incidencia_Marea_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot_incidencia, type=3)
dev.off()








#Sector

Oceanico_incidencia<-filter(Codigo_fito_incidencia, Sector=="Oceanico")
Costero_incidencia<-filter(Codigo_fito_incidencia, Sector=="Costero")


OceanicoSum_incidencia<-as.data.frame(t(Oceanico[,5:145]%>% summarise_all(sum)))
CosteroSum<-as.data.frame(t(Costero[,5:145]%>% summarise_all(sum)))


OceanicoDiv<-filter(OceanicoSum, V1>0)
CosteroDiv<-filter(CosteroSum, V1>0)


OceanicoVec<-as.vector(OceanicoDiv$V1)
CosteroVec<-as.vector(CosteroDiv$V1)


OceanicoVec<-append(OceanicoVec,18,0)
CosteroVec<-append(CosteroVec,18,0)


Sector<-list(OceanicoVec, CosteroVec)
names(Sector)<-c("Oceanico", "Costero")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Sector_Plot <- iNEXT(Sector, 
                     q=c(0,1,2), 
                     datatype = "abundance",
                     endpoint = 100) # q = 0 es la riqueza (diversidades verdaderas)


Sector_Data<- Sector_Plot[["iNextEst"]][["size_based"]]



dataprueba<-Sector_Data %>% select(qD,
                                   qD.LCL,
                                   qD.UCL,
                                   SC,
                                   SC.LCL,
                                   SC.UCL)




vegan::mrpp(dat = dataprueba,  Sector_Data$Assemblage, permutations = 2000)




colnames(Transecto_Data)


# Sample-size-based R/E curves, separating plots by "site"

png(filename="./Imagenes/Sector_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="./Imagenes/Sector_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="./Imagenes/Sector_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="./Imagenes/Sector_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Both")
dev.off()



Transectos_Plot_Hill<-ggiNEXT(Transectos_Plot, type=1,facet.var = "Assemblage")
Marea_Plot_Hill<-ggiNEXT(Marea_Plot, type=1,facet.var = "Assemblage")
Sector_Plot_Hill<-ggiNEXT(Sector_Plot, type=1,facet.var = "Assemblage")

Transectos_Plot_Coverage<-ggiNEXT(Transectos_Plot, type=3,facet.var = "Assemblage")
Marea_Plot_Coverage<-ggiNEXT(Marea_Plot, type=3,facet.var = "Assemblage")
Sector_Plot_Coverage<-ggiNEXT(Sector_Plot, type=3,facet.var = "Assemblage")


png(filename="./Imagenes/Total_Plot_hill_Plot_Assamblage.png", height =1500 , width = 1000, units = "px")
gridExtra:: grid.arrange(Transectos_Plot_Hill,Marea_Plot_Hill,Sector_Plot_Hill, ncol=1)
dev.off()



png(filename="./Imagenes/Total_Plot_coverage_Plot_Assamblage.png", height =1500 , width = 1000, units = "px")
gridExtra:: grid.arrange(Transectos_Plot_Coverage,Marea_Plot_Coverage,Sector_Plot_Coverage, ncol=1)
dev.off()


