

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

Codigo_fito_abundancia<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Abundancia.csv", sep=",", header = TRUE)

#Evaluación de la composición entre transectos sectores y mareas
MRPP_Transecto_Abundancia<-vegan::mrpp(dat = Codigo_fito_abundancia[,5:145],  Codigo_fito_abundancia$Transecto, permutations = 2000)
MRPP_Marea_Abundancia<-vegan::mrpp(dat = Codigo_fito_abundancia[,5:145],  Codigo_fito_abundancia$Marea, permutations = 2000)
MRPP_Sector_Abundancia<-vegan::mrpp(dat = Codigo_fito_abundancia[,5:145],  Codigo_fito_abundancia$Sector, permutations = 2000)



capture.output(MRPP_Transecto_Abundancia,MRPP_Marea_Abundancia, MRPP_Sector_Abundancia, file = "./Resultados/mrpp_resultados_ComposicionBasadaAbundancia.txt")

MRPP_Sector_Abundancia
#Transectos####

Amarales_abundancia<-filter(Codigo_fito_abundancia, Transecto=="Amarales")
Sanquianga_abundancia<-filter(Codigo_fito_abundancia, Transecto=="Sanquianga")
Guascama_abundancia<-filter(Codigo_fito_abundancia, Transecto=="Guascama")

amaSum_abundancia<-as.data.frame(t(Amarales_abundancia[,6:145]%>% summarise_all(sum)))
sanSum_abundancia<-as.data.frame(t(Sanquianga_abundancia[,6:145]%>% summarise_all(sum)))
guaSum_abundancia<-as.data.frame(t(Guascama_abundancia[,6:145]%>% summarise_all(sum)))

amaDiv_abundancia<-filter(amaSum_abundancia, V1>0)
sanDiv_abundancia<-filter(sanSum_abundancia, V1>0)
guaDiv_abundancia<-filter(guaSum_abundancia, V1>0)

amaVec_abundancia<-as.vector(amaDiv_abundancia$V1)
sanVec_abundancia<-as.vector(sanDiv_abundancia$V1)
guaVec_abundancia<-as.vector(guaDiv_abundancia$V1)


Transectos_abundancia<-list(amaVec_abundancia, sanVec_abundancia, guaVec_abundancia)
names(Transectos_abundancia)<-c("Amarales", "Sanquianga", "Guascama")

#Inext###





# Comando general de iNEXT 
Transectos_Plot_abundancia <- iNEXT(Transectos_abundancia, 
                   q=c(0,1,2), 
                   datatype = "abundance",
                   endpoint = 10000) # q = 0 es la riqueza 

Transectos_Plot_abundancia$DataInfo # showing basic data information.
Transectos_Plot_abundancia$AsyEst # showing asymptotic diversity estimates.

Transecto_Data_abundancia<- Transectos_Plot_abundancia$iNextEst$size_based 



dataprueba_abundancia<-Transecto_Data_abundancia %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)
dataprueba<-Transecto_Data %>% select(qD, qD.UCL)



MRPP_Transecto<-vegan::mrpp(dat = dataprueba,  Transecto_Data$Assemblage, permutations = 2000)




colnames(Transecto_Data)
# Sample-size-based R/E curves, separating plots by "site"

png(filename="./Imagenes/Transectos_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="./Imagenes/Transectos_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="./Imagenes/Transectos_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="./Imagenes/Transectos_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Both")
dev.off()





#Transectos + Mareas ####

AmaralesAlta<-filter(Codigo_fito_abundancia, Transecto=="Amarales" & Marea =="Alta")
SanquiangaAlta<-filter(Codigo_fito_abundancia, Transecto=="Sanquianga" & Marea =="Alta")
GuascamaAlta<-filter(Codigo_fito_abundancia, Transecto=="Guascama" & Marea =="Alta")

amaSumAlta<-as.data.frame(t(AmaralesAlta[,6:145]%>% summarise_all(sum)))
sanSumAlta<-as.data.frame(t(SanquiangaAlta[,6:145]%>% summarise_all(sum)))
guaSumAlta<-as.data.frame(t(GuascamaAlta[,6:145]%>% summarise_all(sum)))



amaAltaDiv<-filter(amaSumAlta, V1>0)
sanAltaDiv<-filter(sanSumAlta, V1>0)
guaAltaDiv<-filter(guaSumAlta, V1>0)

amaVecAlta<-as.vector(amaAltaDiv$V1)
sanVecAlta<-as.vector(sanAltaDiv$V1)
guaVecAlta<-as.vector(guaAltaDiv$V1)

amaVecAlta<-append(amaVecAlta,6,0)
sanVecAlta<-append(sanVecAlta,6,0)
guaVecAlta<-append(guaVecAlta,6,0)

#Bajas

AmaralesBaja<-filter(Codigo_fito_abundancia, Transecto=="Amarales" & Marea =="Baja")
SanquiangaBaja<-filter(Codigo_fito_abundancia, Transecto=="Sanquianga" & Marea =="Baja")
GuascamaBaja<-filter(Codigo_fito_abundancia, Transecto=="Guascama" & Marea =="Baja")

amaSumBaja<-as.data.frame(t(AmaralesBaja[,6:145]%>% summarise_all(sum)))
sanSumBaja<-as.data.frame(t(SanquiangaBaja[,6:145]%>% summarise_all(sum)))
guaSumBaja<-as.data.frame(t(GuascamaBaja[,6:145]%>% summarise_all(sum)))

amaBajaDiv<-filter(amaSumBaja, V1>0)
sanBajaDiv<-filter(sanSumBaja, V1>0)
guaBajaDiv<-filter(guaSumBaja, V1>0)

amaVecBaja<-as_vector(amaBajaDiv$V1)
sanVecBaja<-as_vector(sanBajaDiv$V1)
guaVecBaja<-as_vector(guaBajaDiv$V1)

amaVecBaja<-append(amaVecBaja,6,0)
sanVecBaja<-append(sanVecBaja,6,0)
guaVecBaja<-append(guaVecBaja,6,0)

Transectos<-list(amaVecAlta, sanVecAlta, guaVecAlta, amaVecBaja, sanVecBaja, guaVecBaja)
names(Transectos)<-c("Ama_Alta", "San_Alta","Gua_Alta", "Ama_Baja", "San_Baja","Gua_Baja")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
est.Comms <- iNEXT(Transectos, 
                 q=c(0,1,2), 
                 datatype = "abundance",
                 endpoint = 1000000) # q = 0 es la riqueza (diversidades verdaderas)

# Sample-size-based R/E curves, separating plots by "site"

png(filename="./Imagenes/TransectosMarea_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Assemblage")
dev.off()

png(filename="./Imagenes/TransectosMarea_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Order.q")
dev.off()

png(filename="./Imagenes/TransectosMarea_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Both")
dev.off()








#Marea

Alta<-filter(Codigo_fito_abundancia, Marea=="Alta")
Baja<-filter(Codigo_fito_abundancia, Marea=="Baja")


AltaSum<-as.data.frame(t(Alta[,6:145]%>% summarise_all(sum)))
BajaSum<-as.data.frame(t(Baja[,6:145]%>% summarise_all(sum)))


AltaDiv<-filter(AltaSum, V1>0)
BajaDiv<-filter(BajaSum, V1>0)


AltaVec<-as.vector(AltaDiv$V1)
BajaVec<-as.vector(BajaDiv$V1)


AltaVec<-append(AltaVec,18,0)
BajaVec<-append(BajaVec,18,0)


Marea<-list(AltaVec, BajaVec)
names(Marea)<-c("Alta", "Baja")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Marea_Plot <- iNEXT(Marea, 
                         q=c(0,1,2), 
                         datatype = "abundance",
                         endpoint = 1000000) # q = 0 es la riqueza (diversidades verdaderas)

Marea_Data<- Marea_Plot[["iNextEst"]][["size_based"]]



dataprueba<-Marea_Data %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)




vegan::mrpp(dat = dataprueba,  Marea_Data$Assemblage, permutations = 2000)




colnames(Transecto_Data)






# Sample-size-based R/E curves, separating plots by "site"

png(filename="./Imagenes/Marea_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="./Imagenes/Marea_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="./Imagenes/Marea_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="./Imagenes/Marea_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Both")
dev.off()






#Sector

Oceanico<-filter(Codigo_fito_abundancia, Sector=="Oceanico")
Costero<-filter(Codigo_fito_abundancia, Sector=="Costero")


OceanicoSum<-as.data.frame(t(Oceanico[,6:145]%>% summarise_all(sum)))
CosteroSum<-as.data.frame(t(Costero[,6:145]%>% summarise_all(sum)))


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
                    endpoint = 10000) # q = 0 es la riqueza (diversidades verdaderas)


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


