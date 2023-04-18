

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


capture.output("MRPP - Transectos",
  MRPP_Transecto_Abundancia,
  "MRPP - Marea",
  MRPP_Marea_Abundancia, 
  "MRPP - Sector",
  MRPP_Sector_Abundancia, 
  file = "./Resultados/mrpp_resultados_ComposicionBasadaAbundancia.txt")

MRPP_Sector_Abundancia



MDS_transectos_df<-Codigo_fito_abundancia[6:145]
row.names(MDS_transectos_df)<-Codigo_fito_abundancia$Estaciones



sol <- metaMDS(MDS_transectos_df)

data.scores <- as.data.frame(scores(sol))
data.scores$site <- rownames(data.scores)

plot(sol, display = "sites",type="n")


ordihull(sol, Management, col=1:4, lwd=3)
ordiellipse(sol, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(sol, Management, col=1:4, draw="polygon")
ordispider(sol, Management, col=1:4, label = TRUE)
points(sol, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

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

Transectos_Plot_abundancia_Data_Info<-Transectos_Plot_abundancia$DataInfo # showing basic data information.
Transectos_Plot_abundancia_AsyEst<-Transectos_Plot_abundancia$AsyEst # showing asymptotic diversity estimates.

capture.output("Datos del analisis de abundancia por Transecto",Transectos_Plot_abundancia_Data_Info, "Estimadores para Q0, Q1, Q2",Transectos_Plot_abundancia_AsyEst, file="./Resultados/Transectos_Abundancia_Resultados.txt")
Transecto_Data_abundancia<- Transectos_Plot_abundancia$iNextEst$size_based 
write.csv(Transecto_Data_abundancia, file = "./Resultados/Transecto_Data_abundancia.csv",  row.names = FALSE)

dataprueba_abundancia<-Transecto_Data_abundancia %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)


MRPP_Transecto_Abundancia<-vegan::mrpp(dat = dataprueba_abundancia,  Transecto_Data_abundancia$Assemblage, permutations = 2000)
capture.output(MRPP_Transecto_Abundancia, file="./Resultados/MRPP_Transecto_Abundancia.txt")



#Marea

Alta_Abundancia<-filter(Codigo_fito_abundancia, Marea=="Alta")
Baja_Abundancia<-filter(Codigo_fito_abundancia, Marea=="Baja")


AltaSum_Abundancia<-as.data.frame(t(Alta_Abundancia[,6:145]%>% summarise_all(sum)))
BajaSum_Abundancia<-as.data.frame(t(Baja_Abundancia[,6:145]%>% summarise_all(sum)))


AltaDiv_Abundancia<-filter(AltaSum_Abundancia, V1>0)
BajaDiv_Abundancia<-filter(BajaSum_Abundancia, V1>0)


AltaVec_Abundancia<-as.vector(AltaDiv_Abundancia$V1)
BajaVec_Abundancia<-as.vector(BajaDiv_Abundancia$V1)


AltaVec_Abundancia<-append(AltaVec_Abundancia,18,0)
BajaVec_Abundancia<-append(BajaVec_Abundancia,18,0)


Marea_Abundancia<-list(AltaVec_Abundancia, BajaVec_Abundancia)
names(Marea_Abundancia)<-c("Alta", "Baja")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Marea_Plot_Abundancia <- iNEXT(Marea_Abundancia, 
                         q=c(0,1,2), 
                         datatype = "abundance",
                         endpoint = 10000) # q = 0 es la riqueza (diversidades verdaderas)



Marea_Plot_Abundancia_Data_Info<-Marea_Plot_Abundancia$DataInfo # showing basic data information.
Marea_Plot_Abundancia_AsyEst<-Marea_Plot_Abundancia$AsyEst # showing asymptotic diversity estimates.
capture.output("Datos del analisis de abundancia por Transecto",Marea_Plot_Abundancia_Data_Info, "Estimadores para Q0, Q1, Q2",Marea_Plot_Abundancia_AsyEst, file="./Resultados/Marea_Abundancia_Resultados.txt")

Marea_Data_Abundancia<- Marea_Plot_Abundancia$iNextEst$size_based 
write.csv(Marea_Data_Abundancia, file = "./Resultados/Marea_Data_Abundancia.csv",  row.names = FALSE)


dataprueba_Abundancia<-Marea_Data_Abundancia %>% select(qD,
                                      qD.LCL,
                                      qD.UCL,
                                      SC,
                                      SC.LCL,
                                      SC.UCL)




MRPP_Marea_Abundancia<-vegan::mrpp(dat = dataprueba_Abundancia,  Marea_Data_Abundancia$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Abundancia, file="./Resultados/MRPP_Marea_Abundancia.txt")



#Sector

Oceanico_Abundancia<-filter(Codigo_fito_abundancia, Sector=="Oceanico")
Costero_Abundancia<-filter(Codigo_fito_abundancia, Sector=="Costero")


OceanicoSum_Abundancia<-as.data.frame(t(Oceanico_Abundancia[,6:145]%>% summarise_all(sum)))
CosteroSum_Abundancia<-as.data.frame(t(Costero_Abundancia[,6:145]%>% summarise_all(sum)))


OceanicoDiv_Abundancia<-filter(OceanicoSum_Abundancia, V1>0)
CosteroDiv_Abundancia<-filter(CosteroSum_Abundancia, V1>0)


OceanicoVec_Abundancia<-as.vector(OceanicoDiv_Abundancia$V1)
CosteroVec_Abundancia<-as.vector(CosteroDiv_Abundancia$V1)


OceanicoVec_Abundancia<-append(OceanicoVec_Abundancia,18,0)
CosteroVec_Abundancia<-append(CosteroVec_Abundancia,18,0)


Sector_Abundancia<-list(OceanicoVec_Abundancia, CosteroVec_Abundancia)
names(Sector_Abundancia)<-c("Oceanico", "Costero")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Sector_Plot_Abundancia <- iNEXT(Sector_Abundancia, 
                    q=c(0,1,2), 
                    datatype = "abundance",
                    endpoint = 10000) # q = 0 es la riqueza (diversidades verdaderas)



Sector_Plot_Abundancia_Data_Info<-Sector_Plot_Abundancia$DataInfo # showing basic data information.
Sector_Plot_Abundancia_AsyEst<-Sector_Plot_Abundancia$AsyEst # showing asymptotic diversity estimates.
capture.output("Datos del analisis de abundancia por Transecto",Sector_Plot_Abundancia_Data_Info, "Estimadores para Q0, Q1, Q2",Sector_Plot_Abundancia_AsyEst, file="./Resultados/Sector_Abundancia_Resultados.txt")

Sector_Data_Abundancia<- Sector_Plot_Abundancia$iNextEst$size_based 
write.csv(Sector_Data_Abundancia, file = "./Resultados/Sector_Data_Abundancia.csv",  row.names = FALSE)


Sector_dataprueba_Abundancia<-Marea_Data_Abundancia %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)



MRPP_Sector_Abundancia<-vegan::mrpp(dat = Sector_dataprueba_Abundancia,  Sector_Data_Abundancia$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Abundancia, file = "./Resultados/MRPP_Sector_Abundancia.txt")


Transectos_Plot_Hill_Abundancia<-ggiNEXT(Transectos_Plot_abundancia, type=1,facet.var = "Order.q")
Marea_Plot_Hill_Abundancia<-ggiNEXT(Marea_Plot_Abundancia, type=1,facet.var = "Order.q")
Sector_Plot_Hill_Abundancia<-ggiNEXT(Sector_Plot_Abundancia, type=1,facet.var = "Order.q")

Transectos_Plot_Coverage_Abundancia<-ggiNEXT(Transectos_Plot_abundancia, type=3,facet.var = "Order.q")
Marea_Plot_Coverage_Abundancia<-ggiNEXT(Marea_Plot_Abundancia, type=3,facet.var = "Order.q")
Sector_Plot_Coverage_Abundancia<-ggiNEXT(Sector_Plot_Abundancia, type=3,facet.var = "Order.q")


png(filename="./Imagenes/01_Abundancia_Total_Plot_hill_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Hill_Abundancia,Marea_Plot_Hill_Abundancia,Sector_Plot_Hill_Abundancia, ncol=1)
dev.off()



png(filename="./Imagenes/02_Abundancia_Total_Plot_coverage_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Coverage_Abundancia,Marea_Plot_Coverage_Abundancia,Sector_Plot_Coverage_Abundancia, ncol=1)
dev.off()


