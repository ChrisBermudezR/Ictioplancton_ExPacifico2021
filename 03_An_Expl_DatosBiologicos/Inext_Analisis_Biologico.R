

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
library(tidyr)
library(gridExtra)

data(BCI)
DATA <- BCI


Codigo_fito<-read.table("./Biologicos/DatosP_Fitoplancton/Matriz_Incidencia_Fito.csv", sep=",", header = TRUE,)

#Transectos####

Amarales<-filter(Codigo_fito, Transecto=="Amarales")
Sanquianga<-filter(Codigo_fito, Transecto=="Sanquianga")
Guascama<-filter(Codigo_fito, Transecto=="Guascama")

amaSum<-as.data.frame(t(Amarales[,6:191]%>% summarise_all(sum)))
sanSum<-as.data.frame(t(Sanquianga[,6:191]%>% summarise_all(sum)))
guaSum<-as.data.frame(t(Guascama[,6:191]%>% summarise_all(sum)))

amaDiv<-filter(amaSum, V1>0)
sanDiv<-filter(sanSum, V1>0)
guaDiv<-filter(guaSum, V1>0)

amaVec<-as_vector(amaDiv$V1)
sanVec<-as_vector(sanDiv$V1)
guaVec<-as_vector(guaDiv$V1)

amaVec<-append(amaVec,12,0)
sanVec<-append(sanVec,12,0)
guaVec<-append(guaVec,12,0)

Transectos<-list(amaVec, sanVec, guaVec)
names(Transectos)<-c("Amarales", "Sanquianga", "Guascama")

#Inext###

# Comando general de iNEXT (calcula muchas cosas)
Transectos_Plot <- iNEXT(Transectos, 
                   q=c(0,1,2), 
                   datatype = "incidence_freq",
                   endpoint = 20) # q = 0 es la riqueza (diversidades verdaderas)

# Sample-size-based R/E curves, separating plots by "site"

png(filename="Transectos_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="Transectos_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="Transectos_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="Transectos_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Transectos_Plot, type=1,facet.var = "Both")
dev.off()





#Transectos + Mareas ####

AmaralesAlta<-filter(Codigo_fito, Transecto=="Amarales" & Marea =="Alta")
SanquiangaAlta<-filter(Codigo_fito, Transecto=="Sanquianga" & Marea =="Alta")
GuascamaAlta<-filter(Codigo_fito, Transecto=="Guascama" & Marea =="Alta")

amaSumAlta<-as.data.frame(t(AmaralesAlta[,6:191]%>% summarise_all(sum)))
sanSumAlta<-as.data.frame(t(SanquiangaAlta[,6:191]%>% summarise_all(sum)))
guaSumAlta<-as.data.frame(t(GuascamaAlta[,6:191]%>% summarise_all(sum)))



amaAltaDiv<-filter(amaSumAlta, V1>0)
sanAltaDiv<-filter(sanSumAlta, V1>0)
guaAltaDiv<-filter(guaSumAlta, V1>0)

amaVecAlta<-as_vector(amaAltaDiv$V1)
sanVecAlta<-as_vector(sanAltaDiv$V1)
guaVecAlta<-as_vector(guaAltaDiv$V1)

amaVecAlta<-append(amaVecAlta,6,0)
sanVecAlta<-append(sanVecAlta,6,0)
guaVecAlta<-append(guaVecAlta,6,0)

#Bajas

AmaralesBaja<-filter(Codigo_fito, Transecto=="Amarales" & Marea =="Baja")
SanquiangaBaja<-filter(Codigo_fito, Transecto=="Sanquianga" & Marea =="Baja")
GuascamaBaja<-filter(Codigo_fito, Transecto=="Guascama" & Marea =="Baja")

amaSumBaja<-as.data.frame(t(AmaralesBaja[,6:191]%>% summarise_all(sum)))
sanSumBaja<-as.data.frame(t(SanquiangaBaja[,6:191]%>% summarise_all(sum)))
guaSumBaja<-as.data.frame(t(GuascamaBaja[,6:191]%>% summarise_all(sum)))

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
                 datatype = "incidence_freq",
                 endpoint = 20) # q = 0 es la riqueza (diversidades verdaderas)

# Sample-size-based R/E curves, separating plots by "site"

png(filename="hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Assemblage")
dev.off()

png(filename="hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Order.q")
dev.off()

png(filename="hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(est.Comms, type=1,facet.var = "Both")
dev.off()








#Marea

Alta<-filter(Codigo_fito, Marea=="Alta")
Baja<-filter(Codigo_fito, Marea=="Baja")


AltaSum<-as.data.frame(t(Alta[,6:191]%>% summarise_all(sum)))
BajaSum<-as.data.frame(t(Baja[,6:191]%>% summarise_all(sum)))


AltaDiv<-filter(AltaSum, V1>0)
BajaDiv<-filter(BajaSum, V1>0)


AltaVec<-as_vector(AltaDiv$V1)
BajaVec<-as_vector(BajaDiv$V1)


AltaVec<-append(AltaVec,18,0)
BajaVec<-append(BajaVec,18,0)


Marea<-list(AltaVec, BajaVec)
names(Marea)<-c("Alta", "Baja")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Marea_Plot <- iNEXT(Marea, 
                         q=c(0,1,2), 
                         datatype = "incidence_freq",
                         endpoint = 20) # q = 0 es la riqueza (diversidades verdaderas)

# Sample-size-based R/E curves, separating plots by "site"

png(filename="Marea_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="Marea_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="Marea_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="Marea_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Marea_Plot, type=1,facet.var = "Both")
dev.off()






#Sector

Oceanico<-filter(Codigo_fito, Sector=="Oceanico")
Costero<-filter(Codigo_fito, Sector=="Costero")


OceanicoSum<-as.data.frame(t(Oceanico[,6:191]%>% summarise_all(sum)))
CosteroSum<-as.data.frame(t(Costero[,6:191]%>% summarise_all(sum)))


OceanicoDiv<-filter(OceanicoSum, V1>0)
CosteroDiv<-filter(CosteroSum, V1>0)


OceanicoVec<-as_vector(OceanicoDiv$V1)
CosteroVec<-as_vector(CosteroDiv$V1)


OceanicoVec<-append(OceanicoVec,18,0)
CosteroVec<-append(CosteroVec,18,0)


Sector<-list(OceanicoVec, CosteroVec)
names(Sector)<-c("Oceanico", "Costero")

#Inext####

# Comando general de iNEXT (calcula muchas cosas)
Sector_Plot <- iNEXT(Sector, 
                    q=c(0,1,2), 
                    datatype = "incidence_freq",
                    endpoint = 20) # q = 0 es la riqueza (diversidades verdaderas)

# Sample-size-based R/E curves, separating plots by "site"

png(filename="Sector_Plot_hill_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Assemblage")
dev.off()

png(filename="Sector_Plot_coverage_Plot_Assamblage.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=3,facet.var = "Assemblage")
dev.off()



png(filename="Sector_Plot_hill_Plot_Order.q.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Order.q")
dev.off()

png(filename="Sector_Plot_hill_Plot_both.png", height =1000 , width = 1500, units = "px")
ggiNEXT(Sector_Plot, type=1,facet.var = "Both")
dev.off()



Transectos_Plot_Hill<-ggiNEXT(Transectos_Plot, type=1,facet.var = "Assemblage")
Marea_Plot_Hill<-ggiNEXT(Marea_Plot, type=1,facet.var = "Assemblage")
Sector_Plot_Hill<-ggiNEXT(Sector_Plot, type=1,facet.var = "Assemblage")

Transectos_Plot_Coverage<-ggiNEXT(Transectos_Plot, type=3,facet.var = "Assemblage")
Marea_Plot_Coverage<-ggiNEXT(Marea_Plot, type=3,facet.var = "Assemblage")
Sector_Plot_Coverage<-ggiNEXT(Sector_Plot, type=3,facet.var = "Assemblage")


png(filename="Total_Plot_hill_Plot_Assamblage.png", height =1500 , width = 1000, units = "px")
gridExtra:: grid.arrange(Transectos_Plot_Hill,Marea_Plot_Hill,Sector_Plot_Hill, ncol=1)
dev.off()



png(filename="Total_Plot_coverage_Plot_Assamblage.png", height =1500 , width = 1000, units = "px")
gridExtra:: grid.arrange(Transectos_Plot_Coverage,Marea_Plot_Coverage,Sector_Plot_Coverage, ncol=1)
dev.off()


#