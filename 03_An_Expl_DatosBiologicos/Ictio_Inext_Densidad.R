if(!require(entropart))install.packages("entropart")
if(!require(iNEXT))install.packages("iNEXT")
if(!require(vegan))install.packages("vegan")
if(!require(dplyr))install.packages("dplyr")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(gridExtra))install.packages("gridExtra")
if(!require(parallel))install.packages("parallel")


install.packages("C:/Users/chris/Downloads/entropart_1.6-11.zip", repos = NULL, type = "source")



Codigo_Ictio<-read.table("./Biologicos/DatosP_Ictioplancton//Data_Ictio.csv", sep=",", header = TRUE)


#Transectos####

Amarales_Densidad<-filter(Codigo_Ictio, Transecto=="Amarales")
Sanquianga_Densidad<-filter(Codigo_Ictio, Transecto=="Sanquianga")
Guascama_Densidad<-filter(Codigo_Ictio, Transecto=="Guascama")

amaSum_Densidad<-as.data.frame(t(Amarales_Densidad[,5:37]%>% summarise_all(sum)))
sanSum_Densidad<-as.data.frame(t(Sanquianga_Densidad[,5:37]%>% summarise_all(sum)))
guaSum_Densidad<-as.data.frame(t(Guascama_Densidad[,5:37]%>% summarise_all(sum)))

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
write.csv(Transecto_Data_Densidad, file = "./Resultados/Ictio_Transecto_Data_Densidad.csv",  row.names = FALSE)

dataprueba_Densidad<-Transecto_Data_Densidad %>% select(qD,
                                                        qD.LCL,
                                                        qD.UCL,
                                                        SC,
                                                        SC.LCL,
                                                        SC.UCL)


MRPP_Transecto_Densidad<-vegan::mrpp(dat = dataprueba_Densidad,  Transecto_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Transecto_Densidad, file="./Resultados/Ictio_MRPP_Transecto_Densidad.txt")



#Marea

Alta_Densidad<-filter(Codigo_Ictio, Marea=="Alta")
Baja_Densidad<-filter(Codigo_Ictio, Marea=="Baja")


AltaSum_Densidad<-as.data.frame(t(Alta_Densidad[,5:37]%>% summarise_all(sum)))
BajaSum_Densidad<-as.data.frame(t(Baja_Densidad[,5:37]%>% summarise_all(sum)))


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
write.csv(Marea_Data_Densidad, file = "./Resultados/Ictio_Marea_Data_Densidad.csv",  row.names = FALSE)


dataprueba_Densidad<-Marea_Data_Densidad %>% select(qD,
                                                    qD.LCL,
                                                    qD.UCL,
                                                    SC,
                                                    SC.LCL,
                                                    SC.UCL)




MRPP_Marea_Densidad<-vegan::mrpp(dat = dataprueba_Densidad,  Marea_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Densidad, file="./Resultados/Ictio_MRPP_Marea_Densidad.txt")



#Sector

Oceanico_Densidad<-filter(Codigo_Ictio, Sector=="Oceanico")
Costero_Densidad<-filter(Codigo_Ictio, Sector=="Costero")


OceanicoSum_Densidad<-as.data.frame(t(Oceanico_Densidad[,5:37]%>% summarise_all(sum)))
CosteroSum_Densidad<-as.data.frame(t(Costero_Densidad[,5:37]%>% summarise_all(sum)))


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
write.csv(Sector_Data_Densidad, file = "./Resultados/Ictio_Sector_Data_Densidad.csv",  row.names = FALSE)


Sector_dataprueba_Densidad<-Marea_Data_Densidad %>% select(qD, qD.LCL, qD.UCL, SC, SC.LCL, SC.UCL)



MRPP_Sector_Densidad<-vegan::mrpp(dat = Sector_dataprueba_Densidad,  Sector_Data_Densidad$Assemblage, permutations = 2000)
capture.output(MRPP_Marea_Densidad, file = "./Resultados/Ictio_MRPP_Sector_Densidad.txt")


Transectos_Plot_Hill_Densidad<-ggiNEXT(Transectos_Plot_Densidad, type=1,facet.var = "Order.q")
Marea_Plot_Hill_Densidad<-ggiNEXT(Marea_Plot_Densidad, type=1,facet.var = "Order.q")
Sector_Plot_Hill_Densidad<-ggiNEXT(Sector_Plot_Densidad, type=1,facet.var = "Order.q")

Transectos_Plot_Coverage_Densidad<-ggiNEXT(Transectos_Plot_Densidad, type=3,facet.var = "Order.q")
Marea_Plot_Coverage_Densidad<-ggiNEXT(Marea_Plot_Densidad, type=3,facet.var = "Order.q")
Sector_Plot_Coverage_Densidad<-ggiNEXT(Sector_Plot_Densidad, type=3,facet.var = "Order.q")


png(filename="./Imagenes/Ictio_01_Densidad_Total_Plot_hill_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Hill_Densidad,Marea_Plot_Hill_Densidad,Sector_Plot_Hill_Densidad, ncol=1)
dev.off()



png(filename="./Imagenes/Ictio_02_Densidad_Total_Plot_coverage_Plot_Assamblage.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Transectos_Plot_Coverage_Densidad,Marea_Plot_Coverage_Densidad,Sector_Plot_Coverage_Densidad, ncol=1)
dev.off()


