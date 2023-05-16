library(vegan)
library(ggplot2)
library(dplyr)
library(spdep)

FitoData<-read.table("./Resultados/Fito_Diversidad_Estaciones.csv", sep=",", header = TRUE)
IctioData<-read.table("./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", header = TRUE)
Datos_Totales_Limpios<-read.table( "../02_An_Expl_DatosQuimicos/01_Datos_Quimicos/Datos_Totales_CCCP.csv", header =  TRUE, sep = ",")

coordenadas_fuente<-read.table("../Sig_Datos/loglat_Estaciones.csv", header = TRUE, sep = ",")
FitoData<-read.table("./Resultados/Fito_Diversidad_Estaciones.csv", sep=",", header = TRUE)
IctioData<-read.table("./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", header = TRUE)


colnames(FitoData)<-c(
  "Estaciones",  "Transecto",   "Sector",      "Marea",       "Fito_S",           "Fito_Simpson",     "Fito_Shannon",    
  "Fito_Pielou",      "Fito_q0",          "Fito_q1",          "Fito_q2",          "Fito_Densidad",    "Clorofila",   "No.Estacion"
)
colnames(IctioData)<-c(
  "Estaciones",  "Transecto",   "Sector",      "Marea",       "Ictio_S",           "Ictio_Simpson",     "Ictio_Shannon",    
  "Ictio_Pielou",      "Ictio_q0",          "Ictio_q1",          "Ictio_q2",          "Ictio_Densidad"
)

dataTotal<-cbind(FitoData,IctioData[,5:12], Datos_Totales_Limpios[,12:31])

dataTotal_alta<-dataTotal%>% filter(Marea=="Alta")
dataTotal_baja<-dataTotal%>% filter(Marea=="Baja")



loc_matrix <- as.matrix(coordenadas_fuente[, c('longitud','latitud')])

kn <- knearneigh(loc_matrix, 5)
nb <- knn2nb(kn)

listw <- nb2listw(nb)
if(!require(spatialreg))install.packages("spatialreg")

dataTotal_alta

#El método usado para hacer este modelo es: Bivand & Piras (2015) https://www.jstatsoft.org/v63/i18/.







q0_Alta <- spatialreg::lagsarlm(Ictio_q0 ~ Fito_q0,
                                      data = dataTotal_alta,
                                      listw = listw)
q0_Baja <- spatialreg::lagsarlm(Ictio_q0 ~ Fito_q0,
                                      data = dataTotal_baja,
                                      listw = listw)
q1_Alta <- spatialreg::lagsarlm(Ictio_q1 ~ Fito_q1,
                                data = dataTotal_alta,
                                listw = listw)
q1_Baja <- spatialreg::lagsarlm(Ictio_q1 ~ Fito_q1,
                                data = dataTotal_baja,
                                listw = listw)
q2_Alta <- spatialreg::lagsarlm(Ictio_q2 ~ Fito_q2,
                                data = dataTotal_alta,
                                listw = listw)
q2_Baja <- spatialreg::lagsarlm(Ictio_q2 ~ Fito_q2,
                                data = dataTotal_baja,
                                listw = listw)

densidad_Alta <- spatialreg::lagsarlm(Ictio_Densidad ~ Fito_Densidad,
                                      data = dataTotal_alta,
                                      listw = listw)
densidad_Baja <- spatialreg::lagsarlm(Ictio_Densidad ~ Fito_Densidad,
                                      data = dataTotal_baja,
                                      listw = listw)



clorofila_Alta <- spatialreg::lagsarlm(Ictio_Densidad ~ Clorofila,
                                      data = dataTotal_alta,
                                      listw = listw)
clorofila_Baja <- spatialreg::lagsarlm(Ictio_Densidad ~ Clorofila,
                                       data = dataTotal_baja,
                                       listw = listw)

lagsarlm_q0_Alta<-summary(q0_Alta, correlation=TRUE)
lagsarlm_q0_Baja<-summary(q0_Baja, correlation=TRUE)
lagsarlm_q1_Alta<-summary(q1_Alta, correlation=TRUE)
lagsarlm_q1_Baja<-summary(q1_Baja, correlation=TRUE)
lagsarlm_q2_Alta<-summary(q2_Alta, correlation=TRUE)
lagsarlm_q2_Baja<-summary(q2_Baja, correlation=TRUE)
lagsarlm_densidad_Alta<-summary(densidad_Alta, correlation=TRUE)
lagsarlm_densidad_Baja<-summary(densidad_Baja, correlation=TRUE)
lagsarlm_clorofila_Alta<-summary(clorofila_Alta, correlation=TRUE)
lagsarlm_clorofila_Baja<-summary(clorofila_Baja, correlation=TRUE)



capture.output("####q0",
               lagsarlm_q0_Alta,
               lagsarlm_q0_Baja,
               "####q1",
               lagsarlm_q1_Alta,
               lagsarlm_q1_Baja,
               "####q2",
               lagsarlm_q2_Alta,
               lagsarlm_q2_Baja,
               "####densidad",
               lagsarlm_densidad_Alta,
               lagsarlm_densidad_Baja,
               "####Clorofila",
               lagsarlm_clorofila_Alta,
               lagsarlm_clorofila_Baja,
               file="SAR_Congruencias.txt"
  
)



Ictio_q1_qui_Alta <- spatialreg::lagsarlm(Fito_q1 ~ OD+
                                            Transparencia+
                                            Salinidad_median+
                                            Temperatura_IQR,
                                data = dataTotal_alta,
                                listw = listw)
Ictio_q1_qui_Baja <- spatialreg::lagsarlm(Fito_q1 ~ OD+
                                            Transparencia+
                                            Salinidad_median+
                                            Temperatura_IQR,
                                          data = dataTotal_baja,
                                          listw = listw)


summary(Ictio_q1_qui_Alta, correlation=TRUE)
summary(Ictio_q1_qui_Baja, correlation=TRUE)






cor.test(dataTotal_alta$Fito_q0,dataTotal_alta$Ictio_q0, method = "spearman")
cor.test(dataTotal_alta$Fito_q1,dataTotal_alta$Ictio_q1, method = "spearman")
cor.test(dataTotal_alta$Fito_q2,dataTotal_alta$Ictio_q2, method = "spearman")
cor.test(dataTotal_alta$Fito_Densidad,dataTotal_alta$Ictio_Densidad, method = "spearman")
cor.test(dataTotal_alta$Clorofila,dataTotal_alta$Fito_Densidad, method = "spearman")
cor.test(dataTotal_alta$Clorofila,dataTotal_alta$Ictio_Densidad, method = "spearman")

cor.test(dataTotal_baja$Fito_q0,dataTotal_baja$Ictio_q0, method = "spearman")
cor.test(dataTotal_baja$Fito_q1,dataTotal_baja$Ictio_q1, method = "spearman")
cor.test(dataTotal_baja$Fito_q2,dataTotal_baja$Ictio_q2, method = "spearman")
cor.test(dataTotal_baja$Fito_Densidad,dataTotal_baja$Ictio_Densidad, method = "spearman")
cor.test(dataTotal_baja$Clorofila,dataTotal_baja$Fito_Densidad, method = "spearman")
cor.test(dataTotal_baja$Clorofila,dataTotal_baja$Ictio_Densidad, method = "spearman")

Congruence_q0_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_q0, y=dataTotal_alta$Ictio_q0), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_q0, y=dataTotal_baja$Ictio_q0), colour="blue")+
  xlab(expression(paste("Fitoplancton ("^0,"D)")))+
  ylab(expression(paste("Ictioplancton ("^0,"D)")))+
  labs(title = "(a)",
        subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
  theme_bw()

Congruence_q1_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_q1, y=dataTotal_alta$Ictio_q1), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_q1, y=dataTotal_baja$Ictio_q1), colour="blue")+
  xlab(expression(paste("Fitoplancton ("^1,"D)")))+
  ylab(expression(paste("Ictioplancton ("^1,"D)")))+
  labs(title = "(b)",
       subtitle = expression("SAR Alta"~ rho~"= -0.74 Baja:"~ rho~"= 0.38"))+
  theme_bw()

Congruence_q2_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_q2, y=dataTotal_alta$Ictio_q2), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_q2, y=dataTotal_baja$Ictio_q2), colour="blue")+
  xlab(expression(paste("Fitoplancton ("^2,"D)")))+
  ylab(expression(paste("Ictioplancton ("^2,"D)")))+
  labs(title = "(c)",
      
  subtitle = expression("SAR Alta"~ rho~"= -0.37 Baja:"~ rho~"= 0.21"))+
  
  theme_bw()

Congruence_den_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_Densidad, y=dataTotal_alta$Ictio_Densidad), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_Densidad, y=dataTotal_baja$Ictio_Densidad), colour="blue")+
  xlab(expression(paste("Densidad de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(d)",
       
  subtitle = expression("SAR Alta"~ rho~"= -0.22 Baja:"~ rho~"= 0.22"))+
  theme_bw()

Congruence_clor_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Clorofila, y=dataTotal_alta$Ictio_Densidad), colour="red")+
  geom_point(aes(x=dataTotal_baja$Clorofila, y=dataTotal_baja$Ictio_Densidad), colour="blue")+
  xlab(expression(paste("Clorofila [",mu,"g.L"^-1,"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(e)",
       
  subtitle = expression("SAR Alta"~ rho~"= 0.21 Baja:"~ rho~"= 0.31"))+
  theme_bw()





Codigo_Ictio<-read.table("./Biologicos/DatosP_Ictioplancton//Data_Ictio.csv", sep=",", header = TRUE)

Div_Code_Ictio<-Codigo_Ictio[,5:37]
row.names(Div_Code_Ictio) <- Codigo_Ictio[,1]


Ictio_groups_df<-Codigo_Ictio[2:4]
row.names(Ictio_groups_df)<-Codigo_Ictio$Especie
Ictio_groups_df$Transecto<-as.factor(Ictio_groups_df$Transecto)



Ictio_Densidad_Relativa <-         
  vegan::decostand(Div_Code_Ictio, method = "total")
# Calculate distance matrix
Ictio_Densidad_Relativa_distmat <- 
  vegdist(Ictio_Densidad_Relativa, method = "bray")

Ictio_Densidad_Relativa_distmat <- 
  as.matrix(Ictio_Densidad_Relativa_distmat, labels = T)


# Running NMDS in vegan (metaMDS)
Ictio_Densidad_Relativa_NMS <-
  metaMDS(Ictio_Densidad_Relativa_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

Ictio_coordenadas <- as.data.frame(scores(Ictio_Densidad_Relativa_NMS[["points"]]))



Codigo_fito_Densidad<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Densidad.csv", sep=",", header = TRUE)

Fito_Densidad_df<-Codigo_fito_Densidad[6:145]
row.names(Fito_Densidad_df)<-Codigo_fito_Densidad$Estaciones


Fito_groups_df<-Codigo_fito_Densidad[2:4]
row.names(Fito_groups_df)<-Codigo_fito_Densidad$Estaciones
Fito_groups_df$Transecto<-as.factor(Fito_groups_df$Transecto)



Fito_Densidad_Relativa <-         
  vegan::decostand(Fito_Densidad_df, method = "total")

Fito_Densidad_Relativa_distmat <- 
  vegdist(Fito_Densidad_Relativa, method = "bray")

Fito_Densidad_Relativa_distmat <- 
  as.matrix(Fito_Densidad_Relativa_distmat, labels = T)



# Running NMDS in vegan (metaMDS)
Fito_Densidad_Relativa_NMS <-
  metaMDS(Fito_Densidad_Relativa_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

Fito_coordenadas <- as.data.frame(scores(Fito_Densidad_Relativa_NMS[["points"]]))

#####Cogruencia en la composición

Fito_nmds1<-Fito_coordenadas$MDS1
Ictio_nmds1<-Ictio_coordenadas$MDS1



cor.test(Fito_nmds1,Ictio_nmds1, method = "spearman")
Congruence_nmds_plot<-ggplot() + 
  geom_point(aes(x=Fito_nmds1, y=Ictio_nmds1))+
  xlab(expression(paste("NMDS1 (Fitoplancton)")))+
  ylab(expression(paste("NMDS1 (Ictioplancton)")))+
  labs(title = "(f)",
       subtitle = "Cor. spearman = 0.06")+
  theme_bw()





png(filename="./Imagenes/Congruence_Taxa_plot.png", height =25 , width = 20, units = "cm", res=400)
gridExtra:: grid.arrange(Congruence_q0_plot,
                         Congruence_den_plot,
                         Congruence_q1_plot,
                         Congruence_clor_plot,
                         Congruence_q2_plot,
                         Congruence_nmds_plot,
                         ncol=2)
dev.off()




#Congruencia con el medio ambiente


Diversidad_q0_MA_Icito<-spatialreg::lagsarlm(Ictio_q0 ~
                                OD+
                                Temperatura_Sup+
                                Salinidad_Sup+
                                Salinidad_IQR+
                                Oxigeno_median  
                               
                                 ,
                              data = dataTotal_alta,
                              listw = listw)     
q0_MA_Icito_LagSAR<-summary(Diversidad_q0_MA_Icito)


Diversidad_q0_MB_Icito<-spatialreg::lagsarlm(Ictio_q0 ~
                                               OD+
                                               Temperatura_Sup+
                                               Salinidad_Sup+
                                               Salinidad_IQR+
                                               Oxigeno_median  
                                             
                                             ,
                                             data = dataTotal_baja,
                                             listw = listw)     
q0_MB_Icito_LagSAR<-summary(Diversidad_q0_MB_Icito)

Diversidad_q0__MA_Fito<-spatialreg::lagsarlm(Fito_q0 ~
                                            OD+
                                            Temperatura_Sup+
                                            Salinidad_Sup+
                                            Salinidad_IQR+
                                            Oxigeno_median  
                                          
                                          ,
                                          data = dataTotal_alta,
                                          listw = listw)     
q0_MA_Fito_LagSAR<-summary(Diversidad_q0__MA_Fito)

Diversidad_q0__Mb_Fito<-spatialreg::lagsarlm(Fito_q0 ~
                                               OD+
                                               Temperatura_Sup+
                                               Salinidad_Sup+
                                               Salinidad_IQR+
                                               Oxigeno_median  
                                             
                                             ,
                                             data = dataTotal_baja,
                                             listw = listw)     
q0_MB_Fito_LagSAR<-summary(Diversidad_q0__Mb_Fito)


capture.output("####Modelos para explicar la diversidad de ictio",
               q0_MA_Icito_LagSAR,
               q0_MB_Icito_LagSAR,
               "###Modelo para explicar la diversidad de fito",
               q0_MA_Fito_LagSAR,
               q0_MB_Fito_LagSAR,
               file ="Modelos_SAR.txt"
  
)
