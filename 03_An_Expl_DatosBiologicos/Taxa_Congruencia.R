library(vegan)
library(ggplot2)
library(dplyr)
library(spdep)
if(!require(spdep))install.packages("spdep")
if(!require(spatialreg))install.packages("spatialreg")

FitoData<-read.table("./Resultados/Fito_Diversidad_Estaciones.csv", sep=",", header = TRUE)
IctioData<-read.table("./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", header = TRUE)
Datos_Totales_Limpios<-read.table( "../02_An_Expl_DatosQuimicos/01_Datos_Quimicos/Datos_Totales_CCCP.csv", header =  TRUE, sep = ",")


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

dataTotal<-cbind(FitoData,IctioData[,5:12], Datos_Totales_Limpios[,12:31], coordenadas_fuente[,2:3])
write.table(dataTotal, "dataTotal.csv", col.names =TRUE, sep =",")


dataTotal_alta<-dataTotal%>% filter(Marea=="Alta")
dataTotal_baja<-dataTotal%>% filter(Marea=="Baja")
coordenadas_fuente<-Datos_Totales_Limpios%>% filter(Marea=="Baja")
coordenadas_fuente<-coordenadas_fuente[, c('longitud','latitud')]

loc_matrix <- as.matrix(coordenadas_fuente)

kn <- knearneigh(loc_matrix, 5)
nb <- knn2nb(kn)

listw <- nb2listw(nb)



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

Fito_Densidad_modelo <- spatialreg::lagsarlm(Fito_Densidad ~ Salinidad_Sup+OD+PO4,
                                       data = dataTotal_baja,
                                       listw = listw)

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
        #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
  subtitle = expression("Alta: r"[M]~"= -0.11 Baja: r"[M]~"= -0.05"))+
  theme_bw()

Congruence_q1_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_q1, y=dataTotal_alta$Ictio_q1), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_q1, y=dataTotal_baja$Ictio_q1), colour="blue")+
  xlab(expression(paste("Fitoplancton ("^1,"D)")))+
  ylab(expression(paste("Ictioplancton ("^1,"D)")))+
  labs(title = "(b)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.16 Baja: r"[M]~"= 0.02"))+
  theme_bw()

Congruence_q2_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_q2, y=dataTotal_alta$Ictio_q2), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_q2, y=dataTotal_baja$Ictio_q2), colour="blue")+
  xlab(expression(paste("Fitoplancton ("^2,"D)")))+
  ylab(expression(paste("Ictioplancton ("^2,"D)")))+
  labs(title = "(c)",
      
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.2 Baja: r"[M]~"= -0.03"))+
  
  theme_bw()

Congruence_den_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Fito_Densidad, y=dataTotal_alta$Ictio_Densidad), colour="red")+
  geom_point(aes(x=dataTotal_baja$Fito_Densidad, y=dataTotal_baja$Ictio_Densidad), colour="blue")+
  xlab(expression(paste("Densidad de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(d)",
       
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= 0.16 Baja: r"[M]~"= 0.01"))+
  theme_bw()

Congruence_clor_plot<-ggplot() + 
  geom_point(aes(x=dataTotal_alta$Clorofila, y=dataTotal_alta$Ictio_Densidad), colour="red")+
  geom_point(aes(x=dataTotal_baja$Clorofila, y=dataTotal_baja$Ictio_Densidad), colour="blue")+
  xlab(expression(paste("Clorofila [",mu,"g.L"^-1,"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(e)",
       
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.07 Baja: r"[M]~"= 0.23*"))+
  theme_bw()








#####

Codigo_Ictio<-read.table("./Biologicos/DatosP_Ictioplancton//Data_Ictio.csv", sep=",", header = TRUE)


FitoData


Codigo_Ictio<-cbind(Codigo_Ictio, FitoData$Marea)

Codigo_Ictio_alta<-Codigo_Ictio%>% filter(FitoData$Marea=="Alta")
Codigo_Ictio_baja<-Codigo_Ictio%>% filter(FitoData$Marea=="Baja")


Div_Code_Ictio<-Codigo_Ictio_alta[,5:37]
row.names(Div_Code_Ictio) <- Codigo_Ictio_alta[,1]


Ictio_groups_Alta__df<-Codigo_Ictio_alta[2:4]
row.names(Ictio_groups_Alta__df)<-Codigo_Ictio_alta$Especie
Ictio_groups_Alta__df$Transecto<-as.factor(Ictio_groups_Alta__df$Transecto)



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
                                  Fito_q0+
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
                                               Fito_q0+
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






#################Mantel





matriz_distancias <- dist(coordenadas_fuente)

write.table(coordenadas_fuente,"../Sig_Datos/loglat_Estaciones.csv", col.names =  TRUE, sep = ",")

Ictio_q0_MA_matrix <- vegdist(dataTotal_alta$Ictio_q0, method = "bray")
Ictio_q0_MB_matrix <- vegdist(dataTotal_baja$Ictio_q0, method = "bray")

Fito_q0_MA_matrix <- vegdist(dataTotal_alta$Fito_q0, method = "bray")
Fito_q0_MB_matrix <- vegdist(dataTotal_baja$Fito_q0, method = "bray")



Fito_q0_MA_matrixvsIctio_q0_MA_matrix <- mantel.partial(Ictio_q0_MA_matrix, Fito_q0_MA_matrix, matriz_distancias, permutations = 999,method = "spearman")
Fito_q0_MA_matrixvsIctio_q0_MA_matrix
Ictio_q0_MB_matrixvsFito_q0_MB_matrix <- mantel.partial(Ictio_q0_MB_matrix, Fito_q0_MB_matrix, matriz_distancias, permutations = 999,method = "spearman")
Ictio_q0_MB_matrixvsFito_q0_MB_matrix


Ictio_q1_MA_matrix <- vegdist(dataTotal_alta$Ictio_q1, method = "bray")
Ictio_q1_MB_matrix <- vegdist(dataTotal_baja$Ictio_q1, method = "bray")

Fito_q1_MA_matrix <- vegdist(dataTotal_alta$Fito_q1, method = "bray")
Fito_q1_MB_matrix <- vegdist(dataTotal_baja$Fito_q1, method = "bray")



Fito_q1_MA_matrixvsIctio_q1_MA_matrix <- mantel.partial(Ictio_q1_MA_matrix, Fito_q1_MA_matrix, matriz_distancias, permutations = 999,method = "spearman")
Fito_q1_MA_matrixvsIctio_q1_MA_matrix
Ictio_q1_MB_matrixvsFito_q1_MB_matrix <- mantel.partial(Ictio_q1_MB_matrix, Fito_q1_MB_matrix, matriz_distancias, permutations = 999,method = "spearman")
Ictio_q1_MB_matrixvsFito_q1_MB_matrix





Ictio_q2_MA_matrix <- vegdist(dataTotal_alta$Ictio_q2, method = "bray")
Ictio_q2_MB_matrix <- vegdist(dataTotal_baja$Ictio_q2, method = "bray")

Fito_q2_MA_matrix <- vegdist(dataTotal_alta$Fito_q2, method = "bray")
Fito_q2_MB_matrix <- vegdist(dataTotal_baja$Fito_q2, method = "bray")



Fito_q2_MA_matrixvsIctio_q2_MA_matrix <- mantel.partial(Ictio_q2_MA_matrix, Fito_q2_MA_matrix, matriz_distancias, permutations = 999,method = "spearman")
Fito_q2_MA_matrixvsIctio_q2_MA_matrix
Ictio_q2_MB_matrixvsFito_q2_MB_matrix <- mantel.partial(Ictio_q2_MB_matrix, Fito_q2_MB_matrix, matriz_distancias, permutations = 999,method = "spearman")
Ictio_q2_MB_matrixvsFito_q2_MB_matrix




Ictio_Densidad_MA_matrix <- vegdist(dataTotal_alta$Ictio_Densidad, method = "bray")
Ictio_Densidad_MB_matrix <- vegdist(dataTotal_baja$Ictio_Densidad, method = "bray")

Fito_Densidad_MA_matrix <- vegdist(dataTotal_alta$Fito_Densidad, method = "bray")
Fito_Densidad_MB_matrix <- vegdist(dataTotal_baja$Fito_Densidad, method = "bray")



Fito_Densidad_MA_matrixvsIctio_Densidad_MA_matrix <- mantel.partial(Ictio_Densidad_MA_matrix, Fito_Densidad_MA_matrix, matriz_distancias, permutations = 999,method = "spearman")
Fito_Densidad_MA_matrixvsIctio_Densidad_MA_matrix
Ictio_Densidad_MB_matrixvsFito_Densidad_MB_matrix <- mantel.partial(Ictio_Densidad_MB_matrix, Fito_Densidad_MB_matrix, matriz_distancias, permutations = 999,method = "spearman")
Ictio_Densidad_MB_matrixvsFito_Densidad_MB_matrix





Ictio_Densidad_MA_matrix <- vegdist(dataTotal_alta$Ictio_Densidad, method = "bray")
Ictio_Densidad_MB_matrix <- vegdist(dataTotal_baja$Ictio_Densidad, method = "bray")

Fito_Clorofila_MA_matrix <- vegdist(dataTotal_alta$Clorofila, method = "bray")
Fito_Clorofila_MB_matrix <- vegdist(dataTotal_baja$Clorofila, method = "bray")



Fito_Clorofila_MA_matrixvsIctio_Densidad_MA_matrix <- mantel.partial(Ictio_Densidad_MA_matrix, Fito_Clorofila_MA_matrix, matriz_distancias, permutations = 999,method = "spearman")
Fito_Clorofila_MA_matrixvsIctio_Densidad_MA_matrix
Ictio_Densidad_MB_matrixvsFito_Clorofila_MB_matrix <- mantel.partial(Ictio_Densidad_MB_matrix, Fito_Clorofila_MB_matrix, matriz_distancias, permutations = 999,method = "spearman")
Ictio_Densidad_MB_matrixvsFito_Clorofila_MB_matrix


plot(Ictio_Densidad_MB_matrix, Fito_Clorofila_MB_matrix)



Ictio_q0_MA_matrix

Congruence_q0_plot<-ggplot() + 
  geom_point(aes(x=Fito_q0_MA_matrix, y=Ictio_q0_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_q0_MB_matrix, y=Ictio_q0_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Fitoplancton ("^0,"D)")))+
  ylab(expression(paste("Ictioplancton ("^0,"D)")))+
  labs(title = "(a)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.11 Baja: r"[M]~"= -0.05"))+
  theme_bw()

Congruence_q1_plot<-ggplot() + 
  geom_point(aes(x=Fito_q1_MA_matrix, y=Ictio_q1_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_q1_MB_matrix, y=Ictio_q1_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Fitoplancton ("^1,"D)")))+
  ylab(expression(paste("Ictioplancton ("^1,"D)")))+
  labs(title = "(b)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.16 Baja: r"[M]~"= 0.02"))+
  theme_bw()

Congruence_q2_plot<-ggplot() + 
  geom_point(aes(x=Fito_q2_MA_matrix, y=Ictio_q2_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_q2_MA_matrix, y=Ictio_q2_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Fitoplancton ("^2,"D)")))+
  ylab(expression(paste("Ictioplancton ("^2,"D)")))+
  labs(title = "(c)",
       
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.2 Baja: r"[M]~"= -0.03"))+
  
  theme_bw()

Congruence_den_plot<-ggplot() + 
  geom_point(aes(x=Fito_Densidad_MA_matrix, y=Ictio_Densidad_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=Ictio_Densidad_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Densidad de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(d)",
       
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= 0.16 Baja: r"[M]~"= 0.01"))+
  theme_bw()

Congruence_clor_plot<-ggplot() + 
  geom_point(aes(x=Fito_Clorofila_MA_matrix, y=Ictio_Densidad_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Clorofila_MB_matrix, y=Ictio_Densidad_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Clorofila [",mu,"g.L"^-1,"]")))+
  ylab(expression(paste("Densidad de Ictioplancton ["~ Ind.1000m^-3~"]")))+
  labs(title = "(e)",
       
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= -0.07 Baja: r"[M]~"= 0.23*"))+
  theme_bw()

####Relaciones con el ambiente####

Fito_q0_MA <- vegdist(dataTotal_alta$Fito_q0, method = "bray")
Fito_q0_MB <- vegdist(dataTotal_baja$Fito_q0, method = "bray")

Fito_q1_MA <- vegdist(dataTotal_alta$Fito_q1, method = "bray")
Fito_q1_MB <- vegdist(dataTotal_baja$Fito_q1, method = "bray")

Fito_q2_MA <- vegdist(dataTotal_alta$Fito_q2, method = "bray")
Fito_q2_MB <- vegdist(dataTotal_baja$Fito_q2, method = "bray")

Fito_Densidad_MA <- vegdist(dataTotal_alta$Fito_Densidad, method = "bray")
Fito_Densidad_MB <- vegdist(dataTotal_baja$Fito_Densidad, method = "bray")



###########################Ictio####

Ictio_q0_MA <- vegdist(dataTotal_alta$Ictio_q0, method = "bray")
Ictio_q0_MB <- vegdist(dataTotal_baja$Ictio_q0, method = "bray")

Ictio_q1_MA <- vegdist(dataTotal_alta$Ictio_q1, method = "bray")
Ictio_q1_MB <- vegdist(dataTotal_baja$Ictio_q1, method = "bray")

Ictio_q2_MA <- vegdist(dataTotal_alta$Ictio_q2, method = "bray")
Ictio_q2_MB <- vegdist(dataTotal_baja$Ictio_q2, method = "bray")

Ictio_Densidad_MA <- vegdist(dataTotal_alta$Ictio_Densidad, method = "bray")
Ictio_Densidad_MB <- vegdist(dataTotal_baja$Ictio_Densidad, method = "bray")





NO2_MA_matrix <- vegdist(dataTotal_alta$NO2, matriz_distancias = "bray")
NO2_MB_matrix <- vegdist(dataTotal_baja$NO2, matriz_distancias = "bray")



NO3_MA_matrix <- vegdist(dataTotal_alta$NO3, matriz_distancias = "bray")
NO3_MB_matrix <- vegdist(dataTotal_baja$NO3, matriz_distancias = "bray")


PO4_MA_matrix <- vegdist(dataTotal_alta$PO4, matriz_distancias = "bray")
PO4_MB_matrix <- vegdist(dataTotal_baja$PO4, matriz_distancias = "bray")

SiO2_MA_matrix <- vegdist(dataTotal_alta$SiO2, matriz_distancias = "bray")
SiO2_MB_matrix <- vegdist(dataTotal_baja$SiO2, matriz_distancias = "bray")


pH_MA_matrix <- vegdist(dataTotal_alta$pH, matriz_distancias = "bray")
pH_MB_matrix <- vegdist(dataTotal_baja$pH, matriz_distancias = "bray")


OD_MA_matrix <- vegdist(dataTotal_alta$OD, matriz_distancias = "bray")
OD_MB_matrix <- vegdist(dataTotal_baja$OD, matriz_distancias = "bray")



Transparencia_MA_matrix <- vegdist(dataTotal_alta$Transparencia, matriz_distancias = "bray")
Transparencia_MB_matrix <- vegdist(dataTotal_baja$Transparencia, matriz_distancias = "bray")


SST_MA_matrix <- vegdist(dataTotal_alta$SST, matriz_distancias = "bray")
SST_MB_matrix <- vegdist(dataTotal_baja$SST, matriz_distancias = "bray")


TSI_SECCHI_MA_matrix <- vegdist(dataTotal_alta$TSI_SECCHI, matriz_distancias = "bray")
TSI_SECCHI_MB_matrix <- vegdist(dataTotal_baja$TSI_SECCHI, matriz_distancias = "bray")


Temperatura_median_MA_matrix <- vegdist(dataTotal_alta$Temperatura_median, matriz_distancias = "bray")
Temperatura_median_MB_matrix <- vegdist(dataTotal_baja$Temperatura_median, matriz_distancias = "bray")



Salinidad_median_MA_matrix <- vegdist(dataTotal_alta$Salinidad_median, matriz_distancias = "bray")
Salinidad_median_MB_matrix <- vegdist(dataTotal_baja$Salinidad_median, matriz_distancias = "bray")



Oxigeno_median_MA_matrix <- vegdist(dataTotal_alta$Oxigeno_median, matriz_distancias = "bray")
Oxigeno_median_MB_matrix <- vegdist(dataTotal_baja$Oxigeno_median, matriz_distancias = "bray")


Densidad_median_MA_matrix <- vegdist(dataTotal_alta$Densidad_median, matriz_distancias = "bray")
Densidad_median_MB_matrix <- vegdist(dataTotal_baja$Densidad_median, matriz_distancias = "bray")


Temperatura_IQR_MA_matrix <- vegdist(dataTotal_alta$Temperatura_IQR, matriz_distancias = "bray")
Temperatura_IQR_MB_matrix <- vegdist(dataTotal_baja$Temperatura_IQR, matriz_distancias = "bray")


Salinidad_IQR_MA_matrix <- vegdist(dataTotal_alta$Salinidad_IQR, matriz_distancias = "bray")
Salinidad_IQR_MB_matrix <- vegdist(dataTotal_baja$Salinidad_IQR, matriz_distancias = "bray")


Oxigeno_IQR_MA_matrix <- vegdist(dataTotal_alta$Oxigeno_IQR, matriz_distancias = "bray")
Oxigeno_IQR_MB_matrix <- vegdist(dataTotal_baja$Oxigeno_IQR, matriz_distancias = "bray")


Densidad_IQR_MA_matrix <- vegdist(dataTotal_alta$Densidad_IQR, matriz_distancias = "bray")
Densidad_IQR_MB_matrix <- vegdist(dataTotal_baja$Densidad_IQR, matriz_distancias = "bray")


####Salinidad
Salinidad_Sup_MA_matrix <- vegdist(dataTotal_alta$Salinidad_Sup, method = "bray")
Salinidad_Sup_MB_matrix <- vegdist(dataTotal_baja$Salinidad_Sup, method = "bray")

######Temperatura
Temperatura_Sup_MA_matrix <- vegdist(dataTotal_alta$Temperatura_Sup, method = "bray")
Temperatura_Sup_MB_matrix <- vegdist(dataTotal_baja$Temperatura_Sup, method = "bray")


Densidad_Sup_MA_matrix <- vegdist(dataTotal_alta$Densidad_Sup, matriz_distancias = "bray")
Densidad_Sup_MB_matrix <- vegdist(dataTotal_baja$Densidad_Sup, matriz_distancias = "bray")


#####Fosfatos









mantelPar_Resultados<-function(Matriz_Bio, Matrix_Var, Nombre_bio, Nombre_Var, Marea){
assign(paste0(Nombre_bio, Nombre_Var, Marea, "_mantel.partial"), mantel.partial(Matriz_Bio, Matrix_Var, matriz_distancias, permutations = 999,method = "spearman"),  envir = parent.frame())
  capture.output(paste0("###", Nombre_bio,Nombre_Var,Marea),
                 
                 
                 print(mantel.partial(Matriz_Bio, Matrix_Var, matriz_distancias, permutations = 999,method = "spearman")),
  file = paste0(Nombre_bio,Nombre_Var,Marea, "_mantelPar.txt"))
            
  
}
mantelPar_Resultados(Fito_q0_MA, NO2_MA_matrix, "Fito_q0", "NO2", "Alta")
mantelPar_Resultados(Fito_q0_MB, NO2_MB_matrix, "Fito_q0", "NO2", "Baja")
mantelPar_Resultados(Fito_q1_MA, NO2_MA_matrix, "Fito_q1", "NO2", "Alta")
mantelPar_Resultados(Fito_q1_MB, NO2_MB_matrix, "Fito_q1", "NO2", "Baja")
mantelPar_Resultados(Fito_q2_MA, NO2_MA_matrix, "Fito_q2", "NO2", "Alta")
mantelPar_Resultados(Fito_q2_MB, NO2_MB_matrix, "Fito_q2", "NO2", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, NO2_MA_matrix, "Fito_Densidad", "NO2", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, NO2_MB_matrix, "Fito_Densidad", "NO2", "Baja")

mantelPar_Resultados(Fito_q0_MA, NO3_MA_matrix, "Fito_q0", "NO3", "Alta")
mantelPar_Resultados(Fito_q0_MB, NO3_MB_matrix, "Fito_q0", "NO3", "Baja")
mantelPar_Resultados(Fito_q1_MA, NO3_MA_matrix, "Fito_q1", "NO3", "Alta")
mantelPar_Resultados(Fito_q1_MB, NO3_MB_matrix, "Fito_q1", "NO3", "Baja")
mantelPar_Resultados(Fito_q2_MA, NO3_MA_matrix, "Fito_q2", "NO3", "Alta")
mantelPar_Resultados(Fito_q2_MB, NO3_MB_matrix, "Fito_q2", "NO3", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, NO3_MA_matrix, "Fito_Densidad", "NO3", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, NO3_MB_matrix, "Fito_Densidad", "NO3", "Baja")

mantelPar_Resultados(Fito_q0_MA, PO4_MA_matrix, "Fito_q0", "PO4", "Alta")
mantelPar_Resultados(Fito_q0_MB, PO4_MB_matrix, "Fito_q0", "PO4", "Baja")
mantelPar_Resultados(Fito_q1_MA, PO4_MA_matrix, "Fito_q1", "PO4", "Alta")
mantelPar_Resultados(Fito_q1_MB, PO4_MB_matrix, "Fito_q1", "PO4", "Baja")
mantelPar_Resultados(Fito_q2_MA, PO4_MA_matrix, "Fito_q2", "PO4", "Alta")
mantelPar_Resultados(Fito_q2_MB, PO4_MB_matrix, "Fito_q2", "PO4", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, PO4_MA_matrix, "Fito_Densidad", "PO4", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, PO4_MB_matrix, "Fito_Densidad", "PO4", "Baja")

mantelPar_Resultados(Fito_q0_MA, SiO2_MA_matrix, "Fito_q0", "SiO2", "Alta")
mantelPar_Resultados(Fito_q0_MB, SiO2_MB_matrix, "Fito_q0", "SiO2", "Baja")
mantelPar_Resultados(Fito_q1_MA, SiO2_MA_matrix, "Fito_q1", "SiO2", "Alta")
mantelPar_Resultados(Fito_q1_MB, SiO2_MB_matrix, "Fito_q1", "SiO2", "Baja")
mantelPar_Resultados(Fito_q2_MA, SiO2_MA_matrix, "Fito_q2", "SiO2", "Alta")
mantelPar_Resultados(Fito_q2_MB, SiO2_MB_matrix, "Fito_q2", "SiO2", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, SiO2_MA_matrix, "Fito_Densidad", "SiO2", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, SiO2_MB_matrix, "Fito_Densidad", "SiO2", "Baja")

mantelPar_Resultados(Fito_q0_MA, pH_MA_matrix, "Fito_q0", "pH", "Alta")
mantelPar_Resultados(Fito_q0_MB, pH_MB_matrix, "Fito_q0", "pH", "Baja")
mantelPar_Resultados(Fito_q1_MA, pH_MA_matrix, "Fito_q1", "pH", "Alta")
mantelPar_Resultados(Fito_q1_MB, pH_MB_matrix, "Fito_q1", "pH", "Baja")
mantelPar_Resultados(Fito_q2_MA, pH_MA_matrix, "Fito_q2", "pH", "Alta")
mantelPar_Resultados(Fito_q2_MB, pH_MB_matrix, "Fito_q2", "pH", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, pH_MA_matrix, "Fito_Densidad", "pH", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, pH_MB_matrix, "Fito_Densidad", "pH", "Baja")

mantelPar_Resultados(Fito_q0_MA, OD_MA_matrix, "Fito_q0", "OD", "Alta")
mantelPar_Resultados(Fito_q0_MB, OD_MB_matrix, "Fito_q0", "OD", "Baja")
mantelPar_Resultados(Fito_q1_MA, OD_MA_matrix, "Fito_q1", "OD", "Alta")
mantelPar_Resultados(Fito_q1_MB, OD_MB_matrix, "Fito_q1", "OD", "Baja")
mantelPar_Resultados(Fito_q2_MA, OD_MA_matrix, "Fito_q2", "OD", "Alta")
mantelPar_Resultados(Fito_q2_MB, OD_MB_matrix, "Fito_q2", "OD", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, OD_MA_matrix, "Fito_Densidad", "OD", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, OD_MB_matrix, "Fito_Densidad", "OD", "Baja")

mantelPar_Resultados(Fito_q0_MA, Transparencia_MA_matrix, "Fito_q0", "Transparencia", "Alta")
mantelPar_Resultados(Fito_q0_MB, Transparencia_MB_matrix, "Fito_q0", "Transparencia", "Baja")
mantelPar_Resultados(Fito_q1_MA, Transparencia_MA_matrix, "Fito_q1", "Transparencia", "Alta")
mantelPar_Resultados(Fito_q1_MB, Transparencia_MB_matrix, "Fito_q1", "Transparencia", "Baja")
mantelPar_Resultados(Fito_q2_MA, Transparencia_MA_matrix, "Fito_q2", "Transparencia", "Alta")
mantelPar_Resultados(Fito_q2_MB, Transparencia_MB_matrix, "Fito_q2", "Transparencia", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Transparencia_MA_matrix, "Fito_Densidad", "Transparencia", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Transparencia_MB_matrix, "Fito_Densidad", "Transparencia", "Baja")

mantelPar_Resultados(Fito_q0_MA, SST_MA_matrix, "Fito_q0", "SST", "Alta")
mantelPar_Resultados(Fito_q0_MB, SST_MB_matrix, "Fito_q0", "SST", "Baja")
mantelPar_Resultados(Fito_q1_MA, SST_MA_matrix, "Fito_q1", "SST", "Alta")
mantelPar_Resultados(Fito_q1_MB, SST_MB_matrix, "Fito_q1", "SST", "Baja")
mantelPar_Resultados(Fito_q2_MA, SST_MA_matrix, "Fito_q2", "SST", "Alta")
mantelPar_Resultados(Fito_q2_MB, SST_MB_matrix, "Fito_q2", "SST", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, SST_MA_matrix, "Fito_Densidad", "SST", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, SST_MB_matrix, "Fito_Densidad", "SST", "Baja")

mantelPar_Resultados(Fito_q0_MA, TSI_SECCHI_MA_matrix, "Fito_q0", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Fito_q0_MB, TSI_SECCHI_MB_matrix, "Fito_q0", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Fito_q1_MA, TSI_SECCHI_MA_matrix, "Fito_q1", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Fito_q1_MB, TSI_SECCHI_MB_matrix, "Fito_q1", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Fito_q2_MA, TSI_SECCHI_MA_matrix, "Fito_q2", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Fito_q2_MB, TSI_SECCHI_MB_matrix, "Fito_q2", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, TSI_SECCHI_MA_matrix, "Fito_Densidad", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, TSI_SECCHI_MB_matrix, "Fito_Densidad", "TSI_SECCHI", "Baja")

mantelPar_Resultados(Fito_q0_MA, Temperatura_median_MA_matrix, "Fito_q0", "Temperatura_median", "Alta")
mantelPar_Resultados(Fito_q0_MB, Temperatura_median_MB_matrix, "Fito_q0", "Temperatura_median", "Baja")
mantelPar_Resultados(Fito_q1_MA, Temperatura_median_MA_matrix, "Fito_q1", "Temperatura_median", "Alta")
mantelPar_Resultados(Fito_q1_MB, Temperatura_median_MB_matrix, "Fito_q1", "Temperatura_median", "Baja")
mantelPar_Resultados(Fito_q2_MA, Temperatura_median_MA_matrix, "Fito_q2", "Temperatura_median", "Alta")
mantelPar_Resultados(Fito_q2_MB, Temperatura_median_MB_matrix, "Fito_q2", "Temperatura_median", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Temperatura_median_MA_matrix, "Fito_Densidad", "Temperatura_median", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Temperatura_median_MB_matrix, "Fito_Densidad", "Temperatura_median", "Baja")

mantelPar_Resultados(Fito_q0_MA, Oxigeno_median_MA_matrix, "Fito_q0", "Oxigeno_median", "Alta")
mantelPar_Resultados(Fito_q0_MB, Oxigeno_median_MB_matrix, "Fito_q0", "Oxigeno_median", "Baja")
mantelPar_Resultados(Fito_q1_MA, Oxigeno_median_MA_matrix, "Fito_q1", "Oxigeno_median", "Alta")
mantelPar_Resultados(Fito_q1_MB, Oxigeno_median_MB_matrix, "Fito_q1", "Oxigeno_median", "Baja")
mantelPar_Resultados(Fito_q2_MA, Oxigeno_median_MA_matrix, "Fito_q2", "Oxigeno_median", "Alta")
mantelPar_Resultados(Fito_q2_MB, Oxigeno_median_MB_matrix, "Fito_q2", "Oxigeno_median", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Oxigeno_median_MA_matrix, "Fito_Densidad", "Oxigeno_median", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Oxigeno_median_MB_matrix, "Fito_Densidad", "Oxigeno_median", "Baja")

mantelPar_Resultados(Fito_q0_MA, Densidad_median_MA_matrix, "Fito_q0", "Densidad_median", "Alta")
mantelPar_Resultados(Fito_q0_MB, Densidad_median_MB_matrix, "Fito_q0", "Densidad_median", "Baja")
mantelPar_Resultados(Fito_q1_MA, Densidad_median_MA_matrix, "Fito_q1", "Densidad_median", "Alta")
mantelPar_Resultados(Fito_q1_MB, Densidad_median_MB_matrix, "Fito_q1", "Densidad_median", "Baja")
mantelPar_Resultados(Fito_q2_MA, Densidad_median_MA_matrix, "Fito_q2", "Densidad_median", "Alta")
mantelPar_Resultados(Fito_q2_MB, Densidad_median_MB_matrix, "Fito_q2", "Densidad_median", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Densidad_median_MA_matrix, "Fito_Densidad", "Densidad_median", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Densidad_median_MB_matrix, "Fito_Densidad", "Densidad_median", "Baja")

mantelPar_Resultados(Fito_q0_MA, Temperatura_IQR_MA_matrix, "Fito_q0", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Fito_q0_MB, Temperatura_IQR_MB_matrix, "Fito_q0", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Fito_q1_MA, Temperatura_IQR_MA_matrix, "Fito_q1", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Fito_q1_MB, Temperatura_IQR_MB_matrix, "Fito_q1", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Fito_q2_MA, Temperatura_IQR_MA_matrix, "Fito_q2", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Fito_q2_MB, Temperatura_IQR_MB_matrix, "Fito_q2", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Temperatura_IQR_MA_matrix, "Fito_Densidad", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Temperatura_IQR_MB_matrix, "Fito_Densidad", "Temperatura_IQR", "Baja")

mantelPar_Resultados(Fito_q0_MA, Salinidad_IQR_MA_matrix, "Fito_q0", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Fito_q0_MB, Salinidad_IQR_MB_matrix, "Fito_q0", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Fito_q1_MA, Salinidad_IQR_MA_matrix, "Fito_q1", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Fito_q1_MB, Salinidad_IQR_MB_matrix, "Fito_q1", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Fito_q2_MA, Salinidad_IQR_MA_matrix, "Fito_q2", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Fito_q2_MB, Salinidad_IQR_MB_matrix, "Fito_q2", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Salinidad_IQR_MA_matrix, "Fito_Densidad", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Salinidad_IQR_MB_matrix, "Fito_Densidad", "Salinidad_IQR", "Baja")

mantelPar_Resultados(Fito_q0_MA, Oxigeno_IQR_MA_matrix, "Fito_q0", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Fito_q0_MB, Oxigeno_IQR_MB_matrix, "Fito_q0", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Fito_q1_MA, Oxigeno_IQR_MA_matrix, "Fito_q1", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Fito_q1_MB, Oxigeno_IQR_MB_matrix, "Fito_q1", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Fito_q2_MA, Oxigeno_IQR_MA_matrix, "Fito_q2", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Fito_q2_MB, Oxigeno_IQR_MB_matrix, "Fito_q2", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Oxigeno_IQR_MA_matrix, "Fito_Densidad", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Oxigeno_IQR_MB_matrix, "Fito_Densidad", "Oxigeno_IQR", "Baja")

mantelPar_Resultados(Fito_q0_MA, Densidad_IQR_MA_matrix, "Fito_q0", "Densidad_IQR", "Alta")
mantelPar_Resultados(Fito_q0_MB, Densidad_IQR_MB_matrix, "Fito_q0", "Densidad_IQR", "Baja")
mantelPar_Resultados(Fito_q1_MA, Densidad_IQR_MA_matrix, "Fito_q1", "Densidad_IQR", "Alta")
mantelPar_Resultados(Fito_q1_MB, Densidad_IQR_MB_matrix, "Fito_q1", "Densidad_IQR", "Baja")
mantelPar_Resultados(Fito_q2_MA, Densidad_IQR_MA_matrix, "Fito_q2", "Densidad_IQR", "Alta")
mantelPar_Resultados(Fito_q2_MB, Densidad_IQR_MB_matrix, "Fito_q2", "Densidad_IQR", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Densidad_IQR_MA_matrix, "Fito_Densidad", "Densidad_IQR", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Densidad_IQR_MB_matrix, "Fito_Densidad", "Densidad_IQR", "Baja")

mantelPar_Resultados(Fito_q0_MA, SiO2_MA_matrix, "Fito_q0", "SiO2", "Alta")
mantelPar_Resultados(Fito_q0_MB, SiO2_MB_matrix, "Fito_q0", "SiO2", "Baja")
mantelPar_Resultados(Fito_q1_MA, SiO2_MA_matrix, "Fito_q1", "SiO2", "Alta")
mantelPar_Resultados(Fito_q1_MB, SiO2_MB_matrix, "Fito_q1", "SiO2", "Baja")
mantelPar_Resultados(Fito_q2_MA, SiO2_MA_matrix, "Fito_q2", "SiO2", "Alta")
mantelPar_Resultados(Fito_q2_MB, SiO2_MB_matrix, "Fito_q2", "SiO2", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, SiO2_MA_matrix, "Fito_Densidad", "SiO2", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, SiO2_MB_matrix, "Fito_Densidad", "SiO2", "Baja")




mantelPar_Resultados(Fito_q0_MA, Salinidad_Sup_MA_matrix, "Fito_q0", "Salinidad_sup", "Alta")
mantelPar_Resultados(Fito_q0_MB, Salinidad_Sup_MB_matrix, "Fito_q0", "Salinidad_sup", "Baja")
mantelPar_Resultados(Fito_q1_MA, Salinidad_Sup_MA_matrix, "Fito_q1", "Salinidad_sup", "Alta")
mantelPar_Resultados(Fito_q1_MB, Salinidad_Sup_MB_matrix, "Fito_q1", "Salinidad_sup", "Baja")
mantelPar_Resultados(Fito_q2_MA, Salinidad_Sup_MA_matrix, "Fito_q2", "Salinidad_sup", "Alta")
mantelPar_Resultados(Fito_q2_MB, Salinidad_Sup_MB_matrix, "Fito_q2", "Salinidad_sup", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Salinidad_Sup_MA_matrix, "Fito_Densidad", "Salinidad_sup", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Salinidad_Sup_MB_matrix, "Fito_Densidad", "Salinidad_sup", "Baja")

mantelPar_Resultados(Fito_q0_MA, Temperatura_Sup_MA_matrix, "Fito_q0", "Temperatura_sup", "Alta")
mantelPar_Resultados(Fito_q0_MB, Temperatura_Sup_MB_matrix, "Fito_q0", "Temperatura_sup", "Baja")
mantelPar_Resultados(Fito_q1_MA, Temperatura_Sup_MA_matrix, "Fito_q1", "Temperatura_sup", "Alta")
mantelPar_Resultados(Fito_q1_MB, Temperatura_Sup_MB_matrix, "Fito_q1", "Temperatura_sup", "Baja")
mantelPar_Resultados(Fito_q2_MA, Temperatura_Sup_MA_matrix, "Fito_q2", "Temperatura_sup", "Alta")
mantelPar_Resultados(Fito_q2_MB, Temperatura_Sup_MB_matrix, "Fito_q2", "Temperatura_sup", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Temperatura_Sup_MA_matrix, "Fito_Densidad", "Temperatura_sup", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Temperatura_Sup_MB_matrix, "Fito_Densidad", "Temperatura_sup", "Baja")


mantelPar_Resultados(Fito_q0_MA, Densidad_Sup_Sup_MA_matrix, "Fito_q0", "Densidad_Sup_sup", "Alta")
mantelPar_Resultados(Fito_q0_MB, Densidad_Sup_Sup_MB_matrix, "Fito_q0", "Densidad_Sup_sup", "Baja")
mantelPar_Resultados(Fito_q1_MA, Densidad_Sup_Sup_MA_matrix, "Fito_q1", "Densidad_Sup_sup", "Alta")
mantelPar_Resultados(Fito_q1_MB, Densidad_Sup_Sup_MB_matrix, "Fito_q1", "Densidad_Sup_sup", "Baja")
mantelPar_Resultados(Fito_q2_MA, Densidad_Sup_Sup_MA_matrix, "Fito_q2", "Densidad_Sup_sup", "Alta")
mantelPar_Resultados(Fito_q2_MB, Densidad_Sup_Sup_MB_matrix, "Fito_q2", "Densidad_Sup_sup", "Baja")
mantelPar_Resultados(Fito_Densidad_MA, Densidad_Sup_Sup_MA_matrix, "Fito_Densidad", "Densidad_Sup_sup", "Alta")
mantelPar_Resultados(Fito_Densidad_MB, Densidad_Sup_Sup_MB_matrix, "Fito_Densidad", "Densidad_Sup_sup", "Baja")




mantelPar_Resultados(Ictio_q0_MA, NO2_MA_matrix, "Ictio_q0", "NO2", "Alta")
mantelPar_Resultados(Ictio_q0_MB, NO2_MB_matrix, "Ictio_q0", "NO2", "Baja")
mantelPar_Resultados(Ictio_q1_MA, NO2_MA_matrix, "Ictio_q1", "NO2", "Alta")
mantelPar_Resultados(Ictio_q1_MB, NO2_MB_matrix, "Ictio_q1", "NO2", "Baja")
mantelPar_Resultados(Ictio_q2_MA, NO2_MA_matrix, "Ictio_q2", "NO2", "Alta")
mantelPar_Resultados(Ictio_q2_MB, NO2_MB_matrix, "Ictio_q2", "NO2", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, NO2_MA_matrix, "Ictio_Densidad", "NO2", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, NO2_MB_matrix, "Ictio_Densidad", "NO2", "Baja")

mantelPar_Resultados(Ictio_q0_MA, NO3_MA_matrix, "Ictio_q0", "NO3", "Alta")
mantelPar_Resultados(Ictio_q0_MB, NO3_MB_matrix, "Ictio_q0", "NO3", "Baja")
mantelPar_Resultados(Ictio_q1_MA, NO3_MA_matrix, "Ictio_q1", "NO3", "Alta")
mantelPar_Resultados(Ictio_q1_MB, NO3_MB_matrix, "Ictio_q1", "NO3", "Baja")
mantelPar_Resultados(Ictio_q2_MA, NO3_MA_matrix, "Ictio_q2", "NO3", "Alta")
mantelPar_Resultados(Ictio_q2_MB, NO3_MB_matrix, "Ictio_q2", "NO3", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, NO3_MA_matrix, "Ictio_Densidad", "NO3", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, NO3_MB_matrix, "Ictio_Densidad", "NO3", "Baja")

mantelPar_Resultados(Ictio_q0_MA, PO4_MA_matrix, "Ictio_q0", "PO4", "Alta")
mantelPar_Resultados(Ictio_q0_MB, PO4_MB_matrix, "Ictio_q0", "PO4", "Baja")
mantelPar_Resultados(Ictio_q1_MA, PO4_MA_matrix, "Ictio_q1", "PO4", "Alta")
mantelPar_Resultados(Ictio_q1_MB, PO4_MB_matrix, "Ictio_q1", "PO4", "Baja")
mantelPar_Resultados(Ictio_q2_MA, PO4_MA_matrix, "Ictio_q2", "PO4", "Alta")
mantelPar_Resultados(Ictio_q2_MB, PO4_MB_matrix, "Ictio_q2", "PO4", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, PO4_MA_matrix, "Ictio_Densidad", "PO4", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, PO4_MB_matrix, "Ictio_Densidad", "PO4", "Baja")

mantelPar_Resultados(Ictio_q0_MA, SiO2_MA_matrix, "Ictio_q0", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q0_MB, SiO2_MB_matrix, "Ictio_q0", "SiO2", "Baja")
mantelPar_Resultados(Ictio_q1_MA, SiO2_MA_matrix, "Ictio_q1", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q1_MB, SiO2_MB_matrix, "Ictio_q1", "SiO2", "Baja")
mantelPar_Resultados(Ictio_q2_MA, SiO2_MA_matrix, "Ictio_q2", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q2_MB, SiO2_MB_matrix, "Ictio_q2", "SiO2", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, SiO2_MA_matrix, "Ictio_Densidad", "SiO2", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, SiO2_MB_matrix, "Ictio_Densidad", "SiO2", "Baja")

mantelPar_Resultados(Ictio_q0_MA, pH_MA_matrix, "Ictio_q0", "pH", "Alta")
mantelPar_Resultados(Ictio_q0_MB, pH_MB_matrix, "Ictio_q0", "pH", "Baja")
mantelPar_Resultados(Ictio_q1_MA, pH_MA_matrix, "Ictio_q1", "pH", "Alta")
mantelPar_Resultados(Ictio_q1_MB, pH_MB_matrix, "Ictio_q1", "pH", "Baja")
mantelPar_Resultados(Ictio_q2_MA, pH_MA_matrix, "Ictio_q2", "pH", "Alta")
mantelPar_Resultados(Ictio_q2_MB, pH_MB_matrix, "Ictio_q2", "pH", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, pH_MA_matrix, "Ictio_Densidad", "pH", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, pH_MB_matrix, "Ictio_Densidad", "pH", "Baja")

mantelPar_Resultados(Ictio_q0_MA, OD_MA_matrix, "Ictio_q0", "OD", "Alta")
mantelPar_Resultados(Ictio_q0_MB, OD_MB_matrix, "Ictio_q0", "OD", "Baja")
mantelPar_Resultados(Ictio_q1_MA, OD_MA_matrix, "Ictio_q1", "OD", "Alta")
mantelPar_Resultados(Ictio_q1_MB, OD_MB_matrix, "Ictio_q1", "OD", "Baja")
mantelPar_Resultados(Ictio_q2_MA, OD_MA_matrix, "Ictio_q2", "OD", "Alta")
mantelPar_Resultados(Ictio_q2_MB, OD_MB_matrix, "Ictio_q2", "OD", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, OD_MA_matrix, "Ictio_Densidad", "OD", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, OD_MB_matrix, "Ictio_Densidad", "OD", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Transparencia_MA_matrix, "Ictio_q0", "Transparencia", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Transparencia_MB_matrix, "Ictio_q0", "Transparencia", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Transparencia_MA_matrix, "Ictio_q1", "Transparencia", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Transparencia_MB_matrix, "Ictio_q1", "Transparencia", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Transparencia_MA_matrix, "Ictio_q2", "Transparencia", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Transparencia_MB_matrix, "Ictio_q2", "Transparencia", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Transparencia_MA_matrix, "Ictio_Densidad", "Transparencia", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Transparencia_MB_matrix, "Ictio_Densidad", "Transparencia", "Baja")

mantelPar_Resultados(Ictio_q0_MA, SST_MA_matrix, "Ictio_q0", "SST", "Alta")
mantelPar_Resultados(Ictio_q0_MB, SST_MB_matrix, "Ictio_q0", "SST", "Baja")
mantelPar_Resultados(Ictio_q1_MA, SST_MA_matrix, "Ictio_q1", "SST", "Alta")
mantelPar_Resultados(Ictio_q1_MB, SST_MB_matrix, "Ictio_q1", "SST", "Baja")
mantelPar_Resultados(Ictio_q2_MA, SST_MA_matrix, "Ictio_q2", "SST", "Alta")
mantelPar_Resultados(Ictio_q2_MB, SST_MB_matrix, "Ictio_q2", "SST", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, SST_MA_matrix, "Ictio_Densidad", "SST", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, SST_MB_matrix, "Ictio_Densidad", "SST", "Baja")

mantelPar_Resultados(Ictio_q0_MA, TSI_SECCHI_MA_matrix, "Ictio_q0", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Ictio_q0_MB, TSI_SECCHI_MB_matrix, "Ictio_q0", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Ictio_q1_MA, TSI_SECCHI_MA_matrix, "Ictio_q1", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Ictio_q1_MB, TSI_SECCHI_MB_matrix, "Ictio_q1", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Ictio_q2_MA, TSI_SECCHI_MA_matrix, "Ictio_q2", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Ictio_q2_MB, TSI_SECCHI_MB_matrix, "Ictio_q2", "TSI_SECCHI", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, TSI_SECCHI_MA_matrix, "Ictio_Densidad", "TSI_SECCHI", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, TSI_SECCHI_MB_matrix, "Ictio_Densidad", "TSI_SECCHI", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Temperatura_median_MA_matrix, "Ictio_q0", "Temperatura_median", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Temperatura_median_MB_matrix, "Ictio_q0", "Temperatura_median", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Temperatura_median_MA_matrix, "Ictio_q1", "Temperatura_median", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Temperatura_median_MB_matrix, "Ictio_q1", "Temperatura_median", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Temperatura_median_MA_matrix, "Ictio_q2", "Temperatura_median", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Temperatura_median_MB_matrix, "Ictio_q2", "Temperatura_median", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Temperatura_median_MA_matrix, "Ictio_Densidad", "Temperatura_median", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Temperatura_median_MB_matrix, "Ictio_Densidad", "Temperatura_median", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Oxigeno_median_MA_matrix, "Ictio_q0", "Oxigeno_median", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Oxigeno_median_MB_matrix, "Ictio_q0", "Oxigeno_median", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Oxigeno_median_MA_matrix, "Ictio_q1", "Oxigeno_median", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Oxigeno_median_MB_matrix, "Ictio_q1", "Oxigeno_median", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Oxigeno_median_MA_matrix, "Ictio_q2", "Oxigeno_median", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Oxigeno_median_MB_matrix, "Ictio_q2", "Oxigeno_median", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Oxigeno_median_MA_matrix, "Ictio_Densidad", "Oxigeno_median", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Oxigeno_median_MB_matrix, "Ictio_Densidad", "Oxigeno_median", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Densidad_median_MA_matrix, "Ictio_q0", "Densidad_median", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Densidad_median_MB_matrix, "Ictio_q0", "Densidad_median", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Densidad_median_MA_matrix, "Ictio_q1", "Densidad_median", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Densidad_median_MB_matrix, "Ictio_q1", "Densidad_median", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Densidad_median_MA_matrix, "Ictio_q2", "Densidad_median", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Densidad_median_MB_matrix, "Ictio_q2", "Densidad_median", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Densidad_median_MA_matrix, "Ictio_Densidad", "Densidad_median", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Densidad_median_MB_matrix, "Ictio_Densidad", "Densidad_median", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Temperatura_IQR_MA_matrix, "Ictio_q0", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Temperatura_IQR_MB_matrix, "Ictio_q0", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Temperatura_IQR_MA_matrix, "Ictio_q1", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Temperatura_IQR_MB_matrix, "Ictio_q1", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Temperatura_IQR_MA_matrix, "Ictio_q2", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Temperatura_IQR_MB_matrix, "Ictio_q2", "Temperatura_IQR", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Temperatura_IQR_MA_matrix, "Ictio_Densidad", "Temperatura_IQR", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Temperatura_IQR_MB_matrix, "Ictio_Densidad", "Temperatura_IQR", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Salinidad_IQR_MA_matrix, "Ictio_q0", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Salinidad_IQR_MB_matrix, "Ictio_q0", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Salinidad_IQR_MA_matrix, "Ictio_q1", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Salinidad_IQR_MB_matrix, "Ictio_q1", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Salinidad_IQR_MA_matrix, "Ictio_q2", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Salinidad_IQR_MB_matrix, "Ictio_q2", "Salinidad_IQR", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Salinidad_IQR_MA_matrix, "Ictio_Densidad", "Salinidad_IQR", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Salinidad_IQR_MB_matrix, "Ictio_Densidad", "Salinidad_IQR", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Oxigeno_IQR_MA_matrix, "Ictio_q0", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Oxigeno_IQR_MB_matrix, "Ictio_q0", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Oxigeno_IQR_MA_matrix, "Ictio_q1", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Oxigeno_IQR_MB_matrix, "Ictio_q1", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Oxigeno_IQR_MA_matrix, "Ictio_q2", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Oxigeno_IQR_MB_matrix, "Ictio_q2", "Oxigeno_IQR", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Oxigeno_IQR_MA_matrix, "Ictio_Densidad", "Oxigeno_IQR", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Oxigeno_IQR_MB_matrix, "Ictio_Densidad", "Oxigeno_IQR", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Densidad_IQR_MA_matrix, "Ictio_q0", "Densidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Densidad_IQR_MB_matrix, "Ictio_q0", "Densidad_IQR", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Densidad_IQR_MA_matrix, "Ictio_q1", "Densidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Densidad_IQR_MB_matrix, "Ictio_q1", "Densidad_IQR", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Densidad_IQR_MA_matrix, "Ictio_q2", "Densidad_IQR", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Densidad_IQR_MB_matrix, "Ictio_q2", "Densidad_IQR", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Densidad_IQR_MA_matrix, "Ictio_Densidad", "Densidad_IQR", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Densidad_IQR_MB_matrix, "Ictio_Densidad", "Densidad_IQR", "Baja")

mantelPar_Resultados(Ictio_q0_MA, SiO2_MA_matrix, "Ictio_q0", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q0_MB, SiO2_MB_matrix, "Ictio_q0", "SiO2", "Baja")
mantelPar_Resultados(Ictio_q1_MA, SiO2_MA_matrix, "Ictio_q1", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q1_MB, SiO2_MB_matrix, "Ictio_q1", "SiO2", "Baja")
mantelPar_Resultados(Ictio_q2_MA, SiO2_MA_matrix, "Ictio_q2", "SiO2", "Alta")
mantelPar_Resultados(Ictio_q2_MB, SiO2_MB_matrix, "Ictio_q2", "SiO2", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, SiO2_MA_matrix, "Ictio_Densidad", "SiO2", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, SiO2_MB_matrix, "Ictio_Densidad", "SiO2", "Baja")




mantelPar_Resultados(Ictio_q0_MA, Salinidad_Sup_MA_matrix, "Ictio_q0", "Salinidad_sup", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Salinidad_Sup_MB_matrix, "Ictio_q0", "Salinidad_sup", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Salinidad_Sup_MA_matrix, "Ictio_q1", "Salinidad_sup", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Salinidad_Sup_MB_matrix, "Ictio_q1", "Salinidad_sup", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Salinidad_Sup_MA_matrix, "Ictio_q2", "Salinidad_sup", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Salinidad_Sup_MB_matrix, "Ictio_q2", "Salinidad_sup", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Salinidad_Sup_MA_matrix, "Ictio_Densidad", "Salinidad_sup", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Salinidad_Sup_MB_matrix, "Ictio_Densidad", "Salinidad_sup", "Baja")

mantelPar_Resultados(Ictio_q0_MA, Temperatura_Sup_MA_matrix, "Ictio_q0", "Temperatura_sup", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Temperatura_Sup_MB_matrix, "Ictio_q0", "Temperatura_sup", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Temperatura_Sup_MA_matrix, "Ictio_q1", "Temperatura_sup", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Temperatura_Sup_MB_matrix, "Ictio_q1", "Temperatura_sup", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Temperatura_Sup_MA_matrix, "Ictio_q2", "Temperatura_sup", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Temperatura_Sup_MB_matrix, "Ictio_q2", "Temperatura_sup", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Temperatura_Sup_MA_matrix, "Ictio_Densidad", "Temperatura_sup", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Temperatura_Sup_MB_matrix, "Ictio_Densidad", "Temperatura_sup", "Baja")


mantelPar_Resultados(Ictio_q0_MA, Densidad_Sup_MA_matrix, "Ictio_q0", "Densidad_Sup", "Alta")
mantelPar_Resultados(Ictio_q0_MB, Densidad_Sup_MB_matrix, "Ictio_q0", "Densidad_Sup", "Baja")
mantelPar_Resultados(Ictio_q1_MA, Densidad_Sup_MA_matrix, "Ictio_q1", "Densidad_Sup", "Alta")
mantelPar_Resultados(Ictio_q1_MB, Densidad_Sup_MB_matrix, "Ictio_q1", "Densidad_Sup", "Baja")
mantelPar_Resultados(Ictio_q2_MA, Densidad_Sup_MA_matrix, "Ictio_q2", "Densidad_Sup", "Alta")
mantelPar_Resultados(Ictio_q2_MB, Densidad_Sup_MB_matrix, "Ictio_q2", "Densidad_Sup", "Baja")
mantelPar_Resultados(Ictio_Densidad_MA, Densidad_Sup_MA_matrix, "Ictio_Densidad", "Densidad_Sup", "Alta")
mantelPar_Resultados(Ictio_Densidad_MB, Densidad_Sup_MB_matrix, "Ictio_Densidad", "Densidad_Sup", "Baja")



Fito_Densidad_NO2_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=SiO2_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=NO2_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. [NO"[2]^"-","] [",mu,"M]")))+
  labs(title = "(a)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.37 p < 0.01"))+
  theme_bw()


Fito_Densidad_NO3_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=SiO2_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=NO3_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste(" Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("[Sim. NO"[3]^"-","] [",mu,"M]")))+
  labs(title = "(b)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.21 p < 0.08"))+
  theme_bw()


Fito_q0_PO4_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=SiO2_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_q0_MB_matrix, y=PO4_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Fitoplancton ("^0,"D)")))+
  ylab(expression(paste("Sim. [PO"[4]^-3,"] [",mu,"M]")))+
  labs(title = "(c)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.25 p < 0.05"))+
  theme_bw()

Fito_Densidad_SiO2_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=SiO2_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=SiO2_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. [SiO"[2],"] [",mu,"M]")))+
  labs(title = "(d)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.27 p < 0.05"))+
  theme_bw()


Fito_Densidad_OD_plot<-ggplot() + 
  geom_point(aes(x=Fito_Densidad_MA_matrix, y=OD_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=OD_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. Ox.D.Sup.[mg O"[2],".L"^-1,"]")))+
  labs(title = "(e)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= 0.43 p < 0.05 Baja: r"[M]~"= 0.22 p < 0.05"))+
  theme_bw()


Fito_Densidad_Transparencia_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=Transparencia_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=Transparencia_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression("Sim. Transparencia (m)"))+
  labs(title = "(f)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.24 p < 0.05"))+
  theme_bw()

Fito_Densidad_SST_plot<-ggplot() + 
  geom_point(aes(x=Fito_Densidad_MA_matrix, y=SST_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=SST_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. SST[mg.L"^-1,"]")))+
  labs(title = "(a)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= 0.37 p < 0.05 Baja: r"[M]~"= 0.22 p < 0.07"))+
  theme_bw()


Fito_Densidad_TSI_SECCHI_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=TSI_SECCHI_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=TSI_SECCHI_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. TSI SECCHI")))+
  labs(title = "(b)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.26 p < 0.07"))+
  theme_bw()


Fito_Densidad_Temperatura_median_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=Temperatura_median_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=Temperatura_median_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. Q"[2]," de la Temp.(°C)")))+
  labs(title = "(c)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.40 p < 0.05"))+
  theme_bw()


Fito_Densidad_Temperatura_IQR_plot<-ggplot() + 
  #geom_point(aes(x=Fito_Densidad_MA_matrix, y=Temperatura_IQR_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=Temperatura_IQR_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. RIC Temp. en prof. (°C)")))+
  labs(title = "(d)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Baja: r"[M]~"= 0.14 p < 0.05"))+
  theme_bw()


Fito_Densidad_Salinidad_Sup_plot<-ggplot() + 
  geom_point(aes(x=Fito_Densidad_MA_matrix, y=Salinidad_Sup_MA_matrix), colour="red")+
  geom_point(aes(x=Fito_Densidad_MB_matrix, y=Salinidad_Sup_MB_matrix), colour="blue",fill="blue",shape=23)+
  xlab(expression(paste("Sim. Den. de Fitoplancton ["~Cel.L^-1~"]")))+
  ylab(expression(paste("Sim. Sal. Sup. (PSU)")))+
  labs(title = "(e)",
       #subtitle = expression("SAR Alta"~ rho~"= 0.49 Baja:"~ rho~"= 0.71*"))+
       subtitle = expression("Alta: r"[M]~"= 0.33 p < 0.05 Baja: r"[M]~"= 0.45 p < 0.05"))+
  theme_bw()





png(filename="./Imagenes/Diversidad_Variables_plot_01.png", height =25 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(Fito_Densidad_NO2_plot,
                         Fito_Densidad_NO3_plot,
                         Fito_q0_PO4_plot,
                         Fito_Densidad_SiO2_plot,
                         Fito_Densidad_OD_plot,
                         Fito_Densidad_Transparencia_plot,
                          ncol=2, nrow=3)
dev.off()


png(filename="./Imagenes/Diversidad_Variables_plot_02.png", height =25 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(
                         Fito_Densidad_SST_plot,
                         Fito_Densidad_TSI_SECCHI_plot,
                         Fito_Densidad_Temperatura_median_plot,
                         Fito_Densidad_Temperatura_IQR_plot,
                         Fito_Densidad_Salinidad_Sup_plot,
                         ncol=2, nrow=3)
dev.off()
