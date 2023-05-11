

FitoData<-read.table("./Resultados/Fito_Diversidad_Estaciones.csv", sep=",", header = TRUE)
IctioData<-read.table("./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", header = TRUE)

Fito_q0<-FitoData$q0
Fito_q1<-FitoData$q1
Fito_q2<-FitoData$q2
Fito_Densidad<-FitoData$Densidad
Fito_Clorofila<-FitoData$Clorofila


Ictio_q0<-IctioData$q0
Ictio_q1<-IctioData$q1
Ictio_q2<-IctioData$q2
Ictio_Densidad<-IctioData$Densidad

#Congruencia en la diversidad

cor.test(Fito_q0,Ictio_q0, method = "spearman")
cor.test(Fito_q1,Ictio_q1, method = "spearman")
cor.test(Fito_q2,Ictio_q2, method = "spearman")
cor.test(Fito_Densidad,Ictio_Densidad, method = "spearman")
cor.test(Fito_Clorofila,Ictio_Densidad, method = "spearman")



plot(Fito_q0,Ictio_q0)
plot(Fito_q1,Ictio_q1)
plot(Fito_q2,Ictio_q2)
plot(Fito_Densidad,Ictio_Densidad)
plot(Fito_Clorofila,Ictio_Densidad)


cor(Fito_q0,Ictio_q0)
plot(Fito_q1,Ictio_q1)
plot(Fito_q2,Ictio_q2)
plot(Fito_Densidad,Ictio_Densidad)
plot(Fito_Clorofila,Ictio_Densidad)




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
write.csv(Ictio_Densidad_Relativa_distmat, "./Resultados/Ictio_Densidad_Relativa_distmat.csv")

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


# Calculating relative abundance and creating new dataframe with relative abundance data
Fito_Densidad_Relativa <-         
  vegan::decostand(Fito_Densidad_df, method = "total")
# Calculate distance matrix
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

plot(Fito_nmds1,Ictio_nmds1)


#Congruencia con el medio ambiente

Componentes_principales_valores_Var_CCCP


PCA_Data<-read.table("../04_Analisis_Combinado/Componentes_principales_valores_Var_CCCP.csv", sep=",", header = TRUE, nrow = FALSE)


PC01<-PCA_Data$PC01

cor.test(Fito_q0,PC01, method = "spearman")
cor.test(Ictio_q0,PC01, method = "spearman")

cor.test(Fito_q1,PC01, method = "spearman")
cor.test(Ictio_q1,PC01, method = "spearman")

cor.test(Fito_q2,PC01, method = "spearman")
cor.test(Ictio_q2,PC01, method = "spearman")

cor.test(Fito_Densidad,PC01, method = "spearman")
cor.test(Ictio_Densidad,PC01, method = "spearman")

cor.test(Fito_Clorofila,PC01, method = "spearman")

plot(Fito_q0,PC01)




Arch_Data<-read.table("../04_Analisis_Combinado/GLRM_Archetipes.csv", sep=",", header = TRUE, nrow = FALSE)

