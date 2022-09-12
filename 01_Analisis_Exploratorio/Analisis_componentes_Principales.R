#Título del script: Análisis de Componentes principales, MRPP y Correlaciones de los datos fisicoquímicos.
#Autores: Christian Bermúdez-Rivas 
#Objetivo: Realizar los análisis de los datos fisico químicos.
#Lenguaje: R
#Fecha: Junio 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################



#library(Rcpp)
install.packages("factoextra")
library(gridExtra)
library(ggplot2)
library(factoextra)
library(corrplot)

datos_CCCP<-read.csv("./01_Resultados/Datos_Totales_CCCP.csv")



#Separar la matriz de variables
data_completo_CCCP<-datos_CCCP[c(12:41)]
#dar nombres a las filas utilizando el codigo para las estaciones en cada tipo de marea
rownames(data_completo_CCCP)<-datos_CCCP$Codigo   


#Calcular los componentes principales
data_completo_CCCP.pca <- prcomp(na.omit(data_completo_CCCP), scale = TRUE)
print(data_completo_CCCP.pca)
summary(data_completo_CCCP.pca)
Comp_var<-as.data.frame(data_completo_CCCP.pca[5])
colnames(Comp_var)<-c("PC01", "PC02", 
                      "PC03", "PC04", 
                      "PC05", "PC06", 
                      "PC07","PC08", 
                      "PC09", "PC10", 
                      "PC11", "PC12", 
                      "PC13", "PC14",
                      "PC15", "PC16", 
                      "PC17", "PC18",
                      "PC19", "PC20",
                      "PC21", "PC22",
                      "PC23", "PC24",
                      "PC25", "PC26",
                      "PC27", "PC28",
                      "PC29", "PC30"
                      
)
#exportar los valores de los residuales del cáculo de los componentes
write.table(Comp_var, "Componentes_principales_valores_Var_CCCP.csv", dec = ".", sep=",")


######Gráficas de los componentes principales#####

var <-get_pca_var(data_completo_CCCP.pca)
#Gráfica de correlación entre lasvariables y los componentes
var$cos2

corrplot(var$cor, is.corr=TRUE)


graf01<-factoextra::fviz_eig(data_completo_CCCP.pca,addlabels = TRUE,hjust = -0.3,linecolor ="red")+
  labs(title="PCA - Screeplot",x="Dimensiones (PC)", y="% explicado de var.")+
  ylim(c(0,65))



PCA_12<-factoextra::fviz_pca_biplot(data_completo_CCCP.pca, repel = TRUE,
                                    axes = c(1,2),
                                    col.var = "#2E9FDF", # Variables color
                                    col.ind = "#696969"  # Individuals color
)+
  labs(x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23<-factoextra::fviz_pca_biplot(data_completo_CCCP.pca, repel = TRUE,
                                    axes = c(2,3),
                                    col.var = "#2E9FDF", # Variables color
                                    col.ind = "#696969"  # Individuals color
)+
  labs(x="PC2 (15%)", y="PC3 (8.8%)")


Marea <- as.factor(datos_CCCP$Marea)
PCA_12_marea<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                       axes = c(1,2),
                                       col.ind = Marea, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por mareas",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_marea<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                       axes = c(2,3),
                                       col.ind = Marea, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por mareas",x="PC2 (15%)", y="PC3 (8.8%)")



Transecto <- as.factor(datos_CCCP$Transecto)


PCA_12_Transecto<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                       axes = c(1,2),
                                       col.ind = Transecto, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07", "grey"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por transectos",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_Transecto<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                       axes = c(2,3),
                                       col.ind = Transecto, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07", "grey"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por transectos",x="PC2 (15%)", y="PC3 (8.8%)")





Sector <- as.factor(datos_CCCP$Sector)


PCA_12_Sector<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                           axes = c(1,2),
                                           col.ind = Sector, # color by groups
                                           palette = c("#00AFBB",  "#FC4E07", "grey", "blue", "black","green"),
                                           addEllipses = TRUE, # Concentration ellipses
                                           ellipse.type = "confidence",
                                           legend.title = "Groups",
                                           repel = TRUE
)+
  labs(title="PCA - Agrupado por sector",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_Sector<-factoextra::fviz_pca_ind(data_completo_CCCP.pca,
                                           axes = c(2,3),
                                           col.ind = Sector, # color by groups
                                           palette = c("#00AFBB",  "#FC4E07", "grey", "blue", "black","green"),
                                           addEllipses = TRUE, # Concentration ellipses
                                           ellipse.type = "confidence",
                                           legend.title = "Groups",
                                           repel = TRUE
)+
  labs(title="PCA - Agrupado por sector",x="PC2 (15%)", y="PC3 (8.8%)")


png(filename = "./PCA_CCCP01.png",width = 20, height = 30, units = "cm", res=300, pointsize = 0.1)
grid.arrange(arrangeGrob(graf01),
             arrangeGrob(PCA_12, PCA_23, ncol = 2),
             ncol=1)
dev.off()


png(filename = "./PCA_CCCP02.png",width = 20, height = 30, units = "cm", res=300, pointsize = 0.1)
grid.arrange(arrangeGrob(PCA_12_marea,PCA_23_marea, ncol = 2),
             arrangeGrob(PCA_12_Transecto,PCA_23_Transecto, ncol = 2),
             arrangeGrob(PCA_12_Sector,PCA_23_Sector, ncol = 2),
             ncol=1)
dev.off()



#####Prueba de grupos con Multi-Response Permutation Procedure#####

#En esta prueba se realizaron 10000 permutaciones

library(vegan)

#Comparación entre las mareas
Mrpp_Marea<-mrpp(data_completo_CCCP, group= datos_CCCP$Marea, distance="bray", permutations = 10000)
#Comparación entre loas transectos
Mrpp_Transecto<-mrpp(data_completo_CCCP, group= datos_CCCP$Transecto, distance="bray", permutations = 10000)

Mrpp_No.Estacion<-mrpp(data_completo_CCCP, group= datos_CCCP$No.Estacion, distance="bray", permutations = 10000)

Mrpp_Marea
Mrpp_Transecto
Mrpp_No.Estacion
summary(Mrpp_Marea)
summary(Mrpp_Transecto)
summary(Mrpp_No.Estacion)




##### PNNE ####

#library(Rcpp)

library(gridExtra)
library(ggplot2)
library(factoextra)
library(corrplot)

datos_PNN<-read.csv("./01_Resultados/Datos_Totales_PNN.csv")
head(datos_PNN)
dim(datos_PNN)

#Separar la matriz de variables
data_completo_PNN<-datos_PNN[c(11:38)]
#dar nombres a las filas utilizando el codigo para las estaciones en cada tipo de marea
rownames(data_completo_PNN)<-datos_PNN$Codigo   


#Calcular los componentes principales
data_completo_PNN.pca <- prcomp(na.omit(data_completo_PNN), scale = TRUE)
print(data_completo_PNN.pca)
summary(data_completo_PNN.pca)
Comp_var<-as.data.frame(data_completo_PNN.pca[5])
colnames(Comp_var)<-c("PC01", "PC02", 
                      "PC03", "PC04", 
                      "PC05", "PC06", 
                      "PC07","PC08", 
                      "PC09", "PC10", 
                      "PC11", "PC12", 
                      "PC13", "PC14",
                      "PC15", "PC16", 
                      "PC17", "PC18",
                      "PC19", "PC20",
                      "PC21", "PC22",
                      "PC23", "PC24",
                      "PC25", "PC26",
                      "PC27", "PC28"
                      
)
#exportar los valores de los residuales del cáculo de los componentes
write.table(Comp_var, "Componentes_principales_valores_Var_PNN.csv", dec = ".", sep=",")


######Gráficas de los componentes principales#####

var_PNN<-get_pca_var(data_completo_PNN.pca)
#Gráfica de correlación entre lasvariables y los componentes
var_PNN$cos2

corrplot(var_PNN$cor, is.corr=TRUE)


graf01_PNN<-factoextra::fviz_eig(data_completo_PNN.pca,addlabels = TRUE,hjust = -0.3,linecolor ="red")+
  labs(title="PCA - Screeplot",x="Dimensiones (PC)", y="% explicado de var.")+
  ylim(c(0,65))



PCA_12_PNN<-factoextra::fviz_pca_biplot(data_completo_PNN.pca, repel = TRUE,
                                    axes = c(1,2),
                                    col.var = "#2E9FDF", # Variables color
                                    col.ind = "#696969"  # Individuals color
)+
  labs(x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_PNN<-factoextra::fviz_pca_biplot(data_completo_PNN.pca, repel = TRUE,
                                    axes = c(2,3),
                                    col.var = "#2E9FDF", # Variables color
                                    col.ind = "#696969"  # Individuals color
)+
  labs(x="PC2 (15%)", y="PC3 (8.8%)")


Marea_PNN <- as.factor(datos_PNN$Marea)
PCA_12_marea_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                       axes = c(1,2),
                                       col.ind = Marea, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por mareas",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_marea_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                       axes = c(2,3),
                                       col.ind = Marea, # color by groups
                                       palette = c("#00AFBB",  "#FC4E07"),
                                       addEllipses = TRUE, # Concentration ellipses
                                       ellipse.type = "confidence",
                                       legend.title = "Groups",
                                       repel = TRUE
)+
  labs(title="PCA - Agrupado por mareas",x="PC2 (15%)", y="PC3 (8.8%)")



Transecto_PNN <- as.factor(datos_PNN$Transecto)


PCA_12_Transecto_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                           axes = c(1,2),
                                           col.ind = Transecto, # color by groups
                                           palette = c("#00AFBB",  "#FC4E07", "grey"),
                                           addEllipses = TRUE, # Concentration ellipses
                                           ellipse.type = "confidence",
                                           legend.title = "Groups",
                                           repel = TRUE
)+
  labs(title="PCA - Agrupado por transectos",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_Transecto_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                           axes = c(2,3),
                                           col.ind = Transecto, # color by groups
                                           palette = c("#00AFBB",  "#FC4E07", "grey"),
                                           addEllipses = TRUE, # Concentration ellipses
                                           ellipse.type = "confidence",
                                           legend.title = "Groups",
                                           repel = TRUE
)+
  labs(title="PCA - Agrupado por transectos",x="PC2 (15%)", y="PC3 (8.8%)")





No.Estacion_PNN <- as.factor(datos_PNN$No.Estacion)


PCA_12_No.Estacion_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                             axes = c(1,2),
                                             col.ind = No.Estacion, # color by groups
                                             palette = c("#00AFBB",  "#FC4E07", "grey", "blue", "black","green"),
                                             addEllipses = TRUE, # Concentration ellipses
                                             ellipse.type = "confidence",
                                             legend.title = "Groups",
                                             repel = TRUE
)+
  labs(title="PCA - Agrupado por No.Estacion",x="PC1 (53.2%)", y="PC2 (15%)")

PCA_23_No.Estacion_PNN<-factoextra::fviz_pca_ind(data_completo_PNN.pca,
                                             axes = c(2,3),
                                             col.ind = No.Estacion, # color by groups
                                             palette = c("#00AFBB",  "#FC4E07", "grey", "blue", "black","green"),
                                             addEllipses = TRUE, # Concentration ellipses
                                             ellipse.type = "confidence",
                                             legend.title = "Groups",
                                             repel = TRUE
)+
  labs(title="PCA - Agrupado por No.Estacion",x="PC2 (15%)", y="PC3 (8.8%)")


png(filename = "./PCA_PNN01.png",width = 20, height = 30, units = "cm", res=300, pointsize = 0.1)
grid.arrange(arrangeGrob(graf01),
             arrangeGrob(PCA_12_PNN, PCA_23_PNN, ncol = 2),
             ncol=1)
dev.off()


png(filename = "./PCA_PNN02.png",width = 20, height = 30, units = "cm", res=300, pointsize = 0.1)
grid.arrange(arrangeGrob(PCA_12_marea_PNN,PCA_23_marea_PNN, ncol = 2),
             arrangeGrob(PCA_12_Transecto_PNN,PCA_23_Transecto_PNN, ncol = 2),
             arrangeGrob(PCA_12_No.Estacion_PNN,PCA_23_No.Estacion_PNN, ncol = 2),
             ncol=1)
dev.off()



#####Prueba de grupos con Multi-Response Permutation Procedure#####

#En esta prueba se realizaron 10000 permutaciones

library(vegan)

#Comparación entre las mareas
Mrpp_Marea_PNN<-mrpp(data_completo_PNN, group= datos_PNN$Marea, distance="bray", permutations = 10000)
#Comparación entre loas transectos
Mrpp_Transecto_PNN<-mrpp(data_completo_PNN, group= datos_PNN$Transecto, distance="bray", permutations = 10000)

Mrpp_No.Estacion_PNN<-mrpp(data_completo_PNN, group= datos_PNN$No.Estacion, distance="bray", permutations = 10000)

Mrpp_Marea_PNN
Mrpp_Transecto_PNN
Mrpp_No.Estacion_PNN
summary(Mrpp_Marea)
summary(Mrpp_Transecto)
summary(Mrpp_No.Estacion)
