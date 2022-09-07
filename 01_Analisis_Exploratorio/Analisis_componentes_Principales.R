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

datos_CCCP<-read.csv("./01_Resultados/Datos_Totales_CCCP.csv")



#Separar la matriz de variables
data_completo_CCCP<-datos_CCCP[c(11:52)]
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
                      "PC29", "PC30",
                      "PC31", "PC32",
                      "PC33","PC34"
                      
)
#exportar los valores de los residuales del cáculo de los componentes
write.table(Comp_var, "Componentes_principales_valores_Var_CCCP.csv", dec = ".", sep=",")


######Gráficas de los componentes principales#####
library("corrplot")
var <-corrplot::get_pca_var(data_completo_CCCP.pca)
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
  labs(x="PC1 (58.8%)", y="PC2 (17.1%)")




#PCA Variables
data_completo<-Datos_Totales_Limpios[c(11:44)]
row.names(data_completo)<-Datos_Totales_Limpios$Codigo
#Calcular los componentes principales
data_completo.pca <- prcomp(na.omit(data_completo), scale = TRUE)
print(data_completo.pca)
summary(data_completo.pca)
Comp_var<-as.data.frame(data_completo.pca[5])
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
                       "PC29", "PC30",
                       "PC31", "PC32",
                       "PC33","PC34"
                      )
write.table(Comp_var, "Componentes_principales_valores_Var.csv", dec = ".", sep=",")









######Gráficas de los componentes principales#####
library("corrplot")
var <- get_pca_var(data_completo.pca)
#Gráfica de correlación entre lasvariables y los componentes
var$cos2

corrplot(var$cor, is.corr=TRUE)

graf01<-factoextra::fviz_eig(data_completo.pca,addlabels = TRUE,hjust = -0.3,linecolor ="red")+
  labs(title="PCA - Screeplot",x="Dimensiones (PC)", y="% explicado de var.")+
  ylim(c(0,65))

graf02


















Scores_PC1<- data_completo.pca$rotation[,1]
var_scores_PC1<-abs(Scores_PC1)
var_scores_rank_PC1<-sort(var_scores_PC1, decreasing = TRUE)
top_var_PC1<-names(var_scores_rank_PC1[1:34])
data_completo.pca$rotation[top_var_PC1,1]#Cuales variables tiran hacia que lado las muestras?



Scores_PC2<-data_completo.pca$rotation[,2]
var_scores_PC2<-abs(Scores_PC2)
var_scores_rank_PC2<-sort(var_scores_PC2, decreasing = TRUE)
top_var_PC2<-names(var_scores_rank_PC2[1:34])
data_completo.pca$rotation[top_var_PC2,2]



Scores_PC3<-data_completo.pca$rotation[,3]
var_scores_PC3<-abs(Scores_PC3)
var_scores_rank_PC3<-sort(var_scores_PC3, decreasing = TRUE)
top_var_PC3<-names(var_scores_rank_PC3[1:34])
data_completo.pca$rotation[top_var_PC3,3]

Scores_PC4<-data_completo.pca$rotation[,4]
var_scores_PC4<-abs(Scores_PC4)
var_scores_rank_PC4<-sort(var_scores_PC4, decreasing = TRUE)
top_var_PC4<-names(var_scores_rank_PC4[1:34])
data_completo.pca$rotation[top_var_PC4,4]

graf01<-fviz_eig(data_completo.pca)


graf02<-fviz_pca_ind(data_completo.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

graf03<-fviz_pca_var(data_completo.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


graf04<-fviz_pca_biplot(data_completo.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


Marea <- as.factor(Datos_Totales_Limpios$Marea)
graf05<-fviz_pca_ind(data_completo.pca,
             col.ind = Marea, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

Transecto <- as.factor(Datos_Totales_Limpios$Transecto)
graf07<-fviz_pca_ind(data_completo.pca,
                     col.ind = Transecto, # color by groups
                     palette = c("#00AFBB",  "#FC4E07", "blue"),
                     addEllipses = TRUE, # Concentration ellipses
                     ellipse.type = "confidence",
                     legend.title = "Groups",
                     repel = TRUE
)

No.Estacion <- as.factor(Datos_Totales_Limpios$No.Estacion)
graf08<-fviz_pca_ind(data_completo.pca,
                     col.ind = No.Estacion, # color by groups
                     palette = c("#00AFBB",  "#FC4E07", "blue", "yellow", "green", "black"),
                     addEllipses = TRUE, # Concentration ellipses
                     ellipse.type = "confidence",
                     legend.title = "Groups",
                     repel = TRUE
)


prop_varianza <- data_completo.pca$sdev^2 / sum(data_completo.pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza[1:10])

graf06<-ggplot(data = data.frame(prop_varianza_acum, pc = factor(1:10)),
           aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componentes principales", 
       y = "Prop. varianza explicada acumulada")

# Contributions of variables to PC1
fviz_contrib(data_completo.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(data_completo.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC1
fviz_contrib(data_completo.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC2
fviz_contrib(data_completo.pca, choice = "var", axes = 4, top = 10)

tiff(filename = "PCA_Total.tiff",width = 30, height = 25, units = "cm", res=300)
grid.arrange(arrangeGrob(graf02,graf04, graf01),arrangeGrob(graf03,graf05,graf06), ncol=2)
dev.off()




data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]




#determinar cual variable tiene el efecto mas grande sobre la ordenaci�n de las estaciones
as.data.frame(data_completo.pca[["scores"]][,1])



Scores_PC1<- data_completo.pca[["scores"]][,1]
var_scores_PC1<-abs(Scores_PC1)
var_scores_rank_PC1<-sort(var_scores_PC1, decreasing = TRUE)
top_var_PC1<-names(var_scores_rank_PC1[1:34])
data_completo.pca$rotation[top_var_PC1,1]#Cuales variables tiran hacia que lado las muestras?



Scores_PC2<-data_completo.pca$rotation[,2]
var_scores_PC2<-abs(Scores_PC2)
var_scores_rank_PC2<-sort(var_scores_PC2, decreasing = TRUE)
top_var_PC2<-names(var_scores_rank_PC2[1:34])
data_completo.pca$rotation[top_var_PC2,2]



Scores_PC3<-data_completo.pca$rotation[,3]
var_scores_PC3<-abs(Scores_PC3)
var_scores_rank_PC3<-sort(var_scores_PC3, decreasing = TRUE)
top_var_PC3<-names(var_scores_rank_PC3[1:34])
data_completo.pca$rotation[top_var_PC3,3]

Scores_PC4<-data_completo.pca$rotation[,4]
var_scores_PC4<-abs(Scores_PC4)
var_scores_rank_PC4<-sort(var_scores_PC4, decreasing = TRUE)
top_var_PC4<-names(var_scores_rank_PC4[1:34])
data_completo.pca$rotation[top_var_PC4,4]

#Obtener los valores de los componentes principales
Comp_var<-as.data.frame(data_completo.pca[5])
colnames(Comp_var)<-c("PC01", "PC02", "PC03", "PC04", "PC05", "PC06", "PC07","PC08", "PC09", "PC10", "PC11", "PC12", "PC13", "PC14","PC15", "PC16", "PC17", "PC18")
write.table(Comp_var, "Componentes_principales_valores_Var.csv", dec = ".", sep=";")


## scree plot para la variabilidad
pca.var <- data_completo.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


PC<-c("PC01", "PC02", "PC03", "PC04", "PC05", "PC06", "PC07","PC08", "PC09", "PC10", "PC11", "PC12", "PC13", "PC14","PC15", "PC16", "PC17", "PC18")
PC_VAR<-as.data.frame(pca.var.per)
PC_NUMERO<-as.data.frame(PC)
PC_BARRAS<-cbind(PC_VAR, PC_NUMERO)
colnames(PC_BARRAS)<-c("Porcentaje", "Componente_Principal")

pc_Graf_Barras<-  ggplot(data=PC_BARRAS, aes(x=Componente_Principal, y=Porcentaje)) +
  xlab("Componentes Principales")+
  ylab("Porcentaje de Variabilidad")+
  geom_bar(stat="identity")+
  theme_bw()

#grafico de los eje de lso componentes

biplot(data_completo.pca,  scale=0, cex = 0.6, col = c("black", "grey50"))



z1 <- data.frame(Estacion = rownames(data_completo.pca$x), data_completo.pca$x[, 1:4])
z1$Estacion<-data_completo$Estacion
z2 <- data.frame(Variables = rownames(data_completo.pca$rotation), data_completo.pca$rotation[, 1:4])


p01<-ggplot()+
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  geom_text(data=z1, aes(PC1, PC2, label=Estacion), col="black") +
  theme_bw()


p02<-ggplot()+
  xlab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ylab(paste("PC3 - ", pca.var.per[3], "%", sep="")) +
  geom_text(data=z1, aes(PC2, PC3, label=Estacion), col="black") +
  theme_bw()

p03<-ggplot()+
  xlab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ylab(paste("PC3 - ", pca.var.per[3], "%", sep="")) +
  geom_text(data=z1, aes(PC3, PC4, label=Estacion), col="black") +
  theme_bw()

p_segmentPC1PC2<-ggplot()+
  geom_segment(data=z2, aes(x=PC1, y=PC2,xend=0, yend=0), col="grey20")+
  geom_text(data=z2, aes(PC1, PC2, label=Variables), col="grey20", size=3) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw()

p_segmentPC2PC3<-ggplot()+
  geom_segment(data=z2, aes(x=PC2, y=PC3,xend=0, yend=0), col="grey20")+
  geom_text(data=z2, aes(PC2, PC3, label=Variables), col="grey20", size=3) +
  xlab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ylab(paste("PC3 - ", pca.var.per[3], "%", sep="")) +
  theme_bw()


#Calculo de varianza acumulada
prop_varianza <- data_completo.pca$sdev^2 / sum(data_completo.pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)

 co<-ggplot(data = data.frame(prop_varianza_acum, pc = factor(1:18)),
                  aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componentes principales", 
       y = "Prop. varianza explicada acumulada")


#Grafico

tiff(filename = "PCA_PROF_FLUOR.tiff",width = 45, height = 20, units = "cm", res=300)
grid.arrange(arrangeGrob(p01, p_segmentPC1PC2),arrangeGrob(pc_Graf_Barras,acumm_var), ncol=2)
dev.off()

