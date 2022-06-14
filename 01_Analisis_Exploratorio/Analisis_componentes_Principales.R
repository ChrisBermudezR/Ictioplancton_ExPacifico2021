library(Rcpp)
library(gridExtra)
library(ggplot2)
install.packages("PCAtools")
library(PCAtools)


#PCA Variables
data_completo<-Datos_Totales_Limpios[11:44]


#calculo de la media y varianza de los datos
apply(Datos_Totales_Limpios, 2, mean)
apply(Datos_Totales_Limpios, 2, var)

#Calcular los componentes principales
data_completo.pca <- prcomp(na.omit(data_completo), center = TRUE,scale. = TRUE)
print(data_completo.pca)
summary(data_completo.pca)

#determinar cual variable tiene el efecto mas grande sobre la ordenaci�n de las estaciones

Scores_PC1<-data_completo.pca$rotation[,1]
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
colnames(Comp_var)<-c("PC01", "PC02", "PC03", "PC04", "PC05", "PC06", "PC07","PC08", "PC09", "PC10", "PC11", "PC12", "PC13", "PC14","PC15", "PC16", "PC17", "PC18", "PC19", "PC20", "PC21","PC22", "PC23", "PC24", "PC25", "PC26", "PC27", "PC28","PC29", "PC30", "PC31", "PC32", "PC33", "PC34")
write.table(Comp_var, "Componentes_principales_valores_Var.csv", dec = ".", sep=";")


## scree plot para la variabilidad
pca.var <- data_completo.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


PC<-c("PC01", "PC02", "PC03", "PC04", "PC05", "PC06", "PC07","PC08", "PC09", "PC10", "PC11", "PC12", "PC13", "PC14","PC15", "PC16", "PC17", "PC18", "PC19", "PC20", "PC21","PC22", "PC23", "PC24", "PC25", "PC26", "PC27", "PC28","PC29", "PC30", "PC31", "PC32", "PC33", "PC34")
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



z1 <- data.frame(Estacion = rownames(data_completo.pca$x), data_completo.pca$x[, 1:3])
z2 <- data.frame(Estacion = rownames(data_completo.pca$rotation), data_completo.pca$rotation[, 1:3])


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

p02<-ggplot()+
  xlab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ylab(paste("PC3 - ", pca.var.per[3], "%", sep="")) +
  geom_text(data=z1, aes(PC2, PC3, label=Estacion), col="black") +
  theme_bw()

p_segmentPC1PC2<-ggplot()+
  geom_segment(data=z2, aes(x=PC1, y=PC2,xend=0, yend=0), col="grey20")+
  geom_text(data=z2, aes(PC1, PC2, label=Estacion), col="grey20", size=3) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw()

p_segmentPC2PC3<-ggplot()+
  geom_segment(data=z2, aes(x=PC2, y=PC3,xend=0, yend=0), col="grey20")+
  geom_text(data=z2, aes(PC2, PC3, label=Estacion), col="grey20", size=3) +
  xlab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  ylab(paste("PC3 - ", pca.var.per[3], "%", sep="")) +
  theme_bw()


#Calculo de varianza acumulada
prop_varianza <- data_completo.pca$sdev^2 / sum(data_completo.pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)

acumm_var<-ggplot(data = data.frame(prop_varianza_acum, pc = factor(1:34)),
                  aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componentes principales", 
       y = "Prop. varianza explicada acumulada")


#Grafico

tiff(filename = "PCA_PROF_FLUOR.tiff",width = 45, height = 20, units = "cm", res=300)
grid.arrange(arrangeGrob(p01, p_segmentPC1PC2),arrangeGrob(pc_Graf_Barras),arrangeGrob(acumm_var), nrow=3)
dev.off()

