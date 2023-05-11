#Pruebas no lineales de reducción de dimensionalidad


datos_CCCP<-read.csv("./01_Datos/Datos_Totales_CCCP.csv")




datos_CCCP$NO2

homogenity<-car::leveneTest(data~genus ,center=mean) 
stats::shapiro.test(escalados)
hist(datos_CCCP$NO2)
escalados<-log(no2)
hist(escalados)

pairs.panels(datos_CCCP[,12:31],
             method ="pearson"
)

install.packages("MASS")
library(MASS)
boxcox(lm(no2 ~ 1))
log(no2)
# Aplicar la transformación de Box-Cox a los datos
datos_transformados <- boxcox(no2)
no2<-datos_CCCP$NO2






library(GGally)

png(filename = "./02_Imagenes/CorrMultiples.png",width = 90, height = 45, units = "cm", res=300, pointsize = 0.1)

ggpairs(
  data = datos_CCCP,                ### A quick matrix
  columns = 11:31,
  mapping = ggplot2::aes(color=Marea),   ### With grouping variable
  upper = list(continuous = "cor"), 
  lower = list(continuous = "points"), 
  diag = list(continuous = "densityDiag")
) 

dev.off()


####Prueba non linear PCA
nlpca

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("pcaMethods")
library(pcaMethods)

NLPCA<-pcaMethods::nlpca(datos_CCCP[,12:31], nPcs = 7)


####Kernel PCA

if(!require(kernlab))install.packages("kernlab")
KernelPCA<-kpca(datos_CCCP[,12:31], kernel = "stringdot")

     