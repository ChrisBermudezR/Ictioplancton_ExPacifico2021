if(!require(vegan))install.packages("vegan")
if(!require(dplyr))install.packages("dplyr")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(tidyr))install.packages("tidyr")
if(!require(stars))install.packages("stars")
if(!require(gstat))install.packages("gstat")
if(!require(ape))install.packages("ape")
if(!require(spdep))install.packages("spdep")

source("../Funciones/boxplot_Marea.R")
source("../Funciones/boxplot_Sector.R")
source("../Funciones/boxplot_transecto.R")


####Analisis de diversidad de Ictioplancton####
Codigo_Ictio<-read.table("./Biologicos/DatosP_Ictioplancton//Data_Ictio.csv", sep=",", header = TRUE)

Div_Code_Ictio<-Codigo_Ictio[,5:37]
row.names(Div_Code_Ictio) <- Codigo_Ictio[,1]


S <- c()
# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(Div_Code_Ictio)){ # Ciclo desde uno hasta el total de filas de los datos (comunidades)
  S.tmp <- length(which(Div_Code_Ictio[i, ] > 0)) # Número de especies (columnas) con Densidad > 0 por unidad de muestreo
  S <- append(S,S.tmp) # Añadimos el número de especies del sitio i al vector de especies
}


# Índice de Gini-Simpson
Simpson <- diversity(Div_Code_Ictio, index  = "simpson")

# Índice de Shannon
Shannon <- diversity(Div_Code_Ictio, index = "shannon")

# Índice de Pielou
Pielou <- Shannon / log(S) 

# Combinamos los índices en una tabla
indices <- cbind(S = S, Simpson = Simpson, Shannon = Shannon, Pielou = Pielou)

# Transformamos los índices tradicionales en índices de diversidad verdadera
indices <- dplyr::mutate(as.data.frame(indices),
                         q0 = S, # Riqueza (q = 0)
                         q1 = exp(Shannon), # Exponencial de Shannon (q = 1)
                         q2 = 1 / (1 - Simpson)) # Inverso de Simpson (q = 2)

Ictio_Densidad.t <- as.data.frame(t(Div_Code_Ictio))
Densidad_Ictio <- as.data.frame(colSums(Ictio_Densidad.t))

colnames(Densidad_Ictio)<-c("Densidad")

Ictio_Diversidad_Estaciones<-cbind(Codigo_Ictio[,1:4],indices, Densidad_Ictio)
Ictio_Diversidad_Estaciones$Transecto <- factor(Ictio_Diversidad_Estaciones$Transecto, levels = c("Guascama", "Sanquianga", "Amarales"))

write.table(Ictio_Diversidad_Estaciones, file="./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", col.names = TRUE, row.names = FALSE)


MRPP_Transecto_Ictio_Diversidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,9:11],  Ictio_Diversidad_Estaciones$Transecto, permutations = 2000, distance = "bray")
MRPP_Marea_Ictio_Diversidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,9:11],  Ictio_Diversidad_Estaciones$Marea, permutations = 2000, distance = "bray")
MRPP_Sector_Ictio_Diversidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,9:11],  Ictio_Diversidad_Estaciones$Sector, permutations = 2000, distance = "bray")

MRPP_Transecto_Densidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,12],  Ictio_Diversidad_Estaciones$Transecto, permutations = 2000, distance = "bray")
MRPP_Marea_Densidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,12],  Ictio_Diversidad_Estaciones$Marea, permutations = 2000, distance = "bray")
MRPP_Sector_Densidad_Estaciones<-vegan::mrpp(dat = Ictio_Diversidad_Estaciones[,12],  Ictio_Diversidad_Estaciones$Sector, permutations = 2000, distance = "bray")




capture.output("MRPP para los transectos Comparando la diversidad con los números de Hill",
               MRPP_Transecto_Ictio_Diversidad_Estaciones,
               "MRPP para las Mareas Comparando la diversidad con los números de Hill",
               MRPP_Marea_Ictio_Diversidad_Estaciones,
               "MRPP para los Sectores Comparando la diversidad con los números de Hill",
               MRPP_Sector_Ictio_Diversidad_Estaciones,
               "MRPP para los transectos Comparando la Abundancia",
               MRPP_Transecto_Densidad_Estaciones,
               "MRPP para los Mareas Comparando la Abundancia",
               MRPP_Marea_Densidad_Estaciones,
               "MRPP para los Sectores Comparando la Abundancia",
               MRPP_Sector_Densidad_Estaciones,
               file = "./Resultados/Ictio_MRPP_Hill_Observados.txt"
               
               
)

Datos_Quimica<-read.table("../02_An_Expl_DatosQuimicos/01_Datos_Quimicos/Datos_Quimica.csv", header = TRUE, sep=",")

No.Estacion<-as.data.frame(Datos_Quimica$No.Estacion)
colnames(No.Estacion)<-c("No.Estacion")
Ictio_Diversidad_Estaciones<-cbind(Ictio_Diversidad_Estaciones,No.Estacion)

write.table(Ictio_Diversidad_Estaciones, file="./Resultados/Ictio_Diversidad_Estaciones.csv", sep=",", col.names = TRUE, row.names = FALSE)

q0_bxplt_Marea<-boxplot_marea(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q0, expression(paste(""^0,"D")))
q0_graf_lineas<-graf_lineas(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q0, expression(paste(""^0,"D")))

q1_bxplt_Marea<-boxplot_marea(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q1, expression(paste(""^1,"D")))
q1_graf_lineas<-graf_lineas(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q1, expression(paste(""^1,"D")))

q2_bxplt_Marea<-boxplot_marea(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q2, expression(paste(""^2,"D")))
q2_graf_lineas<-graf_lineas(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$q2, expression(paste(""^2,"D")))


Densidad_bxplt_Marea<-boxplot_marea(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$Densidad, expression(paste("Densidad ["~ Ind.1000m^-3~"]")))

Densidad_graf_lineas<-graf_lineas(Ictio_Diversidad_Estaciones, Ictio_Diversidad_Estaciones$Densidad, expression(paste("Densidad ["~ Ind.1000m^-3~"]")))

png(filename="./Imagenes/Ictio_boxplot_Hill.png", height =25 , width = 20, units = "cm", res=400)
gridExtra:: grid.arrange(q0_bxplt_Marea,
                         q0_graf_lineas,
                         q1_bxplt_Marea,
                         q1_graf_lineas,
                         q2_bxplt_Marea,
                         q2_graf_lineas,
                         Densidad_bxplt_Marea,
                         Densidad_graf_lineas,
                         ncol=2)
dev.off()



####Análisis de autocorrelación Espacila####


Baja <- read.csv(file="./Resultados/Ictio_Baja_Ictio_Diversidad_Estaciones.csv")
Alta <- read.csv(file="./Resultados/Ictio_Alta_Ictio_Diversidad_Estaciones.csv")
#Calculo de distancias






geo_dis<-as.matrix(dist(cbind(Baja$longitud, Baja$latitud)))
geo_Inv_Dist<-1/geo_dis
diag(geo_Inv_Dist)<-0

#Calculo de Indice de Moran

moran_Baja_q0<-Moran.I(Baja$q0, geo_Inv_Dist)
moran_Alta_q0<-Moran.I(Alta$q0, geo_Inv_Dist)

moran_Baja_q1<-Moran.I(Baja$q1, geo_Inv_Dist)
moran_Alta_q1<-Moran.I(Alta$q1, geo_Inv_Dist)

moran_Baja_q2<-Moran.I(Baja$q2, geo_Inv_Dist)
moran_Alta_q2<-Moran.I(Alta$q2, geo_Inv_Dist)


moran_Baja_Densidad<-Moran.I(Baja$Densidad , geo_Inv_Dist)
moran_Alta_Densidad<-Moran.I(Alta$Abundancia , geo_Inv_Dist)




capture.output("##############Moran q0 Baja",
               moran_Baja_q0,
               "##############Moran q0 Alta",
               moran_Alta_q0,
               "##############Moran q1 Baja",
               moran_Baja_q1,
               "##############Moran q1 Alta",
               moran_Alta_q1,
               "##############Moran q2 Baja",
               moran_Baja_q2,
               "##############Moran q2 Alta",
               moran_Alta_q2,
               "##############Moran Densidad Baja",
               moran_Baja_Densidad,
               "##############Moran Densidad Alta",
               moran_Alta_Densidad,
               file = "./Resultados/Ictio_Moran_Pruebas_Hill.txt"
)


