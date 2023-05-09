
if(!require(vegan))install.packages("vegan")
if(!require(dplyr))install.packages("dplyr")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(tidyr))install.packages("tidyr")



source("../Funciones/boxplot_Marea.R")
source("../Funciones/boxplot_Sector.R")
source("../Funciones/boxplot_transecto.R")

Codigo_fito_Densidad<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Densidad.csv", sep=",", header = TRUE)

Div_Code_fito<-Codigo_fito_Densidad[,6:145]
row.names(Div_Code_fito) <- Codigo_fito_Densidad[,1]


S <- c()
# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(Div_Code_fito)){ # Ciclo desde uno hasta el total de filas de los datos (comunidades)
  S.tmp <- length(which(Div_Code_fito[i, ] > 0)) # Número de especies (columnas) con Densidad > 0 por unidad de muestreo
  S <- append(S,S.tmp) # Añadimos el número de especies del sitio i al vector de especies
}


# Índice de Gini-Simpson
Simpson <- diversity(Div_Code_fito, index  = "simpson")

# Índice de Shannon
Shannon <- diversity(Div_Code_fito, index = "shannon")

# Índice de Pielou
Pielou <- Shannon / log(S) 

# Combinamos los índices en una tabla
indices <- cbind(S = S, Simpson = Simpson, Shannon = Shannon, Pielou = Pielou)

# Transformamos los índices tradicionales en índices de diversidad verdadera
indices <- dplyr::mutate(as.data.frame(indices),
                         q0 = S, # Riqueza (q = 0)
                         q1 = exp(Shannon), # Exponencial de Shannon (q = 1)
                         q2 = 1 / (1 - Simpson)) # Inverso de Simpson (q = 2)

datosDensidad.t <- as.data.frame(t(Div_Code_fito))
Densidad <- as.data.frame(colSums(datosDensidad.t))

colnames(Densidad)<-c("Densidad")


Densidad$DenRelativa<-Densidad$Densidad/183086 


Diversidad_Estaciones<-cbind(Codigo_fito_Densidad[,1:4],indices, Densidad)
write.table(Diversidad_Estaciones, file="./Resultados/Diversidad_Estaciones.csv", sep=",", col.names = TRUE, row.names = FALSE)


MRPP_Transecto_Diversidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,9:11],  Diversidad_Estaciones$Transecto, permutations = 2000, distance = "bray")
MRPP_Marea_Diversidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,9:11],  Diversidad_Estaciones$Marea, permutations = 2000, distance = "bray")
MRPP_Sector_Diversidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,9:11],  Diversidad_Estaciones$Sector, permutations = 2000, distance = "bray")

MRPP_Transecto_Densidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,13],  Diversidad_Estaciones$Transecto, permutations = 2000, distance = "bray")
MRPP_Marea_Densidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,13],  Diversidad_Estaciones$Marea, permutations = 2000, distance = "bray")
MRPP_Sector_Densidad_Estaciones<-vegan::mrpp(dat = Diversidad_Estaciones[,13],  Diversidad_Estaciones$Sector, permutations = 2000, distance = "bray")




capture.output("MRPP para los transectos Comparando la diversidad con los números de Hill",
               MRPP_Transecto_Diversidad_Estaciones,
               "MRPP para las Mareas Comparando la diversidad con los números de Hill",
               MRPP_Marea_Diversidad_Estaciones,
               "MRPP para los Sectores Comparando la diversidad con los números de Hill",
               MRPP_Sector_Diversidad_Estaciones,
               "MRPP para los transectos Comparando la densidad",
               MRPP_Transecto_Densidad_Estaciones,
               "MRPP para los Mareas Comparando la densidad",
               MRPP_Marea_Densidad_Estaciones,
               "MRPP para los Sectores Comparando la densidad",
               MRPP_Sector_Densidad_Estaciones,
                           file = "./Resultados/MRPP_Hill_Observados.txt"
               
               
               )



q0_bxplt_Marea<-boxplot_Marea(Diversidad_Estaciones, Diversidad_Estaciones$q0, expression(paste(""^0,"D")))
q0_bxplt_Sector<-boxplot_Sector(Diversidad_Estaciones, Diversidad_Estaciones$q0, expression(paste(""^0,"D")))
q0_bxplt_Transecto<-boxplot_Transecto(Diversidad_Estaciones, Diversidad_Estaciones$q0, expression(paste(""^0,"D")))


q1_bxplt_Marea<-boxplot_Marea(Diversidad_Estaciones, Diversidad_Estaciones$q1, expression(paste(""^1,"D")))
q1_bxplt_Sector<-boxplot_Sector(Diversidad_Estaciones, Diversidad_Estaciones$q1, expression(paste(""^1,"D")))
q1_bxplt_Transecto<-boxplot_Transecto(Diversidad_Estaciones, Diversidad_Estaciones$q1, expression(paste(""^1,"D")))


q2_bxplt_Marea<-boxplot_Marea(Diversidad_Estaciones, Diversidad_Estaciones$q2, expression(paste(""^2,"D")))
q2_bxplt_Sector<-boxplot_Sector(Diversidad_Estaciones, Diversidad_Estaciones$q2, expression(paste(""^2,"D")))
q2_bxplt_Transecto<-boxplot_Transecto(Diversidad_Estaciones, Diversidad_Estaciones$q2, expression(paste(""^2,"D")))


png(filename="./Imagenes/boxplot_Hill.png", height =25 , width = 20, units = "cm", res=400)
gridExtra:: grid.arrange(q0_bxplt_Marea,
                         q0_bxplt_Sector,
                         q0_bxplt_Transecto,
                         q1_bxplt_Marea,
                         q1_bxplt_Sector,
                         q1_bxplt_Transecto,
                         q2_bxplt_Marea,
                         q2_bxplt_Sector,
                         q2_bxplt_Transecto,
                         ncol=3)
dev.off()



Densidad_bxplt_Marea<-boxplot_Marea(Diversidad_Estaciones, Diversidad_Estaciones$Densidad, expression(paste("Densidad ["~Cel.L^-1~"]")))
Densidad_bxplt_Sector<-boxplot_Sector(Diversidad_Estaciones, Diversidad_Estaciones$Densidad, expression(paste("Densidad ["~Cel.L^-1~"]")))
Densidad_bxplt_Transecto<-boxplot_Transecto(Diversidad_Estaciones, Diversidad_Estaciones$Densidad, expression(paste("Densidad ["~Cel.L^-1~"]")))


png(filename="./Imagenes/boxplot_densidad.png", height =25 , width = 15, units = "cm", res=400)
gridExtra:: grid.arrange(Densidad_bxplt_Marea,
                         Densidad_bxplt_Sector,
                         Densidad_bxplt_Transecto,
                         ncol=1)
dev.off()



####Análisis de autocorrelación Espacila####
library(spdep)

Baja <- read.csv(file="./Resultados/Baja_Diversidad_Estaciones_Coordenadas.csv")
Alta <- read.csv(file="./Resultados/Alta_Diversidad_Estaciones_Coordenadas.csv")
#Calculo de distancias

install.packages("ape")
library(ape)




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
moran_Alta_Densidad<-Moran.I(Alta$Densidad , geo_Inv_Dist)

moran_Baja_Pielou<-Moran.I(Baja$Pielou , geo_Inv_Dist)
moran_Alta_Pielou<-Moran.I(Alta$Pielou , geo_Inv_Dist)

moran_Baja_DenRelativa<-Moran.I(Baja$DenRelativa, geo_Inv_Dist)
moran_Alta_DenRelativa<-Moran.I(Alta$DenRelativa, geo_Inv_Dist)

summary(moran_Alta_DenRelativa)

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
               "##############Moran Pielou Baja",
               moran_Baja_Pielou,
               "##############Moran Pielou Alta",
               moran_Alta_Pielou,
               "##############Moran DenRelativa Baja",
               moran_Baja_DenRelativa,
               "##############Moran DenRelativa Alta",
               moran_Alta_DenRelativa,
  
  
  file = "./Resultados/Moran_Pruebas_Hill.txt"
)




######Kriging####
library(stars)
library(gstat)
library(sp)
library(sf)

Baja <- read.csv(file="./Resultados/Baja_Diversidad_Estaciones_Coordenadas.csv")
Alta <- read.csv(file="./Resultados/Alta_Diversidad_Estaciones_Coordenadas.csv")

colnames(Alta)




Alta_sf <- st_as_sf(Alta, coords = c("longitud", "latitud"), remove = FALSE, agr = "constant")

q0_vario = variogram(q0~1, Alta_sf)
q0_v.fit = fit.variogram(v, vgm("Lin"), fit.kappa = TRUE)
plot(q0_vario, q0_v.fit, plot.numbers = TRUE, xlab = "grados decimales")

variogram = autofitVariogram(q0~1,Alta_sf)
png(filename="./Imagenes/q0_v.fit.png", height =15 , width = 25, units = "cm", res=400)
plot(variogram)
dev.off()
ajuste<-variogram$var_model

grd = st_as_stars(st_bbox(Alta_sf))
st_crs(grd) = st_crs(Alta)
kq0 = gstat::krige(q0~1, Alta_sf, grd, ajuste)
## [using ordinary kriging]
names(kq0)[1] <- "q0_Pred"


plot(kq0["q0_Pred"], breaks = "equal", reset = FALSE, axes = TRUE)
plot(Alta_sf, col = 'red', add = TRUE, pch = 3)

q0_kriging<-as.data.frame(kq0)

library(raster)

summary(q0_kriging)

# Crear un objeto SpatialPoints
sp.points <- SpatialPoints(q0_kriging[, 1:2])


ancho = abs(-78.22199 - (-78.40013))
alto = abs(2.848905 - 2.62168)

# Crear un objeto SpatialPixels con la misma extensión y resolución que se quiere en la capa raster

res <- sqrt((alto*ancho)/65088)
raster.out <- raster(extent(-78.40013, -78.22199, 2.62168, 2.848905), res = 0.0007886028 )

# Crear un objeto RasterLayer a partir del objeto SpatialPixels
raster.layer <- raster(raster.out)

# Asignar los valores de var1.pred a la capa raster
values(raster.layer) <- q0_kriging[, 3]

plot(raster.layer)



writeRaster(raster.layer, filename="./Resultados/q0_kriging.tif", format="GTiff", overwrite=TRUE)







Alta_sf <- st_as_sf(Alta, coords = c("longitud", "latitud"), remove = FALSE, agr = "constant")

q1_vario = variogram(q1~1, Alta_sf)
q1_v.fit = fit.variogram(v, vgm("Lin"), fit.kappa = TRUE)
plot(q1_vario, q1_v.fit, plot.numbers = TRUE, xlab = "grados decimales")

variogram = autofitVariogram(q1~1,Alta_sf)
png(filename="./Imagenes/q1_v.fit.png", height =15 , width = 25, units = "cm", res=400)
plot(variogram)
dev.off()

ajuste<-variogram$var_model

grd = st_as_stars(st_bbox(Alta_sf))
st_crs(grd) = st_crs(Alta)
kq1 = gstat::krige(q1~1, Alta_sf, grd, ajuste)
## [using ordinary kriging]
names(kq1)[1] <- "q1_Pred"


plot(kq1["q1_Pred"], breaks = "equal", reset = FALSE, axes = TRUE)
plot(Alta_sf, col = 'red', add = TRUE, pch = 3)

q1_kriging<-as.data.frame(kq1)

library(raster)

summary(q1_kriging)

# Crear un objeto SpatialPoints
sp.points <- SpatialPoints(q1_kriging[, 1:2])


ancho = abs(-78.22199 - (-78.40013))
alto = abs(2.848905 - 2.62168)

# Crear un objeto SpatialPixels con la misma extensión y resolución que se quiere en la capa raster

res <- sqrt((alto*ancho)/65088)
raster.out <- raster(extent(-78.40013, -78.22199, 2.62168, 2.848905), res = 0.0007886028 )

# Crear un objeto RasterLayer a partir del objeto SpatialPixels
raster.layer <- raster(raster.out)

# Asignar los valores de var1.pred a la capa raster
values(raster.layer) <- q1_kriging[, 3]

plot(raster.layer)



writeRaster(raster.layer, filename="./Resultados/q1_kriging.tif", format="GTiff", overwrite=TRUE)








Alta_sf <- st_as_sf(Alta, coords = c("longitud", "latitud"), remove = FALSE, agr = "constant")

q2_vario = variogram(q2~1, Alta_sf)
q2_v.fit = fit.variogram(v, vgm("Lin"), fit.kappa = TRUE)
plot(q2_vario, q2_v.fit, plot.numbers = TRUE, xlab = "grados decimales")

variogram = autofitVariogram(q2~1,Alta_sf)
png(filename="./Imagenes/q2_v.fit.png", height =15 , width = 25, units = "cm", res=400)
plot(variogram)
dev.off()

ajuste<-variogram$var_model

grd = st_as_stars(st_bbox(Alta_sf))
st_crs(grd) = st_crs(Alta)
kq2 = gstat::krige(q2~1, Alta_sf, grd, ajuste)
## [using ordinary kriging]
names(kq2)[1] <- "q2_Pred"


plot(kq2["q2_Pred"], breaks = "equal", reset = FALSE, axes = TRUE)
plot(Alta_sf, col = 'red', add = TRUE, pch = 3)

q2_kriging<-as.data.frame(kq2)

library(raster)

summary(q2_kriging)

# Crear un objeto SpatialPoints
sp.points <- SpatialPoints(q2_kriging[, 1:2])


ancho = abs(-78.22199 - (-78.40013))
alto = abs(2.848905 - 2.62168)

# Crear un objeto SpatialPixels con la misma extensión y resolución que se quiere en la capa raster

res <- sqrt((alto*ancho)/65088)
raster.out <- raster(extent(-78.40013, -78.22199, 2.62168, 2.848905), res = 0.0007886028 )

# Crear un objeto RasterLayer a partir del objeto SpatialPixels
raster.layer <- raster(raster.out)

# Asignar los valores de var1.pred a la capa raster
values(raster.layer) <- q2_kriging[, 3]

plot(raster.layer)



writeRaster(raster.layer, filename="./Resultados/q2_kriging.tif", format="GTiff", overwrite=TRUE)






Alta_sf <- st_as_sf(Alta, coords = c("longitud", "latitud"), remove = FALSE, agr = "constant")

Densidad_vario = variogram(Densidad~1, Alta_sf)
Densidad_v.fit = fit.variogram(v, vgm("Lin"), fit.kappa = TRUE)
plot(Densidad_vario, Densidad_v.fit, plot.numbers = TRUE, xlab = "grados decimales")

variogram = autofitVariogram(Densidad~1,Alta_sf)
png(filename="./Imagenes/Densidad_v.fit.png", height =15 , width = 25, units = "cm", res=400)
plot(variogram)
dev.off()

ajuste<-variogram$var_model

grd = st_as_stars(st_bbox(Alta_sf))
st_crs(grd) = st_crs(Alta)
kDensidad = gstat::krige(Densidad~1, Alta_sf, grd, ajuste)
## [using ordinary kriging]
names(kDensidad)[1] <- "Densidad_Pred"


plot(kDensidad["Densidad_Pred"], breaks = "equal", reset = FALSE, axes = TRUE)
plot(Alta_sf, col = 'red', add = TRUE, pch = 3)

Densidad_kriging<-as.data.frame(kDensidad)

library(raster)

summary(Densidad_kriging)

# Crear un objeto SpatialPoints
sp.points <- SpatialPoints(Densidad_kriging[, 1:2])


ancho = abs(-78.22199 - (-78.40013))
alto = abs(2.848905 - 2.62168)

# Crear un objeto SpatialPixels con la misma extensión y resolución que se quiere en la capa raster

res <- sqrt((alto*ancho)/65088)
raster.out <- raster(extent(-78.40013, -78.22199, 2.62168, 2.848905), res = 0.0007886028 )

# Crear un objeto RasterLayer a partir del objeto SpatialPixels
raster.layer <- raster(raster.out)

# Asignar los valores de var1.pred a la capa raster
values(raster.layer) <- Densidad_kriging[, 3]

plot(raster.layer)



writeRaster(raster.layer, filename="./Resultados/Densidad_kriging.tif", format="GTiff", overwrite=TRUE)








