###############################Caluculo de Correlacion Espacial
#Calculo de distancias####


if(!require(ape))install.packages("ape")
if(!require(gstat))install.packages("gstat")
if(!require(pgirmess))install.packages("pgirmess")
if(!require(ncf))install.packages("ncf")

datosDF <- read.csv("./01_Datos/Datos_Totales_CCCP.csv")




geo_dis<-as.matrix(dist(cbind(datosDF$longitud, datosDF$latitud)))
geo_Inv_Dist_Baja<-1/geo_dis
diag(geo_Inv_Dist_Baja)<-0

head(marea_alta, 3)
plot(marea_alta[,"latitud"] ~marea_alta[, "longitud"], pch=21, bg =gray.colors(12)[cut(marea_alta[,11], breaks = 12)])



coords<-cbind(datosDF$x, datosDF$y)
colnames(coords)<-c("x", "y")
distmat<-as.matrix(dist(coords))

maxdist<-2/3*max(distmat)


correlog.pgirmess<-pgirmess::correlog(coords, as.matrix(marea_alta$NO2), method = "Moran", nbclass = 2, alternative ="two.sided")

plot(correlog.pgirmess)
abline(h=0)


correlog.ncf<-ncf::correlog(x=datosDF$x, y=datosDF$y, z=marea_baja$NO2, increment = 10, resamp = 1000 )

plot(correlog.ncf)
abline(h=0)


spline.corr<-spline.correlog(x=datosDF$x, y=datosDF$y, z=marea_baja$NO2, xmax = maxdist, resamp = 100, type="boot" )

plot(spline.corr)
abline(h=0)

spline.corr<-spline.correlog(x=datosDF$x, y=datosDF$y, z=marea_baja$SST, xmax = maxdist, resamp = 100, type="boot" )

plot(spline.corr)
abline(h=0)





#################
library(sp)
library(sf)
coords<-as.data.frame(cbind(marea_alta$longitud, marea_alta$latitud,marea_alta$NO2 ))
colnames(coords)<-c("x", "y", "variable")
sp::coordinates(coords) = ~x+y
proj4string(coords) <- 
  CRS("+proj=longlat +datum=WGS84")
datos_sp <-
  spTransform(coords,
              CRS("+proj=utm +zone=17 +south
     +ellps=WGS84 +datum=WGS84"))

datosDF<-as.data.frame(datos_sp)

datos_sf <-st_as_sf(datosDF, 
                    coords = c("x", "y"), 
                    crs = "+proj=utm +zone=17 +south
     +ellps=WGS84 +datum=WGS84")



#Calculo de Indice de Moran

ape::Moran.I(marea_alta$NO2, geo_Inv_Dist_Baja)
Moran.I(marea_alta$NO3, geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$PO4 , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$SiO2 , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Clorofila, geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Conductividad , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$pH , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$OD , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Transparencia , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$SST , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$TSI_Clor , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$TSI_SECCHI , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Temperatura_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Oxigeno_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Densidad_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Temperatura_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Oxigeno_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Densidad_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Temperatura_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Oxigeno_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Densidad_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Temperatura_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Oxigeno_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Temperatura_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Salinidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Oxigeno_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Densidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_alta$Profundidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)

semiv_emp <- variogram(Profundidad_max ~ 1, 
                       marea_alta, 
                       cutoff = 400)



geo_dis<-as.matrix(dist(cbind(marea_baja$longitud, marea_baja$latitud)))
geo_Inv_Dist_Baja<-1/geo_dis
diag(geo_Inv_Dist_Baja)<-0


Moran.I(marea_baja$NO2, geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$NO3, geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$PO4 , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$SiO2 , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Clorofila, geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Conductividad , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$pH , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$OD , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Transparencia , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$SST , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$TSI_Clor , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$TSI_SECCHI , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Temperatura_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Oxigeno_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Densidad_mean , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Temperatura_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Oxigeno_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Densidad_median , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Temperatura_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Oxigeno_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Densidad_sd , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Temperatura_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Oxigeno_min , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Temperatura_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Salinidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Oxigeno_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Densidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)
Moran.I(marea_baja$Profundidad_max , geo_Inv_Dist_Baja, na.rm =TRUE)

