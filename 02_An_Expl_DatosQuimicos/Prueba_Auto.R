load(url("https://github.com/mgimond/Spatial/raw/main/Data/moransI.RData"))
plot(s1)
nb <- poly2nb(s1, queen=TRUE)

library(rgdal)
estaciones<-rgdal::readOGR("../SIG_Datos/estaciones.shp")


nb <- spdep::nbdists(estaciones)
colnames(marea_alta)

corr<-as.matrix(cbind(marea_alta$longitud, marea_alta$latitud))
colnames(corr)<-c("longitud", "latitud")


distGrados<-spDists(corr)

distkm<-dist*100

nombresEstaciones<-marea_alta$Estacion



k1 <- knn2nb(knearneigh(corr))


all.linked <- max(unlist(nbdists(k1, corr)))

nb <- dnearneigh(estaciones, 0, all.linked,  longlat=TRUE,row.names=nombresEstaciones)

listanb<-nb2listw(nb, glist=NULL, style="W", zero.policy=NULL)

summary(nb, coords)

spdep::moran.test(marea_baja$Densidad_Sup, listanb)
