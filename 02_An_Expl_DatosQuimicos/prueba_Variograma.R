
coordenadas<-cbind(marea_alta$longitud, marea_alta$latitud)
coordenadas<-as.data.frame(coordenadas)
colnames(coordenadas)<-c("longitud", "latitud")

marea_bajacoor<-marea_baja
coordinates(marea_bajacoor) = ~longitud+latitud
variogram(log(NO2)~1, marea_bajacoor)

v<-variogram(log(NO2)~longitud+latitud, marea_bajacoor)
plot(v, plot.numbers = TRUE)
v.fit = fit.variogram(v, vgm(1, "Sph", 700, 1))
set = list(gls=1)
v
g = gstat(NULL, "NO2", log(NO2)~longitud+latitud, marea_bajacoor, model=v.fit, set = set)
variogram(g)

modelo <- vgm(model = "Sph", nugget = NA) # Valores iniciales por defecto
# modelo <- vgm(psill = 3, model = "Sph", range = 75, nugget = 0) # Valores iniciales
fit <- fit.variogram(v, modelo, fit.method = 2)
attr(fit, "SSErr")
plot(v, fit)

plot(v$dist, v$gamma, xlab = "distance", ylab =  "semivariance")
lines(variogramLine(fit, maxdist = "0.1"))
abline(v = 0, lty = 3)
abline(v = range, lty = 3)
abline(h = nugget, lty = 3)
abline(h = sill, lty = 3)
