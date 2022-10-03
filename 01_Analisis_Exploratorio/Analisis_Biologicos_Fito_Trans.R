install.packages("entropart")
install.packages("iNEXT")
install.packages("vegan")
install.packages("dplyr")
install.packages("tidyr")
install.packages("C:/Users/chris/Downloads/entropart_1.6-11.zip", repos = NULL, type = "source")

library(entropart)
library(iNEXT)
library(vegan)
library(dplyr)
library(tidyverse)
library(tidyr)

data(BCI)
datosDiv <- BCI




# Estimadores de riqueza por unidad de muestreo
est.Sites2 <- vegan::estimateR(datosDiv)

estimateR(datosDiv["S06B",])

# Vector para almacenar el número de doubletons por unidad de muestreo
doub <- c()

# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(Transeptos_fito2)){ # Ciclo desde uno hasta el total de filas de los datos (unidades de muestreo)
  doub.tmp <- length(which(Transeptos_fito2[i, ] == 2)) # Número de doubletons por unidad de muestreo
  doub <- append(doub, doub.tmp) # Añadimos el número de doubletons del sitio i al vector de doubletons
}



# Vector para almacenar los coeficientes de variación
CV <- c() 

# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (unidades de muestreo)
  zeros <- which(datosDiv[i, ] == 0) # Validamos qué especies tienen abundancia de 0 para excluirlas del CV
  CV.tmp <- sd(t(datosDiv[i, -zeros])) / mean(t(datosDiv[i, -zeros])) # CV de la abundancia por unidad de muestreo
  CV <- append(CV, CV.tmp) # Añadimos el CV del sitio i al vector de CV
}


# Estimadores de riqueza para la comunidad entera
est.Comm <- estimateR(colSums(datosDiv)) 



# Estimadores de riqueza adicionales para la comunidad entera
est.Comm <- specpool(datosDiv, smallsample = TRUE)



# Calculo de la diversidad acumulada para 100 permutaciones
accum <- estaccumR(datosDiv, permutations = 100)

# Diversidades acumuladas promedio
accum.mean <- as.datosDiv.frame(accum$means) 

# Abundancias acumuladas
accum.abundance <- cumsum(rowSums(datosDiv))

# Curva de acumulación usando unidades de muestreo
plot(x = accum.mean$N, # Valores de X
     y = accum.mean$S,# Valores de Y
     xlab = "Unidades de muestreo", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     type = "l") # Tipo de línea (sólida)

# Curva de acumulación usando abundancias acumuladas
plot(x = accum.abundance, # Valores de X
     y = accum.mean$S, # Valores de Y
     xlab = "Número de individuos", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     type = "l") # Tipo de línea (sólida)



# Curva de acumulación
plot(x = accum.mean$N, # Valores de X
     y = accum.mean$S, # Valores de Y
     xlab = "Unidades de muestreo", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     type = "l", # Tipo de línea (sólida)
     ylim = c(80, 240), # Límites del eje Y (según los datos)
     yaxt = "n") # Remover la división por defecto del eje Y (para usar personalizada)

# Intervalo personalizado del eje Y de la grafica (según los datos)
axis(side = 2, at = seq(80, 240, 40))

# Curva del estimador Chao
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$Chao, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Curva del estimador ACE
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$ACE, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("SObs", "Chao 1", "ACE"), # Texto de la leyenda
       lty = c(1, 1, 1), # Tipos de línea de los símbolos
       col = c("black", "red", "blue"),  # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.05)) # Márgenes de la leyenda



# Cálculo de los singletons y doubletons acumulados
permutations = 100 # Número de permutaciones
tmp.rnd <- list() # Lista para almacenar las 100 nuevas matrices aleatorizadas
for(i in 1:permutations){ # Ciclo desde uno hasta el total de permutaciones
  tmp.rnd[[i]] <- datosDiv[sample(1:nrow(datosDiv)), ] # Aleatorización del orden de las filas de la matriz original
}
single <- as.datosDiv.frame(matrix(ncol = permutations, nrow = nrow(datosDiv))) # Matriz para almacenar los singletons
double <- as.datosDiv.frame(matrix(ncol = permutations, nrow = nrow(datosDiv))) # Matriz para almacenar los doubletons
for(i in 1:length(tmp.rnd)){ # Ciclo desde uno hasta el total de matrices aleatorizadas
  for(n in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas en los datos
    single[n, i] <- length(which(colSums(tmp.rnd[[i]][1:n, ]) == 1)) # Cálculo de singletons acumulados
    double[n, i] <- length(which(colSums(tmp.rnd[[i]][1:n, ]) == 2)) # Cálculo de doubletons acumulados
  }
}
rares <- datosDiv.frame(single = rowMeans(single), double = rowMeans(double)) # Cálculo de valores promedio por fila

# Curva de acumulación
plot(x = accum.mean$N, # Valores de X
     y = accum.mean$S, # Valores de Y
     xlab = "Unidades de muestreo", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     type = "l", # Tipo de línea (sólida)
     ylim = c(0, 240), # Límites del eje Y (según los datos)
     yaxt = "n") # Remover la división por defecto del eje Y (para usar personalizada)

# Intervalo personalizado del eje Y de la grafica (según los datos)
axis(side = 2, at = seq(0, 240, 40))

# Curva del estimador Chao
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$Chao, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Curva del estimador ACE
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$ACE, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Curva de singletons
lines(x = accum.mean$N, # Valores de X
      y = rares$single, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "darkgreen") # Color de la línea

# Curva de doubletons
lines(x = accum.mean$N, # Valores de X
      y = rares$double, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "orange") # Color de la línea

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("SObs", "Chao 1", "ACE", "Singletons", "Doubletons"), # Texto de la leyenda
       lty = c(1, 1, 1, 1, 1), # Tipos de línea de los símbolos
       col = c("black", "red", "blue", "darkgreen", "orange"),  # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.20)) # Márgenes de la leyenda




# Matriz para almacenar las desviaciones estandar
accum.sd <- matrix(ncol = 5, nrow = nrow(datosDiv))

# Nombres de las columnas de la matriz de desviaciones
colnames(accum.sd) <- c("S.SD", "Chao.SD", "ACE.SD", "Singletons.SD", "Doubletons.SD")

# Ciclo para recorrer las matrices de permutaciones
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (unidades de muestreo)
  accum.sd[i, 1] <- sd(accum$S[i, ]) # Desviación estándar S
  accum.sd[i, 2] <- sd(accum$chao[i, ]) # Desviación estándar Chao
  accum.sd[i, 3] <- sd(accum$ace[i, ]) # Desviación estándar ACE
  accum.sd[i, 4] <- sd(single[i, ]) # Desviación estándar singletons
  accum.sd[i, 5] <- sd(double[i, ]) # Desviación estándar doubletons
}

# Matriz para almacenar los intervalos de confianza
CI <- as.datosDiv.frame(matrix(ncol = 10, nrow = nrow(datosDiv)))

# Nombres de las columnas de la matriz de intervalos de confianza
colnames(CI) <- c("S.LCI", "S.UCI", "Chao.LCI", "Chao.UCI", "ACE.LCI", "ACE.UCI", "Sin.LCI", "Sin.UCI", "Dou.LCI", "Dou.UCI")

# Valor Z para el 95% de confianza
Z <- 1.96

# Ciclo para calcular los intervalos de confianza
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (unidades de muestreo)
  CI[i, 1] <- accum.mean[i, 2] - (Z * (accum.sd[i, 1] / sqrt(ncol(accum$S)))) # Limite inferior 95% CI S
  CI[i, 2] <- accum.mean[i, 2] + (Z * (accum.sd[i, 1] / sqrt(ncol(accum$S)))) # Limite superior 95% CI S
  CI[i, 3] <- accum.mean[i, 3] - (Z * (accum.sd[i, 2] / sqrt(ncol(accum$chao)))) # Limite inferior 95% CI Chao
  CI[i, 4] <- accum.mean[i, 3] + (Z * (accum.sd[i, 2] / sqrt(ncol(accum$chao)))) # Limite superior 95% CI Chao
  CI[i, 5] <- accum.mean[i, 4] - (Z * (accum.sd[i, 3] / sqrt(ncol(accum$ace)))) # Limite inferior 95% CI ACE
  CI[i, 6] <- accum.mean[i, 4] + (Z * (accum.sd[i, 3] / sqrt(ncol(accum$ace)))) # Limite superior 95% CI ACE
  CI[i, 7] <- rares[i, 1] - (Z * (accum.sd[i, 4] / sqrt(length(tmp.rnd)))) # Limite inferior 95% CI singletons
  CI[i, 8] <- rares[i, 1] + (Z * (accum.sd[i, 4] / sqrt(length(tmp.rnd)))) # Limite superior 95% CI singletons
  CI[i, 9] <- rares[i, 2] - (Z * (accum.sd[i, 5] / sqrt(length(tmp.rnd)))) # Limite inferior 95% CI doubletons
  CI[i, 10] <- rares[i, 2] + (Z * (accum.sd[i, 5] / sqrt(length(tmp.rnd)))) # Limite superior 95% CI doubletons
}



# Lienzo vacío para la grafica (con los limites de X e Y según los datos)
plot(NA, # Lienzo sin datos
     xlab = "Unidades de muestreo", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     xlim = c(0, 50), # Límites del eje X (según los datos)
     ylim = c(0, 240), # Límites del eje Y (según los datos)
     yaxt = "n") # Remover la división por defecto del eje Y (para usar personalizada)

# Intervalo personalizado del eje Y de la grafica (según los datos)
axis(side = 2, at = seq(0, 240, 40))

# Polígono con los intervalos de confianza para S
polygon(x = c(seq(1, nrow(datosDiv)) , rev(seq(1, nrow(datosDiv)))), # Valores X del intervalo
        y = c(CI$S.LCI, rev(CI$S.UCI)), # Valores Y del intervalo
        col = rgb(166/255, 122/255, 222/255, 0.6), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Polígono con los intervalos de confianza para Chao
polygon(x = c(seq(1, nrow(datosDiv)) , rev(seq(1, nrow(datosDiv)))), # Valores X del intervalo
        y = c(CI$Chao.LCI, rev(CI$Chao.UCI)), # Valores Y del intervalo
        col = rgb(222/255, 122/255, 122/255, 0.6), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Polígono con los intervalos de confianza para ACE
polygon(x = c(seq(1, nrow(datosDiv)) , rev(seq(1, nrow(datosDiv)))), # Valores X del intervalo
        y = c(CI$ACE.LCI, rev(CI$ACE.UCI)), # Valores Y del intervalo
        col = rgb(122/255, 170/255, 222/255, 0.6), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Polígono con los intervalos de confianza para los singletons
polygon(x = c(seq(1, nrow(datosDiv)) , rev(seq(1, nrow(datosDiv)))), # Valores X del intervalo
        y = c(CI$Sin.LCI, rev(CI$Sin.UCI)), # Valores Y del intervalo
        col = rgb(0/255, 100/255, 0/255, 0.2), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Polígono con los intervalos de confianza para los doubletons
polygon(x = c(seq(1, nrow(datosDiv)) , rev(seq(1, nrow(datosDiv)))), # Valores X del intervalo
        y = c(CI$Dou.LCI, rev(CI$Dou.UCI)), # Valores Y del intervalo
        col = rgb(255/255, 165/255, 0/255, 0.2), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Curva de acumulación (especies observadas)
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$S, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "black") # Color de la línea

# Curva del estimador Chao
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$Chao, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Curva del estimador ACE
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$ACE, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Curva de singletons
lines(x = accum.mean$N, # Valores de X
      y = rares$single, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "darkgreen") # Color de la línea

# Curva de doubletons
lines(x = accum.mean$N, # Valores de X
      y = rares$double, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "orange") # Color de la línea

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("SObs", "Chao 1", "ACE", "Singletons", "Doubletons"), # Texto de la leyenda
       lty = c(1, 1, 1, 1, 1), # Tipos de línea de los símbolos
       col = c("black", "red", "blue", "darkgreen", "orange"),  # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.20)) # Márgenes de la leyenda



# Semilla para generar pseudoaleatorizaciones (solo para que el ejemplo sea replicable)
set.seed(42)

# Vector de abundancias para la Comunidad 1
C1.Jan <- c(rep(1, 70), rep(2, 17), rep(3, 4), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8, 3), rep(9, 1), rep(10, 2), rep(11, 3), rep(12, 2), rep(14, 2), rep(17, 1), rep(19, 2), rep(20, 3), rep(21, 1), rep(24, 1), rep(26, 1), rep(40, 1), rep(57, 2), rep(60, 1), rep(64, 1), rep(71, 1), rep(77, 1))

# Aleatorizamos el orden de los datos
C1.Jan <- sample(C1.Jan, 140)

# Vector de abundancias para la Comunidad 2
C2.Jan <- c(rep(1, 84), rep(2, 10), rep(3, 4), rep(4, 3), rep(5, 5), rep(6, 1), rep(7, 2), rep(8, 1), rep(14, 1), rep(42, 1), rep(0, 28))

# Aleatorizamos el orden de los datos
C2.Jan <- sample(C2.Jan, 140)

# Añadimos ambos vectores a una tabla
datosDiv <- as.datosDiv.frame(rbind(C1 = C1.Jan, C2 = C2.Jan))




# Curvas de acumulación basadas en tamaño
rare <- rarecurve(datosDiv, # Matriz de datos
                  col = c("red", "blue"), # Colores de las líneas
                  label = FALSE, # Sin etiquetas de datos
                  xlab = "Número de individuos", # Etiqueta del eje X
                  ylab = "Riqueza acumulada", # Etiqueta del eje Y
                  ylim = c(0, 140), # Límites del eje Y (según los datos)
                  yaxt = "n") # Remover la división por defecto del eje Y (para usar personalizada)

# Intervalo personalizado del eje Y de la grafica (según los datos)
axis(side = 2, at = seq(0, 140, 20))

# Obtenemos el valor de abundancia mínima común entre las comunidades
ab.min <- min(rowSums(datosDiv))

# Línea punteada en el valor de abundancia mínima común
abline(v = ab.min, lty = 2)

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("C1", "C2"), # Texto de la leyenda
       lty = c(1, 1), # Tipos de línea de los símbolos
       col = c("red", "blue"), # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.05))# Márgenes de la leyenda

# Valores de diversidad al mismo tamaño de muestra
S.rare <- cbind(C1 = rare[[1]][ab.min], C2 = rare[[2]][ab.min])




# Valores de diversidad al mismo tamaño de muestra
S.rare <- cbind(C1 = rare[[1]][ab.min], C2 = rare[[2]][ab.min])




# Vectores para almacenar el número de singletons y doubletons por comunidad
sing <- c()
doub <- c()

# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (comunidades)
  sing.tmp <- length(which(datosDiv[i, ] == 1)) # Número de singletons por comunidad
  sing <- append(sing, sing.tmp) # Añadimos el número de singletons del sitio i al vector de singletons
  doub.tmp <- length(which(datosDiv[i, ] == 2)) # Número de doubletons por comunidad
  doub <- append(doub, doub.tmp) # Añadimos el número de doubletons del sitio i al vector de doubletons
}

# Abundancias totales por comunidad
abundances <- rowSums(datosDiv)

# Matriz para almacenar los datos de cobertura por comunidad
coverage <- matrix(ncol = nrow(datosDiv), nrow = 1)
colnames(coverage) <- c("C1", "C2") # Nombres de las columnas de la matriz de coberturas

# Ciclo para calcular la cobertura por comunidad
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (comunidades)
  f1 <- sing[i]
  f2 <- doub[i]
  n <- abundances[i]
  coverage[1, i] <- 1 - ((f1 / n) * (((n - 1) * f1) / (((n - 1) * f1) + (2 * f2)))) # Cobertura de muestra
}


# Matriz de datos transpuesta
datosDiv.t <- as.datosDiv.frame(t(datosDiv))

# Comando general de iNEXT (calcula muchas cosas)
est.Comms <- iNEXT(datosDiv.t, q = 0, datosDivtype = "abundance") # q = 0 es la riqueza (diversidades verdaderas)



# Cobertura de muestra C1
SC.C1 <- est.Comms$iNextEst$C1[which(est.Comms$iNextEst$C1$method == "observed"), 7]

# Cobertura de muestra C2
SC.C2 <- est.Comms$iNextEst$C2[which(est.Comms$iNextEst$C2$method == "observed"), 7]

# Añadimos las coberturas a una tabla
coverage <- cbind(C1 = SC.C1, C2 = SC.C2) 



# Abundancias totales por comunidad
abundances <- colSums(datosDiv.t)

# Valor máximo para extrapolar (no más del doble de las abundancias)
max.Extrapol <- abundances * 2

# Estimadores para cada comunidad
rare.C1 <- iNEXT(datosDiv.t[, 1], q = 0, datosDivtype = "abundance", knots = max.Extrapol[1])
rare.C2 <- iNEXT(datosDiv.t[, 2], q = 0, datosDivtype = "abundance", knots = max.Extrapol[2])



# Filtro para extraer los valores de ambas comunidades al 65% de cobertura (redondeado a dos decimales)
S.C1.rare <- dplyr::filter(rare.C1$iNextEst, round(SC, 2) == 0.65)$qD
S.C2.rare <- dplyr::filter(rare.C2$iNextEst, round(SC, 2) == 0.65)$qD

# Añadimos los valores promediados a una tabla
S.Comms.rare <- cbind(C1 = mean(S.C1.rare), C2 = mean(S.C2.rare))




# Filtro para separar los datos interpolados y extrapolados de ambas comunidades
# (abundancia menor o igual a la observada = interpolación, mayor = extrapolación)
C1.inter <- dplyr::filter(rare.C1$iNextEst, m <= abundances[1])
C1.extra <- dplyr::filter(rare.C1$iNextEst, m > abundances[1])
C2.inter <- dplyr::filter(rare.C2$iNextEst, m <= abundances[2])
par(mfrow = c(1, 1), mar = c(4, 4, 1, 2), oma = c(0, 2, 0, 0))

# Lienzo vacío para la grafica (con los limites de X e Y según los datos)
plot(NA, # Lienzo sin datos
     xlab = "Cobertura de muestra", # Etiqueta del eje X
     ylab = "Riqueza estimada", # Etiqueta del eje Y
     xlim = c(0, 1), # Límites del eje X (según los datos)
     ylim = c(0, 200)) # Límites del eje Y (según los datos)

# Polígono con los intervalos de confianza para la Comunidad 1
polygon(c(rare.C1$iNextEst$SC , rev(rare.C1$iNextEst$SC)), # Valores X del intervalo
        c(rare.C1$iNextEst$qD.LCL, rev(rare.C1$iNextEst$qD.UCL)), # Valores Y del intervalo
        col = rgb(222/255, 122/255, 122/255, 0.6), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Polígono con los intervalos de confianza para la Comunidad 2
polygon(c(rare.C2$iNextEst$SC , rev(rare.C2$iNextEst$SC)), # Valores X del intervalo
        c(rare.C2$iNextEst$qD.LCL, rev(rare.C2$iNextEst$qD.UCL)), # Valores Y del intervalo
        col = rgb(122/255, 170/255, 222/255, 0.6), # Color (en RGB) y transparencia del polígono
        border = NA) # No mostrar el borde del polígono

# Curva de acumulación basada en cobertura para la Comunidad 1 (valores interpolados)
lines(rare.C1$iNextEst[which(rare.C1$iNextEst$m <= abundances[1]), 7], # Valores de X
      rare.C1$iNextEst[which(rare.C1$iNextEst$m <= abundances[1]), 4], # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Curva de acumulación basada en cobertura para la Comunidad 1 (valores extrapolados)
lines(rare.C1$iNextEst[which(rare.C1$iNextEst$m > abundances[1]), 7], # Valores de X
      rare.C1$iNextEst[which(rare.C1$iNextEst$m > abundances[1]), 4], # Valores de Y
      lty = 2, # Tipo de línea (punteada)
      col = "red") # Color de la línea

# Curva de acumulación basada en cobertura para la Comunidad 2 (valores interpolados)
lines(rare.C2$iNextEst[which(rare.C2$iNextEst$m <= abundances[2]), 7], # Valores de X
      rare.C2$iNextEst[which(rare.C2$iNextEst$m <= abundances[2]), 4], # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Curva de acumulación basada en cobertura para la Comunidad 2 (valores extrapolados)
lines(rare.C2$iNextEst[which(rare.C2$iNextEst$m > abundances[2]), 7], # Valores de X
      rare.C2$iNextEst[which(rare.C2$iNextEst$m > abundances[2]), 4], # Valores de Y
      lty = 2, # Tipo de línea (punteada)
      col = "blue") # Color de la línea

# Línea punteada en el valor de cobertura mínima común 
abline(v = min(coverage), lty = 2)

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("C1", "C2"), # Texto de la leyenda
       lty = c(1, 1), # Tipos de línea de los símbolos
       col = c("red", "blue"), # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.05)) # Márgenes de la leyenda



# Vector para almacenar el número de especies por unidad de muestreo
S <- c()

# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(datosDiv)){ # Ciclo desde uno hasta el total de filas de los datos (comunidades)
  S.tmp <- length(which(datosDiv[i, ] > 0)) # Número de especies (columnas) con abundancia > 0 por unidad de muestreo
  S <- append(S, S.tmp) # Añadimos el número de especies del sitio i al vector de especies
}

# Índice de Gini-Simpson
Simpson <- diversity(datosDiv, index  = "simpson")

# Índice de Shannon
Shannon <- diversity(datosDiv, index = "shannon")

# Índice de Pielou
Pielou <- Shannon / log(S) 

# Combinamos los índices en una tabla
indices <- cbind(S = S, Simpson = Simpson, Shannon = Shannon, Pielou = Pielou)

# Transformamos los índices tradicionales en índices de diversidad verdadera
indices <- dplyr::mutate(as.datosDiv.frame(indices),
                         q0 = S, # Riqueza (q = 0)
                         q1 = exp(Shannon), # Exponencial de Shannon (q = 1)
                         q2 = 1 / (1 - Simpson)) # Inverso de Simpson (q = 2)




# Vector para almacenar el número de especies por unidad de muestreo
indices <- renyi(datosDiv, # Matriz de datos
                 scales = c(0, 1, 2), # Ordenes de diversidad
                 hill = TRUE) # Números de Hill (cuando es FALSE calcula entropías de Renyi)

# Nombre de las columnas de la tabla de índices
colnames(indices) <- c("q0", "q1", "q2")


# Tamaño (número de individuos) de cada comunidad a la misma cobertura de muestra
# (los valores de cobertura los tomamos de la variable coverage previamente calculada)
size.C1 <- Coverage2Size(datosDiv.t[, 1], SampleCoverage = min(coverage))
size.C2 <- Coverage2Size(datosDiv.t[, 2], SampleCoverage = min(coverage))

# Estimados de diversidad para cada comunidad a la misma cobertura de muestra
# (iNEXT no permite el calculo de un único tamaño, así que ponemos un cero que luego filtraremos)
C1.DivProf <- iNEXT(datosDiv.t[, 1], q = seq(0, 2, by = 0.2), size = c(0, size.C1))$iNextEst
C2.DivProf <- iNEXT(datosDiv.t[, 2], q = seq(0, 2, by = 0.2), size = c(0, size.C2))$iNextEst

# Filtro para eliminar los valores de cero que creamos en el paso anterior
C1.DivProf <- dplyr::filter(C1.DivProf, m > 0)
C2.DivProf <- dplyr::filter(C2.DivProf, m > 0)

# Lienzo vacío para el perfil de diversidad
plot(NA, # Lienzo sin datos
     xlab = "Órdenes", # Etiqueta del eje X
     ylab = "ENS", # Etiqueta del eje Y
     xlim = c(0, 2), # Límites del eje X (según los datos)
     ylim = c(0, 120)) # Límites del eje Y (según los datos)

# Perfil de la Comunidad 1
lines(x = C1.DivProf$order, # Valores de X
      y = C1.DivProf$qD, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Perfil de la Comunidad 2
lines(C2.DivProf$order, # Valores de X
      C2.DivProf$qD, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Puntos de la Comunidad 1
points(x = c(0, 1, 2), # Valores de X
       y = c(31.255, 24.994, 19.648), # Valores de Y
       pch = 19, # Forma del símbolo (19 = círculo)
       col = "red") # Color del símbolo

# Puntos de la Comunidad 2
points(x = c(0, 1, 2), # Valores de X
       y = c(112, 58.277, 22.549), # Valores de Y
       pch = 19, # Forma del símbolo (19 = círculo)
       col = "blue") # Color del símbolo

legend(x = "topright", # Posición
       legend = c("C1 (65%)", "C2 (65%)"), # Texto de la leyenda
       lty = c(1, 1), # Tipos de línea de los símbolos
       col = c("red", "blue"), # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.05)) # Márgenes de la leyenda



