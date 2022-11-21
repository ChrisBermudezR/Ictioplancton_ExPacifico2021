

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

Codigo_fito<-read.table("./Biologicos/DatosP_Fitoplancton/Matriz_Codigos_Fito.csv", sep=",", header = TRUE,)
Div_Code_fito<-Codigo_fito[,2:190]
row.names(Div_Code_fito) <- Codigo_fito[["Etiquetas.de.fila"]]
Codigo_fito_conteo<-read.table("./Biologicos/DatosP_Fitoplancton/Cod_Fito_Conteo.csv", sep=",", header = TRUE,)
Codigo_fito_conteo2<-Codigo_fito_conteo[,2:190]
row.names(Codigo_fito_conteo2) <- Codigo_fito_conteo[["Codigo"]]

#Inext####
# Matriz de datos transpuesta
datosDiv.t <- as.data.frame(t(Div_Code_fito))

# Comando general de iNEXT (calcula muchas cosas)
est.Comms <- iNEXT(datosDiv.t, 
                   q = 0, 
                   datatype = "abundance") # q = 0 es la riqueza (diversidades verdaderas)



# Cobertura de muestra C1
SC.C1 <- est.Comms$iNextEst$size_based[which(est.Comms$iNextEst$size_based$Method == "observed"), 7]

# Cobertura de muestra C2
SC.C2 <- est.Comms$iNextEst$coverage_based[which(est.Comms$iNextEst$coverage_based$Method == "observed"), 7]

# Añadimos las coberturas a una tabla
coverage <- cbind(C1 = SC.C1, C2 = SC.C2) 



# Abundancias totales por comunidad
abundances <- colSums(datosDiv.t)

# Valor máximo para extrapolar (no más del doble de las abundancias)
max.Extrapol <- abundances * 2

#Estimadores para cada comunidad####
rare.C1 <- iNEXT(datosDiv.t[, 1], q = 0, datatype = "abundance", knots = max.Extrapol[1])
rare.C2 <- iNEXT(datosDiv.t[, 2], q = 0, datatype = "abundance", knots = max.Extrapol[2])



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



