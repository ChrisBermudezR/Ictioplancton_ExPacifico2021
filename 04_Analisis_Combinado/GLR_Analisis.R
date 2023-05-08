#Título del script: Análisis de Modelos de Rango Bajo Generalizados.
#Autores: Christian Bermúdez-Rivas 
#Objetivo: Realizar los análisis de los datos fisico químicos.
#Lenguaje: R
#Fecha: Junio 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################

library(dplyr)    # 
library(ggplot2)  # 
library(tidyr)    # 
library(bit64)

library(h2o)  # Para el ajuste de los modelos.

datos_GLR <- read.csv("./01_Datos/Datos_Totales_CCCP.csv")

datos_GLR <- datos_GLR[,11:42]
head(datos_GLR)
dim(datos_GLR)

h2o.no_progress()  # Apagar la barra de progreso
h2o.init(max_mem_size = "20g")  # conectarse a una instancia del h2o


# Convertir datos a un objeto h2o
datos_GLR.h2o <- as.h2o(datos_GLR)

# run basic GLRM
basico_glrm <- h2o.glrm(
  training_frame = datos_GLR.h2o,
  k = 20, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "None", 
  transform = "STANDARDIZE", 
  max_iterations = 2000,
  seed = 123
)
arrests_perf <- h2o.performance(basico_glrm)

summary(basico_glrm)
plot(basico_glrm)
str(basico_glrm)
basico_glrm@model$importance



data.frame(
  PC  = basico_glrm@model$importance %>% seq_along(),
  PVE = basico_glrm@model$importance %>% .[2,] %>% unlist(),
  CVE = basico_glrm@model$importance %>% .[3,] %>% unlist()
) %>%
  gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free")


#Extraer los arquetipos d lso modelos.

arquetipos<-as.data.frame(t(basico_glrm@model$archetypes))



#Plots
p1 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)



