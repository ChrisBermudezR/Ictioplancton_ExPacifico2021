#Título del script: Análisis de Modelos de Rango Bajo Generalizados.
#Autores: Christian Bermúdez-Rivas 
#Objetivo: Realizar los análisis de los datos fisico químicos.
#Lenguaje: R
#Fecha: Junio 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################



if(!require(dplyr))install.packages("dplyr")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(tidyr))install.packages("tidyr")
if(!require(bit64))install.packages("bit64")# Para el ajuste de los modelos.
if(!require(h2o))install.packages("h2o")# Para el ajuste de los modelos.


datos_GLR <- utils::read.csv("./01_Datos/Datos_Totales_CCCP.csv")



datos_GLR <- datos_GLR[,12:31]
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
Arch1 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

Arch2 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch2, reorder(feature, Arch2))) +
  geom_point()
Arch3 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch3, reorder(feature, Arch3))) +
  geom_point()


p1 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

p2 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch3, label = feature)) +
  geom_text()
p3 <- t(basico_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch2, Arch3, label = feature)) +
  geom_text()

gridExtra::grid.arrange(Arch1, Arch2, Arch3, p1,p2,p3, nrow = 2, ncol=3)


ArchCor <- t(basico_glrm@model$archetypes)

write.table(ArchCor, "GLRM_Archetipes.csv", dec = ".", sep=",", row.names = TRUE)



png(filename = "./02_Imagenes/Arch_Basi_k20.png",width = 20, height = 20, units = "cm", res=300, pointsize = 0.1)
gridExtra::grid.arrange(Arch1, p1, Arch2, p2, Arch3, p3, nrow = 3, ncol=2)
dev.off()



# Re-run model with k = 8
k8_glrm <- h2o.glrm(
  training_frame = datos_GLR.h2o,
  k = 6, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "None", 
  transform = "STANDARDIZE", 
  max_iterations = 2000,
  seed = 123
)
my_reconstruction <- h2o.reconstruct(k8_glrm, datos_GLR.h2o, reverse_transform = TRUE)
my_reconstruction[1:5, 1:5]

Data_Reconstruida<-as.data.frame(my_reconstruction)
data_propia<-as.data.frame(datos_GLR.h2o)



plot(abs(Data_Reconstruida$reconstr_Salinidad_Su), data_propia$Salinidad_Sup)
cor.test(abs(Data_Reconstruida$reconstr_Salinidad_Su), data_propia$Salinidad_Sup)

shapiro.test(Data_Reconstruida$reconstr_Salinidad_Su)
shapiro.test(log10(abs(Data_Reconstruida$reconstr_Temperatura_IQR)))
shapiro.test(log10(Data_Reconstruida$reconstr_Temperatura_IQR))
shapiro.test(log10(NO2))
