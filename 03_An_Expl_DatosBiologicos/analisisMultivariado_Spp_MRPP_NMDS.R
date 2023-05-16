#Analisis de la comunidad
library(vegan)
library(ggplot2)
library(dplyr)

Codigo_fito_Densidad<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Densidad.csv", sep=",", header = TRUE)

Fito_Densidad_df<-Codigo_fito_Densidad[6:145]
row.names(Fito_Densidad_df)<-Codigo_fito_Densidad$Estaciones


groups_df<-Codigo_fito_Densidad[2:4]
row.names(groups_df)<-Codigo_fito_Densidad$Estaciones
groups_df$Transecto<-as.factor(groups_df$Transecto)


# Calculating relative abundance and creating new dataframe with relative abundance data
Densidad_Relativa <-         
  vegan::decostand(Fito_Densidad_df, method = "total")
# Calculate distance matrix
Densidad_Relativa_distmat <- 
  vegdist(Densidad_Relativa, method = "bray")

Densidad_Relativa_distmat <- 
  as.matrix(Densidad_Relativa_distmat, labels = T)
write.csv(Densidad_Relativa_distmat, "./Resultados/Densidad_Relativa_distmat.csv")



#Evaluación de la composición entre transectos sectores y mareas basados en al Densidad
MRPP_Transecto_Densidad<-vegan::mrpp(dat = Densidad_Relativa_distmat,  Codigo_fito_Densidad$Transecto, permutations = 2000, distance = "bray")
MRPP_Marea_Densidad<-vegan::mrpp(dat = Densidad_Relativa_distmat,  Codigo_fito_Densidad$Marea, permutations = 2000, distance = "bray")
MRPP_Sector_Densidad<-vegan::mrpp(dat = Densidad_Relativa_distmat,  Codigo_fito_Densidad$Sector, permutations = 2000, distance = "bray")


capture.output("Comparación Densidad relativa - MRPP - Transectos",
               MRPP_Transecto_Densidad,
               "MRPP - Marea",
               MRPP_Marea_Densidad, 
               "MRPP - Sector",
               MRPP_Sector_Densidad, 
               file = "./Resultados/mrpp_resultados_ComposicionBasadaDensidad.txt")

# Running NMDS in vegan (metaMDS)
Densidad_Relativa_NMS <-
  metaMDS(Densidad_Relativa_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

capture.output("Resultados NMDS",
               Densidad_Relativa_NMS, 
               file = "./Resultados/NMS_Resultados.txt")

# Shepards test/goodness of fit
goodness(Densidad_Relativa_NMS) # Produces a results of test statistics for goodness of fit for each point



stressplot(Densidad_Relativa_NMS) # Produces a Shepards diagram


coordenadas <- as.data.frame(scores(Densidad_Relativa_NMS[["points"]]))
coordenadas$Estaciones<-Codigo_fito_Densidad$Estaciones
coordenadas$Sector <- as.factor(groups_df$Sector)
coordenadas$Transecto <- as.factor(groups_df$Transecto)
coordenadas$Marea <- as.factor(groups_df$Marea)
colnames(coordenadas)<-c("NMDS1",
                         "NMDS2",
                         "NMDS3",
                         "Estaciones",
                         "Sector",
                         "Transecto",
                         "Marea"
  
)



Marea_group <- coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
              color=Marea,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

hull_data <- 
  coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Marea) %>% 
  slice(chull(NMDS1, NMDS2))

Marea_group<-Marea_group+
geom_polygon(data = hull_data,
             aes(fill = Marea,
                 colour = Marea),
             alpha = 0.3,
             show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = -0.45, y = -0.55,    label = as.character(expression('Stress: 0.12'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))

Sector_group <- coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
             color=Sector,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

hull_data <- 
  coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Sector) %>% 
  slice(chull(NMDS1, NMDS2))

Sector_group<-Sector_group+
  geom_polygon(data = hull_data,
               aes(fill = Sector,
                   colour = Sector),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = -0.45, y = -0.55,    label = as.character(expression('Stress: 0.12'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))


Transecto_group <- coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
             color=Transecto,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

hull_data <- 
  coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Transecto) %>% 
  slice(chull(NMDS1, NMDS2))

Transecto_group<-Transecto_group+
  geom_polygon(data = hull_data,
               aes(fill = Transecto,
                   colour = Transecto),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = -0.45, y = -0.55,    label = as.character(expression('Stress: 0.12'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))

####Stresplot

str(stressplot(Densidad_Relativa_NMS))

# Create a tibble that contains the data from stressplot
StresPlot1 <- tibble(x = stressplot(Densidad_Relativa_NMS)$x,
             y = stressplot(Densidad_Relativa_NMS)$y,
             yf = stressplot(Densidad_Relativa_NMS)$yf) %>%
  # Change data to long format
  tidyr::pivot_longer(cols = c(y, yf),
               names_to = "var")
################################################

 

StresPlot_DEf<-StresPlot1 %>%
  ggplot(aes(x = x,
             y = value)) +
  # Add points just for y values
  geom_point(data = StresPlot1 %>%
               dplyr:: filter(var == "y"),
             col="blue"
  ) +
  # Add line just for yf values
  geom_step(data = StresPlot1 %>%
              dplyr::filter(var == "yf"),
            col = "red",
            direction = "vh") +
  # Change axis labels
  labs(x = "Observed Dissimilarity", y = "Ordination Distance") +
  geom_text(aes(x = 0.45, y = 0.75,    label = as.character(expression('Non-metric fit: '~ R^2~ ' = 0.985'))), stat = "unique",
            size = 6, color = "black", parse = T)+
  geom_text(aes(x = 0.45, y = 0.7,    label = as.character(expression('Linear fit: '~ R^2~ ' = 0.921'))), stat = "unique",
            size = 6, color = "black", parse = T)+
  theme_bw()








png(filename="./Imagenes/Fito_NMDS_Total.png", height =30 , width = 30, units = "cm", res=400)
gridExtra:: grid.arrange(StresPlot_DEf,
                         Marea_group,
                         Sector_group,
                         Transecto_group,
                         ncol=2)
dev.off()






Densidad_Relativa_scrs <- 
  sppscores(Densidad_Relativa_NMS) <- Densidad_Relativa

Densidad_Relativa_cor <-  cor(Densidad_Relativa,  Densidad_Relativa_NMS$points,  use = "complete.obs",    method = "pearson")
write.csv(Densidad_Relativa_cor, file = "./Resultados/Densidad_Relativa_NMS.csv") 


