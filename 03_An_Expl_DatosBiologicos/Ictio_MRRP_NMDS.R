if(!require(vegan))install.packages("vegan")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(dplyr))install.packages("dplyr")


Codigo_Ictio<-read.table("./Biologicos/DatosP_Ictioplancton//Data_Ictio.csv", sep=",", header = TRUE)

Div_Code_Ictio<-Codigo_Ictio[,5:37]
row.names(Div_Code_Ictio) <- Codigo_Ictio[,1]


Ictio_groups_df<-Codigo_Ictio[2:4]
row.names(Ictio_groups_df)<-Codigo_Ictio$Especie
Ictio_groups_df$Transecto<-as.factor(Ictio_groups_df$Transecto)



Ictio_Densidad_Relativa <-         
  vegan::decostand(Div_Code_Ictio, method = "total")
# Calculate distance matrix
Ictio_Densidad_Relativa_distmat <- 
  vegdist(Ictio_Densidad_Relativa, method = "bray")

Ictio_Densidad_Relativa_distmat <- 
  as.matrix(Ictio_Densidad_Relativa_distmat, labels = T)
write.csv(Ictio_Densidad_Relativa_distmat, "./Resultados/Ictio_Densidad_Relativa_distmat.csv")



#Evaluación de la composición entre transectos sectores y mareas basados en al Densidad
Ictio_MRPP_Transecto_Densidad<-vegan::mrpp(dat = Ictio_Densidad_Relativa_distmat,  Codigo_Ictio$Transecto, permutations = 2000, distance = "bray")
Ictio_MRPP_Marea_Densidad<-vegan::mrpp(dat = Ictio_Densidad_Relativa_distmat,  Codigo_Ictio$Marea, permutations = 2000, distance = "bray")
Ictio_MRPP_Sector_Densidad<-vegan::mrpp(dat = Ictio_Densidad_Relativa_distmat,  Codigo_Ictio$Sector, permutations = 2000, distance = "bray")


capture.output("Comparación Densidad relativa - MRPP - Transectos",
               Ictio_MRPP_Transecto_Densidad,
               "MRPP - Marea",
               Ictio_MRPP_Marea_Densidad, 
               "MRPP - Sector",
               Ictio_MRPP_Sector_Densidad, 
               file = "./Resultados/Ictio_mrpp_resultados_ComposicionBasadaDensidad.txt")

# Running NMDS in vegan (metaMDS)
Ictio_Densidad_Relativa_NMS <-
  metaMDS(Ictio_Densidad_Relativa_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

capture.output("Resultados NMDS",
               Ictio_Densidad_Relativa_NMS, 
               file = "./Resultados/Ictio_NMS_Resultados.txt")

# Shepards test/goodness of fit
goodness(Ictio_Densidad_Relativa_NMS) # Produces a results of test statistics for goodness of fit for each point



stressplot(Ictio_Densidad_Relativa_NMS) # Produces a Shepards diagram


Ictio_coordenadas <- as.data.frame(scores(Ictio_Densidad_Relativa_NMS[["points"]]))
Ictio_coordenadas$Estaciones<-Codigo_Ictio$Especie
Ictio_coordenadas$Sector <- as.factor(groups_df$Sector)
Ictio_coordenadas$Transecto <- as.factor(groups_df$Transecto)
Ictio_coordenadas$Marea <- as.factor(groups_df$Marea)
colnames(Ictio_coordenadas)<-c("NMDS1",
                         "NMDS2",
                         "NMDS3",
                         "Estaciones",
                         "Sector",
                         "Transecto",
                         "Marea"
                         
)


Ictio_Marea_group <- Ictio_coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
             color=Marea,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

Ictio_hull_data <- 
  Ictio_coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Marea) %>% 
  slice(chull(NMDS1, NMDS2))

Ictio_Marea_group<-Ictio_Marea_group+
  geom_polygon(data = Ictio_hull_data,
               aes(fill = Marea,
                   colour = Marea),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = 0, y = -0.55,    label = as.character(expression('Stress: 0.07'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))

Ictio_Sector_group <- Ictio_coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
             color=Sector,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

Ictio_hull_data <- 
  Ictio_coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Sector) %>% 
  slice(chull(NMDS1, NMDS2))

Ictio_Sector_group<-Ictio_Sector_group+
  geom_polygon(data = Ictio_hull_data,
               aes(fill = Sector,
                   colour = Sector),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = 0, y = -0.55,    label = as.character(expression('Stress: 0.07'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))


Ictio_Transecto_group <- Ictio_coordenadas %>%
  ggplot(aes(x = NMDS1,
             y = NMDS2,
             color=Transecto,
             label=Estaciones))+
  geom_point()+geom_text(hjust=0, vjust=0)

Ictio_hull_data <- 
  Ictio_coordenadas %>%
  tidyr::drop_na() %>%
  group_by(Transecto) %>% 
  slice(chull(NMDS1, NMDS2))

Ictio_Transecto_group<-Ictio_Transecto_group+
  geom_polygon(data = Ictio_hull_data,
               aes(fill = Transecto,
                   colour = Transecto),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  geom_text(aes(x = 0, y = -0.55,    label = as.character(expression('Stress: 0.07'))), stat = "unique",
            size = 5, color = "black", parse = T)+
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18))

####Stresplot

str(stressplot(Ictio_Densidad_Relativa_NMS))

# Create a tibble that contains the data from stressplot
Ictio_StresPlot1 <- tibble(x = stressplot(Ictio_Densidad_Relativa_NMS)$x,
                     y = stressplot(Ictio_Densidad_Relativa_NMS)$y,
                     yf = stressplot(Ictio_Densidad_Relativa_NMS)$yf) %>%
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
  geom_text(aes(x = 0.45, y = 0.75,    label = as.character(expression('Non-metric fit: '~ R^2~ ' = 0.995'))), stat = "unique",
            size = 6, color = "black", parse = T)+
  geom_text(aes(x = 0.45, y = 0.7,    label = as.character(expression('Linear fit: '~ R^2~ ' = 0.99'))), stat = "unique",
            size = 6, color = "black", parse = T)+
  theme_bw()








png(filename="./Imagenes/Ictio_NMDS_Total.png", height =30 , width = 30, units = "cm", res=400)
gridExtra:: grid.arrange(StresPlot_DEf,
                         Ictio_Marea_group,
                         Ictio_Sector_group,
                         Ictio_Transecto_group,
                         ncol=2)
dev.off()






Densidad_Relativa_scrs <- 
  sppscores(Densidad_Relativa_NMS) <- Densidad_Relativa

Densidad_Relativa_cor <-  cor(Densidad_Relativa,  Densidad_Relativa_NMS$points,  use = "complete.obs",    method = "pearson")
write.csv(Densidad_Relativa_cor, file = "./Resultados/Ictio_Densidad_Relativa_NMS.csv") 


