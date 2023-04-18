#Analisis de la comunidad


Codigo_fito_abundancia<-read.table("./Biologicos/DatosP_Fitoplancton/Definitiva/Matriz_Abundancia.csv", sep=",", header = TRUE)

Fito_abundancia_df<-Codigo_fito_abundancia[6:145]
row.names(Fito_abundancia_df)<-Codigo_fito_abundancia$Estaciones


groups_df<-Codigo_fito_abundancia[2:4]
row.names(groups_df)<-Codigo_fito_abundancia$Estaciones
groups_df$Transecto<-as.factor(groups_df$Transecto)


# Calculating relative abundance and creating new dataframe with relative abundance data
Abundancia_Relativa <-         
  vegan::decostand(Fito_abundancia_df, method = "total")

#Evaluación de la composición entre transectos sectores y mareas basados en al abundancia
MRPP_Transecto_Abundancia<-vegan::mrpp(dat = Abundancia_Relativa,  Codigo_fito_abundancia$Transecto, permutations = 2000)
MRPP_Marea_Abundancia<-vegan::mrpp(dat = Abundancia_Relativa,  Codigo_fito_abundancia$Marea, permutations = 2000)
MRPP_Sector_Abundancia<-vegan::mrpp(dat = Abundancia_Relativa,  Codigo_fito_abundancia$Sector, permutations = 2000)


capture.output("Comparación abundancia relativa - MRPP - Transectos",
               MRPP_Transecto_Abundancia,
               "MRPP - Marea",
               MRPP_Marea_Abundancia, 
               "MRPP - Sector",
               MRPP_Sector_Abundancia, 
               file = "./Resultados/mrpp_resultados_ComposicionBasadaAbundancia.txt")





# Calculate distance matrix
Abundancia_Relativa_distmat <- 
  vegdist(Abundancia_Relativa, method = "bray")


Abundancia_Relativa_distmat <- 
  as.matrix(Abundancia_Relativa_distmat, labels = T)
write.csv(Abundancia_Relativa_distmat, "./Resultados/Abundancia_Relativa_distmat.csv")

# Running NMDS in vegan (metaMDS)
Abundancia_Relativa_NMS <-
  metaMDS(Abundancia_Relativa_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)



# Shepards test/goodness of fit
goodness(Abundancia_Relativa_NMS) # Produces a results of test statistics for goodness of fit for each point



stressplot(Abundancia_Relativa_NMS) # Produces a Shepards diagram


Marea_group <- coordenadas %>%
  ggplot(aes(x = MDS1,
             y = MDS2
             , color=Marea))+
  geom_point()

coordenadas <- as.data.frame(scores(Abundancia_Relativa_NMS[["points"]]))
coordenadas$Sector <- as.factor(groups_df$Sector)
coordenadas$Transecto <- as.factor(groups_df$Transecto)
coordenadas$Marea <- as.factor(groups_df$Marea)

library(ggplot2)


Marea_group <- coordenadas %>%
  ggplot(aes(x = MDS1,
             y = MDS2
             , color=Marea))+
  geom_point()

hull_data <- 
  coordenadas %>%
  drop_na() %>%
  group_by(Marea) %>% 
  slice(chull(MDS1, MDS2))

Marea_group<-Marea_group+
geom_polygon(data = hull_data,
             aes(fill = Marea,
                 colour = Marea),
             alpha = 0.3,
             show.legend = FALSE)+
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

Sector_group <- coordenadas %>%
  ggplot(aes(x = MDS1,
             y = MDS2
             , color=Sector))+
  geom_point()

hull_data <- 
  coordenadas %>%
  drop_na() %>%
  group_by(Sector) %>% 
  slice(chull(MDS1, MDS2))

Sector_group<-Sector_group+
  geom_polygon(data = hull_data,
               aes(fill = Sector,
                   colour = Sector),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


Transecto_group <- coordenadas %>%
  ggplot(aes(x = MDS1,
             y = MDS2
             , color=Transecto))+
  geom_point()

hull_data <- 
  coordenadas %>%
  drop_na() %>%
  group_by(Transecto) %>% 
  slice(chull(MDS1, MDS2))

Transecto_group<-Transecto_group+
  geom_polygon(data = hull_data,
               aes(fill = Transecto,
                   colour = Transecto),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

####Stresplot

str(stressplot(Abundancia_Relativa_NMS))

# Create a tibble that contains the data from stressplot
StresPlot1 <- tibble(x = stressplot(Abundancia_Relativa_NMS)$x,
             y = stressplot(Abundancia_Relativa_NMS)$y,
             yf = stressplot(Abundancia_Relativa_NMS)$yf) %>%
  # Change data to long format
  pivot_longer(cols = c(y, yf),
               names_to = "var")
################################################
# Create plot
StresPlot1 %>%
  ggplot(aes(x = x,
             y = value)) +
  # Add points just for y values
  geom_point(data = df %>%
               filter(var == "y"),
             col="blue"
             ) +
  # Add line just for yf values
  geom_step(data = df %>%
              filter(var == "yf"),
            col = "red",
            direction = "vh") +
  # Change axis labels
  labs(x = "Observed Dissimilarity", y = "Ordination Distance") +
  geom_text(aes(x = 0.35, y = 0.75,    label = paste0(expression("Non-metric R"^2))), stat = "unique",
            size = 4, color = "darkgrey")+
  theme_bw()
  
  
  
    geom_text(aes(0.35,0.75, label = (paste("Non Metric"))),colour="gray20",parse = TRUE)+
  geom_text(aes(0.35,0.6, label =""),parse = TRUE)+
  theme_bw()







png(filename="./Imagenes/NMDS_Total.png", height =35 , width = 25, units = "cm", res=400)
gridExtra:: grid.arrange(plot(as.raster(stressplor1)),
                         Marea_group,
                         Sector_group,
                         Transecto_group,
                         ncol=2)
dev.off()



Abundancia_Relativa_scrs <- 
  sppscores(Abundancia_Relativa_NMS) <- Abundancia_Relativa

Abundancia_Relativa_cor <-  cor(Abundancia_Relativa,  Abundancia_Relativa_NMS$points,  use = "complete.obs",    method = "pearson")
write.csv(Abundancia_Relativa_cor, file = "./Resultados/Abundancia_Relativa_NMS.csv") 




#####Alternativas de plotting




plot(Abundancia_Relativa_NMS, type = "n")
points(Abundancia_Relativa_NMS$points, pch = as.numeric(groups_df$Transecto), col = as.numeric(groups_df$Transecto),  cex = 1.5) 
legend(x = "topright", legend = c("Cloud forest", "Oak forest",  "Pine forest"),  pch = c(1:3), col = c(1:3))



ordiplot (Abundancia_Relativa_NMS, display = 'sites', type = 'n')
points (Abundancia_Relativa_NMS, col = as.numeric(groups_df$Transecto), pch = as.numeric(groups_df$Transecto))
for (i in unique (groups_df$Transecto)) ordihull (Abundancia_Relativa_NMS, groups = as.numeric(groups_df$Transecto, show.group = i, col = i), draw = 'polygon', label = T)




groups_df$Transecto<-as.numeric(groups_df$Transecto)

plot(Abundancia_Relativa_NMS)
with(groups_df,
     points(Abundancia_Relativa_NMS,
            display = "sites",
            col = "black",
            pch = pchvec[Transecto],
            bg = colvec[Transecto]))

#Create convex hulls that highlight point clusters based on grouping dataframe
ordihull(
  Abundancia_Relativa_NMS,
  groups_df$Transecto,
  display = "sites",
  draw = c("polygon"),
  col = NULL,
  border = c("gray0", "gray0", "gray48", "gray48"),
  lty = c(1, 2, 1, 2),
  lwd = 2.5
)
for (i in unique (groups_df$Transecto)) ordihull (Abundancia_Relativa_NMS, groups = as.numeric(groups_df$Transecto, show.group = i, col = i), draw = 'polygon', label = T)












