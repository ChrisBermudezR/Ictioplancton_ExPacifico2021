#' Title
#'
#' @param datos
#' @param estacion
#' @param variable
#' @param labely
#'
#' @return
#' @export
#'
#' @examples
boxplot_profundidad<-function(datos, estacion, variable, labely){
  ggplot(data=datos) +
    geom_boxplot(aes(x=estacion, y=variable))+
    theme_bw()+
    stat_summary(fun=mean, aes(y = variable, x=estacion), geom="point", shape=20, size=2, color="red", position = position_dodge(width =0.8)) +
    labs(title = paste0("Histograma de la ",labely),
         y = labely, x = "Estaciones")+
    facet_grid(Marea~Transecto)
}
