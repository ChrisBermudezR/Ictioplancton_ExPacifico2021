#' Title
#'
#' @param datos
#' @param marea
#' @param transecto
#' @param variable
#' @param labelx
#'
#' @return
#' @export
#'
#' @examples
histograma_Transecto<-function(datos, marea, transecto, variable, labelx){
  ggplot(data=Datos_CTDO_CCCP, aes(x=variable)) +
    geom_histogram(aes(group=marea))+
    labs(title = paste0("Histograma de la ",labelx ),
         subtitle = "(Distribución por Transectos)",
         y = "Frecuencia", x = labelx)+
    facet_grid(Marea ~ Transecto)
}
