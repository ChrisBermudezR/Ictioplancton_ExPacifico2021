#' Title
#'
#' @param datos
#' @param variable
#' @param y_etiqueta
#'
#' @return
#' @export
#'
#' @examples
graf_lineas<-function(datos,variable, y_etiqueta){

  ggplot(datos, aes(x=No.Estacion, y=variable)) +
    geom_line()+
    geom_point()+
    labs( y = y_etiqueta)+
    scale_x_discrete(name ="Transecto",
                     limits=c("1", "2", "3", "4", "5", "6"))+
    theme_bw()+
    facet_grid(Marea~Transecto)
}
