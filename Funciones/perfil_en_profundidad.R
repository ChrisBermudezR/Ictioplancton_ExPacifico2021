#' Title
#'
#' @param Datos
#' @param CodigoEstacion
#' @param Marea
#' @param variable
#' @param VarProfundidad
#' @param labelx
#' @param labely
#'
#' @return
#' @export
#'
#' @examples
perfil_en_profundidad<-function(Datos, CodigoEstacion, Marea, variable, VarProfundidad, labelx, labely){
  if (!is.null(CodigoEstacion) & !is.null(Marea)& !is.null(VarProfundidad)& !is.null(variable)& !is.null(labelx)& !is.null(labely)) {
    ggplot(data=Datos, aes(x=variable, y=VarProfundidad)) +
      geom_path(size=0.5)+
      labs(title= paste0("Estación ",CodigoEstacion," - Marea ", Marea), x= labelx, y=labely)+
      scale_y_reverse(lim=c(max(VarProfundidad),0))+
      scale_x_continuous(position = "top")+
      theme_bw()
  } else {
    print('Faltan Valores')
  }
}
