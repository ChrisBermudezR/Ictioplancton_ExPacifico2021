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
boxplot_Marea<-function(datos,variable, y_etiqueta){
  ggplot(data=datos, aes(x=Marea, y=variable, color=Marea)) +
    geom_boxplot()+
    stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
    labs( y = y_etiqueta, x = "Marea")+
    scale_x_discrete(limits=c("Alta","Baja"))+
    scale_color_manual(values=c("chocolate1", "deepskyblue"))+
    theme_bw()+
    geom_point(position = position_jitterdodge())+
    theme(legend.position = "none", legend.title = element_blank()) +
    guides(fill=guide_legend(reverse=TRUE))
}
