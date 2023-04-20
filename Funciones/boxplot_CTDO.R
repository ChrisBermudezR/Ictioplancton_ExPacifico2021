#' Title
#'
#' @param datos
#' @param transecto
#' @param variable
#' @param labelx
#' @param labely
#'
#' @return
#' @export
#'
#' @examples
boxplot_CTDO<-function(datos, transecto, variable, labelx, labely){

  ggplot(data=datos, aes(x=transecto, y=variable)) +
    geom_boxplot()+
    theme_bw()+geom_jitter(width=0.2,alpha=0.2) +
    stat_summary(fun=mean, aes(y = variable), geom="point", shape=20, size=4, color="red", position = position_dodge(width =0.8)) +
    labs( y = labely, x = labelx)
}
