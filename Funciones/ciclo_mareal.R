#' Grafica del ciclo mareal.
#'
#' @param datos: Conjunto de datos de donde se obtiene el ciclo mareal.
#' @param fecha_hora: Vector o columna con los datos de la fecha y la hora guardados como as.POSIXct(). Se debe usar el pacquete "scales"
#' @param altura: vector o comna con los datos de la Altura mareal referída al MLWS (m).
#'
#' @return
#' @export
#'
#' @examples
ciclo_mareal<-function(datos, fecha_hora, altura){
  ggplot()+
    geom_line(data=datos, aes(x=fecha_hora, y=altura),size=1, colour="grey")+
    geom_hline(yintercept = 1:3,linetype='dotted', col = 'red')+
    labs(x = "Fecha", y = "Altura mareal referída al MLWS [m]") +
    theme_classic()+
    scale_x_datetime(
      breaks = seq(as.POSIXct("2021-04-29 00:00:00"),
                   as.POSIXct("2021-05-05 00:00:00"), "6 hours"),
      labels = date_format("%a-%d\n%H:%M", tz = ""),
      expand = c(0, 0),
      limits = c(
        as.POSIXct("2021-04-29 00:00:00"),
        as.POSIXct("2021-05-05 00:00:00")))
}
