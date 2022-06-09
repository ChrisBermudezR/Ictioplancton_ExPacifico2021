perfil_en_profundidad<-function(Estacion, Titulo, VarProfundidad, variable, labelx, labely, profundidadMax){
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(VarProfundidad)& !is.null(variable)& !is.null(labelx)& !is.null(labely)& !is.null(profundidadMax)) {
    ggplot(Estacion, aes(x=VarProfundidad, y=variable)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(profundidadMax,0))+
      scale_x_continuous(position = "top")+
      theme_bw()
  } else {
    print('Faltan Valores')
  }
}
