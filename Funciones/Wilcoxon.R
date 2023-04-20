Wilcoxon<-function(grupo, variable, direccion, nombre){

    mannWhitney<-wilcox.test(variable~ grupo, paired = FALSE)
    print(mannWhitney)
  capture.output(nombre, "Kruskal-Wallis rank sum test",mannWhitney,file = direccion)

}