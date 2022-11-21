for (i in 1:30){
  print(paste0(variables[i], "_boxplot_Mareas<-boxplot_Marea(Datos_Fisica_Sup, Datos_Fisica_Sup$",variables[i],", ","'",etiqueta_para_y[i],"'",")"))
}

for (i in 1:30){
  print(paste0(variables[i], "_boxplot_Mareas<-boxplot_Marea(Datos_Fisica_Sup, Datos_Fisica_Sup$",variables[i],", ","'",etiqueta_para_y[i],"'",")"))
}




for (i in seq_along(Datos_Fisica_Sup[,3:33])){
  assign(paste0(Datos_Fisica_Sup[,i], "Boxplot_Mareas"),boxplot_Marea(Datos_Fisica_Sup, Datos_Fisica_Sup$variables[i],etiqueta_para_y[i]))
}



Salinidad_max

Etiqueta_Densidad<-paste0(Densidad~" "~"[Kg/" ~ m^3~"]")


assign(paste0(niveles_Codigo[[i]], "_CCCP"),dplyr::filter(Datos_CTDO_CCCP, Codigo==niveles_Codigo[[i]]))

variables<-colnames(Datos_Fisica_Sup[4:33])
etiqueta_para_y<-c(
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]",
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]",
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]",
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]",
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]",
  "Temperatura [°C]",
  "Salinidad [PSU]",
  "Oxígeno disuelto [mg/L]",
  "XXXXXIntroducir Expresión",
  "Profundidad [m]"
)

nombre<-as.data.frame(variables, etiqueta_para_y)
