#Titulo: Visualización y Análisis descriptivo - CCCP
#Autor: Christian Bermúdez-Rivas
#Objectivo: Visualización y análisis descriptivo de los datos físicos obtenidos de la sonda CDT-O General Oceanics 19v PLUS.
#Lenguaje de programación: R
#Fecha: December 2021
#Notas: 
###############################################################################################################################



#Este script tiene cómo objetivo realizar las gráficas del componente físico de la Expedición Pacífico.

library(ggplot2)
library(gridExtra)


datos<-read.table("ExpPacifico2021.csv", header=TRUE, sep=",")
datos$Estacion<-as.factor(datos$Estacion)
datos$Marea<-as.factor(datos$Marea)



Subconjunto<-function(datos, estacion)
{subset(datos, Estacion == estacion, select = c("Fecha", "Hora", "Latitud", "Longitud", "Estacion", "Marea", "Profundidad",
                                            "Temperatura", "Salinidad", "Conductividad", "Oxigeno", "Densidad", "Vel.Sonido"))}


A06A<-Subconjunto(datos, "A06A")
A05A<-Subconjunto(datos, "A05A")
A04A<-Subconjunto(datos, "A04A")
A03A<-Subconjunto(datos, "A03A")
A02A<-Subconjunto(datos, "A02A")
A01A<-Subconjunto(datos, "A01A")

A06B<-Subconjunto(datos, "A06B")
A05B<-Subconjunto(datos, "A05B")
A04B<-Subconjunto(datos, "A04B")
A03B<-Subconjunto(datos, "A03B")
A02B<-Subconjunto(datos, "A02B")
A01B<-Subconjunto(datos, "A01B")


S06A<-Subconjunto(datos, "S06A")
S05A<-Subconjunto(datos, "S05A")
S04A<-Subconjunto(datos, "S04A")
S03A<-Subconjunto(datos, "S03A")
S02A<-Subconjunto(datos, "S02A")
S01A<-Subconjunto(datos, "S01A")

S06B<-Subconjunto(datos, "S06B")
S05B<-Subconjunto(datos, "S05B")
S04B<-Subconjunto(datos, "S04B")
S03B<-Subconjunto(datos, "S03B")
S02B<-Subconjunto(datos, "S02B")
S01B<-Subconjunto(datos, "S01B")

G06A<-Subconjunto(datos, "G06A")
G05A<-Subconjunto(datos, "G05A")
G04A<-Subconjunto(datos, "G04A")
G03A<-Subconjunto(datos, "G03A")
G02A<-Subconjunto(datos, "G02A")
G01A<-Subconjunto(datos, "G01A")

G06B<-Subconjunto(datos, "G06B")
G05B<-Subconjunto(datos, "G05B")
G04B<-Subconjunto(datos, "G04B")
G03B<-Subconjunto(datos, "G03B")
G02B<-Subconjunto(datos, "G02B")
G01B<-Subconjunto(datos, "G01B")


Estandar_75<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(75,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}

Estandar_15<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(15,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}
Estandar_40<-function(Estacion, Titulo, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) & !is.null(Titulo)& !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_path(size=0.5)+
      labs(title= Titulo, x= labelx, y=labely)+
      scale_y_reverse(lim=c(40,0))+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}

  
Temp_A06A<-Estandar_15(A06A, "A06 - Marea Alta", A06A$Temperatura, A06A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A06B<-Estandar_15(A06B, "A06 - Marea Baja", A06B$Temperatura, A06B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05A<-Estandar_15(A05A, "A05 - Marea Alta", A05A$Temperatura, A05A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A05B<-Estandar_15(A05B, "A05 - Marea Baja", A05B$Temperatura, A05B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04A<-Estandar_15(A04A, "A04 - Marea Alta", A04A$Temperatura, A04A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A04B<-Estandar_15(A04B, "A04 - Marea Baja", A04B$Temperatura, A04B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03A<-Estandar_40(A03A, "A03 - Marea Alta", A03A$Temperatura, A03A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A03B<-Estandar_40(A03B, "A03 - Marea Baja", A03B$Temperatura, A03B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02A<-Estandar_75(A02A, "A02 - Marea Alta", A02A$Temperatura, A02A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A02B<-Estandar_75(A02B, "A02 - Marea Baja", A02B$Temperatura, A02B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01A<-Estandar_75(A01A, "A01 - Marea Alta", A01A$Temperatura, A01A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_A01B<-Estandar_75(A01B, "A01 - Marea Baja", A01B$Temperatura, A01B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Amarales.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_A01A, Temp_A01B, Temp_A02A,Temp_A02B, Temp_A03A, Temp_A03B, Temp_A04A, Temp_A04B, Temp_A05A,Temp_A05B, Temp_A06A, Temp_A06B, top="Boca Amarales")
dev.off()


Sal_A06A<-Estandar_15(A06A, "A06 - Marea Alta", A06A$Salinidad, A06A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A06B<-Estandar_15(A06B, "A06 - Marea Baja", A06B$Salinidad, A06B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05A<-Estandar_15(A05A, "A05 - Marea Alta", A05A$Salinidad, A05A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A05B<-Estandar_15(A05B, "A05 - Marea Baja", A05B$Salinidad, A05B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04A<-Estandar_15(A04A, "A04 - Marea Alta", A04A$Salinidad, A04A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A04B<-Estandar_15(A04B, "A04 - Marea Baja", A04B$Salinidad, A04B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03A<-Estandar_40(A03A, "A03 - Marea Alta", A03A$Salinidad, A03A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A03B<-Estandar_40(A03B, "A03 - Marea Baja", A03B$Salinidad, A03B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02A<-Estandar_75(A02A, "A02 - Marea Alta", A02A$Salinidad, A02A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A02B<-Estandar_75(A02B, "A02 - Marea Baja", A02B$Salinidad, A02B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01A<-Estandar_75(A01A, "A01 - Marea Alta", A01A$Salinidad, A01A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_A01B<-Estandar_75(A01B, "A01 - Marea Baja", A01B$Salinidad, A01B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "Salinidad_Boca_Amarales.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_A01A, Sal_A01B, Sal_A02A,Sal_A02B, Sal_A03A, Sal_A03B, Sal_A04A, Sal_A04B, Sal_A05A,Sal_A05B, Sal_A06A, Sal_A06B, top="Boca Amarales")
dev.off()

Den_A06A<-Estandar_15(A06A, "A06 - Marea Alta", A06A$Densidad, A06A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A06B<-Estandar_15(A06B, "A06 - Marea Baja", A06B$Densidad, A06B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05A<-Estandar_15(A05A, "A05 - Marea Alta", A05A$Densidad, A05A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A05B<-Estandar_15(A05B, "A05 - Marea Baja", A05B$Densidad, A05B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04A<-Estandar_15(A04A, "A04 - Marea Alta", A04A$Densidad, A04A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A04B<-Estandar_15(A04B, "A04 - Marea Baja", A04B$Densidad, A04B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03A<-Estandar_40(A03A, "A03 - Marea Alta", A03A$Densidad, A03A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A03B<-Estandar_40(A03B, "A03 - Marea Baja", A03B$Densidad, A03B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02A<-Estandar_75(A02A, "A02 - Marea Alta", A02A$Densidad, A02A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A02B<-Estandar_75(A02B, "A02 - Marea Baja", A02B$Densidad, A02B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01A<-Estandar_75(A01A, "A01 - Marea Alta", A01A$Densidad, A01A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_A01B<-Estandar_75(A01B, "A01 - Marea Baja", A01B$Densidad, A01B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "Densidad_Boca_Amarales.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_A01A, Den_A01B, Den_A02A,Den_A02B, Den_A03A, Den_A03B, Den_A04A, Den_A04B, Den_A05A,Den_A05B, Den_A06A, Den_A06B, top="Boca Amarales")
dev.off()

Oxi_A06A<-Estandar_15(A06A, "A06 - Marea Alta", A06A$Oxigeno, A06A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A06B<-Estandar_15(A06B, "A06 - Marea Baja", A06B$Oxigeno, A06B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05A<-Estandar_15(A05A, "A05 - Marea Alta", A05A$Oxigeno, A05A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A05B<-Estandar_15(A05B, "A05 - Marea Baja", A05B$Oxigeno, A05B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04A<-Estandar_15(A04A, "A04 - Marea Alta", A04A$Oxigeno, A04A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A04B<-Estandar_15(A04B, "A04 - Marea Baja", A04B$Oxigeno, A04B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03A<-Estandar_40(A03A, "A03 - Marea Alta", A03A$Oxigeno, A03A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A03B<-Estandar_40(A03B, "A03 - Marea Baja", A03B$Oxigeno, A03B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02A<-Estandar_75(A02A, "A02 - Marea Alta", A02A$Oxigeno, A02A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A02B<-Estandar_75(A02B, "A02 - Marea Baja", A02B$Oxigeno, A02B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01A<-Estandar_75(A01A, "A01 - Marea Alta", A01A$Oxigeno, A01A$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")
Oxi_A01B<-Estandar_75(A01B, "A01 - Marea Baja", A01B$Oxigeno, A01B$Profundidad, "Oxígeno - [mg/L]", "Profundidad [m]")


tiff(filename = "Oxigeno_Boca_Amarales.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Oxi_A01A, Oxi_A01B, Oxi_A02A,Oxi_A02B, Oxi_A03A, Oxi_A03B, Oxi_A04A, Oxi_A04B, Oxi_A05A,Oxi_A05B, Oxi_A06A, Oxi_A06B, top="Boca Amarales")
dev.off()



Temp_S06A<-Estandar_15(S06A, "S06 - Marea Alta", S06A$Temperatura, S06A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S06B<-Estandar_15(S06B, "S06 - Marea Baja", S06B$Temperatura, S06B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05A<-Estandar_15(S05A, "S05 - Marea Alta", S05A$Temperatura, S05A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S05B<-Estandar_15(S05B, "S05 - Marea Baja", S05B$Temperatura, S05B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04A<-Estandar_15(S04A, "S04 - Marea Alta", S04A$Temperatura, S04A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S04B<-Estandar_15(S04B, "S04 - Marea Baja", S04B$Temperatura, S04B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03A<-Estandar_40(S03A, "S03 - Marea Alta", S03A$Temperatura, S03A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S03B<-Estandar_40(S03B, "S03 - Marea Baja", S03B$Temperatura, S03B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02A<-Estandar_75(S02A, "S02 - Marea Alta", S02A$Temperatura, S02A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S02B<-Estandar_75(S02B, "S02 - Marea Baja", S02B$Temperatura, S02B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01A<-Estandar_75(S01A, "S01 - Marea Alta", S01A$Temperatura, S01A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_S01B<-Estandar_75(S01B, "S01 - Marea Baja", S01B$Temperatura, S01B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Sanquianga.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_S01A, Temp_S01B, Temp_S02A,Temp_S02B, Temp_S03A, Temp_S03B, Temp_S04A, Temp_S04B, Temp_S05A,Temp_S05B, Temp_S06A, Temp_S06B, top="Boca Sanquianga")
dev.off()

Sal_S06A<-Estandar_15(S06A, "S06 - Marea Alta", S06A$Salinidad, S06A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S06B<-Estandar_15(S06B, "S06 - Marea Baja", S06B$Salinidad, S06B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05A<-Estandar_15(S05A, "S05 - Marea Alta", S05A$Salinidad, S05A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S05B<-Estandar_15(S05B, "S05 - Marea Baja", S05B$Salinidad, S05B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04A<-Estandar_15(S04A, "S04 - Marea Alta", S04A$Salinidad, S04A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S04B<-Estandar_15(S04B, "S04 - Marea Baja", S04B$Salinidad, S04B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03A<-Estandar_40(S03A, "S03 - Marea Alta", S03A$Salinidad, S03A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S03B<-Estandar_40(S03B, "S03 - Marea Baja", S03B$Salinidad, S03B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02A<-Estandar_75(S02A, "S02 - Marea Alta", S02A$Salinidad, S02A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S02B<-Estandar_75(S02B, "S02 - Marea Baja", S02B$Salinidad, S02B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01A<-Estandar_75(S01A, "S01 - Marea Alta", S01A$Salinidad, S01A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_S01B<-Estandar_75(S01B, "S01 - Marea Baja", S01B$Salinidad, S01B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")

tiff(filename = "Salinidad_Boca_Sanquianga.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_S01A, Sal_S01B, Sal_S02A,Sal_S02B, Sal_S03A, Sal_S03B, Sal_S04A, Sal_S04B, Sal_S05A,Sal_S05B, Sal_S06A, Sal_S06B, top="Boca Sanquianga")
dev.off()

Den_S06A<-Estandar_15(S06A, "S06 - Marea Alta", S06A$Densidad, S06A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S06B<-Estandar_15(S06B, "S06 - Marea Baja", S06B$Densidad, S06B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05A<-Estandar_15(S05A, "S05 - Marea Alta", S05A$Densidad, S05A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S05B<-Estandar_15(S05B, "S05 - Marea Baja", S05B$Densidad, S05B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04A<-Estandar_15(S04A, "S04 - Marea Alta", S04A$Densidad, S04A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S04B<-Estandar_15(S04B, "S04 - Marea Baja", S04B$Densidad, S04B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03A<-Estandar_40(S03A, "S03 - Marea Alta", S03A$Densidad, S03A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S03B<-Estandar_40(S03B, "S03 - Marea Baja", S03B$Densidad, S03B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02A<-Estandar_75(S02A, "S02 - Marea Alta", S02A$Densidad, S02A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S02B<-Estandar_75(S02B, "S02 - Marea Baja", S02B$Densidad, S02B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01A<-Estandar_75(S01A, "S01 - Marea Alta", S01A$Densidad, S01A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_S01B<-Estandar_75(S01B, "S01 - Marea Baja", S01B$Densidad, S01B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")


tiff(filename = "Densidad_Boca_Sanquianga.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_S01A, Den_S01B, Den_S02A,Den_S02B, Den_S03A, Den_S03B, Den_S04A, Den_S04B, Den_S05A,Den_S05B, Den_S06A, Den_S06B, top="Boca Sanquianga")
dev.off()


Oxi_S06A<-Estandar_15(S06A, "S06 - Marea Alta", S06A$Oxigeno, S06A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S06B<-Estandar_15(S06B, "S06 - Marea Baja", S06B$Oxigeno, S06B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05A<-Estandar_15(S05A, "S05 - Marea Alta", S05A$Oxigeno, S05A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S05B<-Estandar_15(S05B, "S05 - Marea Baja", S05B$Oxigeno, S05B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04A<-Estandar_15(S04A, "S04 - Marea Alta", S04A$Oxigeno, S04A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S04B<-Estandar_15(S04B, "S04 - Marea Baja", S04B$Oxigeno, S04B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03A<-Estandar_40(S03A, "S03 - Marea Alta", S03A$Oxigeno, S03A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S03B<-Estandar_40(S03B, "S03 - Marea Baja", S03B$Oxigeno, S03B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02A<-Estandar_75(S02A, "S02 - Marea Alta", S02A$Oxigeno, S02A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S02B<-Estandar_75(S02B, "S02 - Marea Baja", S02B$Oxigeno, S02B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01A<-Estandar_75(S01A, "S01 - Marea Alta", S01A$Oxigeno, S01A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_S01B<-Estandar_75(S01B, "S01 - Marea Baja", S01B$Oxigeno, S01B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")

tiff(filename = "Oxigeno_Boca_Sanquianga.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Oxi_S01A, Oxi_S01B, Oxi_S02A,Oxi_S02B, Oxi_S03A, Oxi_S03B, Oxi_S04A, Oxi_S04B, Oxi_S05A,Oxi_S05B, Oxi_S06A, Oxi_S06B, top="Boca Sanquianga")
dev.off()


Temp_G06A<-Estandar_15(G06A, "G06 - Marea Alta", G06A$Temperatura, G06A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G06B<-Estandar_15(G06B, "G06 - Marea Baja", G06B$Temperatura, G06B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05A<-Estandar_15(G05A, "G05 - Marea Alta", G05A$Temperatura, G05A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G05B<-Estandar_15(G05B, "G05 - Marea Baja", G05B$Temperatura, G05B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04A<-Estandar_15(G04A, "G04 - Marea Alta", G04A$Temperatura, G04A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G04B<-Estandar_15(G04B, "G04 - Marea Baja", G04B$Temperatura, G04B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03A<-Estandar_40(G03A, "G03 - Marea Alta", G03A$Temperatura, G03A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G03B<-Estandar_40(G03B, "G03 - Marea Baja", G03B$Temperatura, G03B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02A<-Estandar_75(G02A, "G02 - Marea Alta", G02A$Temperatura, G02A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G02B<-Estandar_75(G02B, "G02 - Marea Baja", G02B$Temperatura, G02B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01A<-Estandar_75(G01A, "G01 - Marea Alta", G01A$Temperatura, G01A$Profundidad, "Temperatura - [°C]", "Profundidad [m]")
Temp_G01B<-Estandar_75(G01B, "G01 - Marea Baja", G01B$Temperatura, G01B$Profundidad, "Temperatura - [°C]", "Profundidad [m]")

tiff(filename = "Temperatura_Boca_Guascama.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Temp_G01A, Temp_G01B, Temp_G02A,Temp_G02B, Temp_G03A, Temp_G03B, Temp_G04A, Temp_G04B, Temp_G05A,Temp_G05B, Temp_G06A, Temp_G06B, top="Boca Guascama")
dev.off()

Sal_G06A<-Estandar_15(G06A, "G06 - Marea Alta", G06A$Salinidad, G06A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G06B<-Estandar_15(G06B, "G06 - Marea Baja", G06B$Salinidad, G06B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05A<-Estandar_15(G05A, "G05 - Marea Alta", G05A$Salinidad, G05A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G05B<-Estandar_15(G05B, "G05 - Marea Baja", G05B$Salinidad, G05B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04A<-Estandar_15(G04A, "G04 - Marea Alta", G04A$Salinidad, G04A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G04B<-Estandar_15(G04B, "G04 - Marea Baja", G04B$Salinidad, G04B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03A<-Estandar_40(G03A, "G03 - Marea Alta", G03A$Salinidad, G03A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G03B<-Estandar_40(G03B, "G03 - Marea Baja", G03B$Salinidad, G03B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02A<-Estandar_75(G02A, "G02 - Marea Alta", G02A$Salinidad, G02A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G02B<-Estandar_75(G02B, "G02 - Marea Baja", G02B$Salinidad, G02B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01A<-Estandar_75(G01A, "G01 - Marea Alta", G01A$Salinidad, G01A$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")
Sal_G01B<-Estandar_75(G01B, "G01 - Marea Baja", G01B$Salinidad, G01B$Profundidad, "Salinidad - [PSU]", "Profundidad [m]")


tiff(filename = "Salinidad_Boca_Guascama.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Sal_G01A, Sal_G01B, Sal_G02A,Sal_G02B, Sal_G03A, Sal_G03B, Sal_G04A, Sal_G04B, Sal_G05A,Sal_G05B, Sal_G06A, Sal_G06B, top="Boca Guascama")
dev.off()


Den_G06A<-Estandar_15(G06A, "G06 - Marea Alta", G06A$Densidad, G06A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G06B<-Estandar_15(G06B, "G06 - Marea Baja", G06B$Densidad, G06B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05A<-Estandar_15(G05A, "G05 - Marea Alta", G05A$Densidad, G05A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G05B<-Estandar_15(G05B, "G05 - Marea Baja", G05B$Densidad, G05B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04A<-Estandar_15(G04A, "G04 - Marea Alta", G04A$Densidad, G04A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G04B<-Estandar_15(G04B, "G04 - Marea Baja", G04B$Densidad, G04B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03A<-Estandar_40(G03A, "G03 - Marea Alta", G03A$Densidad, G03A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G03B<-Estandar_40(G03B, "G03 - Marea Baja", G03B$Densidad, G03B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02A<-Estandar_75(G02A, "G02 - Marea Alta", G02A$Densidad, G02A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G02B<-Estandar_75(G02B, "G02 - Marea Baja", G02B$Densidad, G02B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01A<-Estandar_75(G01A, "G01 - Marea Alta", G01A$Densidad, G01A$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")
Den_G01B<-Estandar_75(G01B, "G01 - Marea Baja", G01B$Densidad, G01B$Profundidad, "Densidad - [Kg/m3]", "Profundidad [m]")

tiff(filename = "Densidad_Boca_Guascama.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Den_G01A, Den_G01B, Den_G02A,Den_G02B, Den_G03A, Den_G03B, Den_G04A, Den_G04B, Den_G05A,Den_G05B, Den_G06A, Den_G06B, top="Boca Guascama")
dev.off()


Oxi_G06A<-Estandar_15(G06A, "G06 - Marea Alta", G06A$Oxigeno, G06A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G06B<-Estandar_15(G06B, "G06 - Marea Baja", G06B$Oxigeno, G06B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G05A<-Estandar_15(G05A, "G05 - Marea Alta", G05A$Oxigeno, G05A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G05B<-Estandar_15(G05B, "G05 - Marea Baja", G05B$Oxigeno, G05B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G04A<-Estandar_15(G04A, "G04 - Marea Alta", G04A$Oxigeno, G04A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G04B<-Estandar_15(G04B, "G04 - Marea Baja", G04B$Oxigeno, G04B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G03A<-Estandar_40(G03A, "G03 - Marea Alta", G03A$Oxigeno, G03A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G03B<-Estandar_40(G03B, "G03 - Marea Baja", G03B$Oxigeno, G03B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G02A<-Estandar_75(G02A, "G02 - Marea Alta", G02A$Oxigeno, G02A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G02B<-Estandar_75(G02B, "G02 - Marea Baja", G02B$Oxigeno, G02B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G01A<-Estandar_75(G01A, "G01 - Marea Alta", G01A$Oxigeno, G01A$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")
Oxi_G01B<-Estandar_75(G01B, "G01 - Marea Baja", G01B$Oxigeno, G01B$Profundidad, "Oxigeno - [mg/L]", "Profundidad [m]")

tiff(filename = "Oxigeno_Boca_Guascama.tif", width = 20, height = 30, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=6, ncol=2,Oxi_G01A, Oxi_G01B, Oxi_G02A,Oxi_G02B, Oxi_G03A, Oxi_G03B, Oxi_G04A, Oxi_G04B, Oxi_G05A,Oxi_G05B, Oxi_G06A, Oxi_G06B, top="Boca Guascama")
dev.off()


#Total 

tiff(filename = "Temperatura_Boca_Guascama_Vibrio.tif", width = 20, height = 25, units = "cm", pointsize = 15, bg = "white", res = 300)
grid.arrange(nrow=3, ncol=2,Temp_G01A, Temp_G02A,Temp_G03A,  Temp_G04A,  Temp_G05A, Temp_G06A,  top="Boca Guascama")
dev.off()


hexbin_plot<-function(Estacion, var1, var2, labelx, labely)
{
  if (!is.null(Estacion) &  !is.null(var1)& !is.null(var2)& !is.null(labelx)& !is.null(labely)){
    ggplot(Estacion, aes(x=var1, y=var2)) +
      geom_hex(bins = 20) +
      labs(x= labelx, y=labely)+
      scale_y_reverse()+
      scale_x_continuous(position = "top")+
      theme_bw()}else{
        print('Faltan Valores')}
}
hexbin_Temp<-hexbin_plot(CTDO, CTDO$Temp, CTDO$Prof, "Temperatura [�C]", "Profundidad [m]")
