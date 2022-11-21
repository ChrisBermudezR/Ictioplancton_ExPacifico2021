#Graficas de los perfiles para marea baja y alta


Tem_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A02"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A01"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
    labs(title= "Transecto Amarales", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()



Temp_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G03"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G02"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G01"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Guascama", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()

Temp_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S03"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S02"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S01"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Sanquianga", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()


tiff(filename = "Pli_Temp.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Tem_Ama, Temp_Guas, Temp_Sanq)
dev.off()


Sal_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A02"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A01"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Amarales", x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(25,36))+
  theme_bw()

Sal_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G03"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G02"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G01"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Guascama", x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(18,36))+
  theme_bw()

Sal_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S03"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S02"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S01"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Sanquianga", x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(5,36))+
  theme_bw()

tiff(filename = "Poli_Salinidad.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Sal_Ama, Sal_Guas, Sal_Sanq)
dev.off()





Oxi_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A02"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A01"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Amarales", x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

Oxi_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G03"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G02"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G01"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Guascama", x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(0,15))+
  theme_bw()

Oxi_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S03"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S02"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S01"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Sanquianga", x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(60,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

tiff(filename = "Poli_Oxigeno.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Oxi_Ama, Oxi_Guas, Oxi_Sanq)
dev.off()








Tem_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Amarales - Marea Alta", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
    theme_bw()



Temp_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Guascama - Marea Alta", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()

Temp_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Sanquianga - Marea Alta", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(10,30))+
  theme_bw()


tiff(filename = "Pli_Temp.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Tem_Ama, Temp_Guas, Temp_Sanq)
dev.off()


Sal_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(25,36))+
  theme_bw()

Sal_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs( x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(18,36))+
  theme_bw()

Sal_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(5,36))+
  theme_bw()

tiff(filename = "Poli_Salinidad.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Sal_Ama, Sal_Guas, Sal_Sanq)
dev.off()





Oxi_Ama=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="A03"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs( x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

Oxi_Guas=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="G04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(0,15))+
  theme_bw()

Oxi_Sanq=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Alta" & Estacion=="S04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

tiff(filename = "Poli_Oxigeno.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Oxi_Ama, Oxi_Guas, Oxi_Sanq)
dev.off()

library(gtable)
Ama_legend = gtable_filter(ggplot_gtable(ggplot_build(Tem_Ama+theme(legend.position="bottom"))), "guide-box")
Guas_legend = gtable_filter(ggplot_gtable(ggplot_build(Temp_Guas+theme(legend.position="bottom"))), "guide-box")
Sanq_legend = gtable_filter(ggplot_gtable(ggplot_build(Temp_Sanq+theme(legend.position="bottom"))), "guide-box")


tiff(filename = "Poli_Total.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=3, ncol=3,Tem_Ama, Temp_Guas, Temp_Sanq, Sal_Ama, Sal_Guas, Sal_Sanq,Oxi_Ama, Oxi_Guas, Oxi_Sanq)
dev.off()



p1=grid.arrange(arrangeGrob(Tem_Ama+theme(legend.position="none"), Sal_Ama+theme(legend.position="none"), Oxi_Ama+theme(legend.position="none"), nrow = 3) )
p2=grid.arrange(arrangeGrob(Temp_Guas+theme(legend.position="none"), Sal_Guas+theme(legend.position="none"), Oxi_Guas+theme(legend.position="none"), nrow = 3))
p3=grid.arrange(arrangeGrob(Temp_Sanq+theme(legend.position="none"), Sal_Sanq+theme(legend.position="none"), Oxi_Sanq+theme(legend.position="none"), nrow = 3))


tiff(filename = "Poli_Total_Alta.tif", width = 24, height = 25, units = "cm", pointsize = 20, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,p1,p2,p3)
dev.off()

Sal_Ama+theme(legend.position="bottom")




Tem_Ama_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A03"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Amarales - Marea Baja", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()



Temp_Guas_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Guascama - Marea Baja", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(15,30))+
  theme_bw()

Temp_Sanq_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S06"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S05"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S04"), size=0.3, aes(x=Temperatura, y=Profundidad, linetype=Estacion))+
  labs(title= "Transecto Sanquianga - Marea Baja", x= " Temperatura [°C]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(10,30))+
  theme_bw()


tiff(filename = "Pli_Temp.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Tem_Ama, Temp_Guas, Temp_Sanq)
dev.off()


Sal_Ama_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A03"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(25,36))+
  theme_bw()

Sal_Guas_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs( x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(18,36))+
  theme_bw()

Sal_Sanq_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S06"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S05"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S04"), size=0.3, aes(x=Salinidad, y=Profundidad, linetype=Estacion))+
  labs(x= "Salinidad - [PSU]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(5,36))+
  theme_bw()

tiff(filename = "Poli_Salinidad.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Sal_Ama, Sal_Guas, Sal_Sanq)
dev.off()





Oxi_Ama_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="A03"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs( x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(40,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

Oxi_Guas_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="G04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(7,0))+
  scale_x_continuous(position = "top", lim=c(0,15))+
  theme_bw()

Oxi_Sanq_Baja=ggplot() +
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S06"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S05"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  geom_path(data =subset(Datos_CTDO_CCCP, Marea=="Baja" & Estacion=="S04"), size=0.3, aes(x=Oxigeno, y=Profundidad, linetype=Estacion))+
  labs(x= "Oxígeno - [mg/L]", y="Profundidad [m]")+
  scale_y_reverse(lim=c(20,0))+
  scale_x_continuous(position = "top", lim=c(0,10))+
  theme_bw()

tiff(filename = "Poli_Oxigeno.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=2,Oxi_Ama, Oxi_Guas, Oxi_Sanq)
dev.off()

library(gtable)
Ama_legend = gtable_filter(ggplot_gtable(ggplot_build(Tem_Ama_Baja+theme(legend.position="bottom"))), "guide-box")
Guas_legend = gtable_filter(ggplot_gtable(ggplot_build(Temp_Guas_Baja+theme(legend.position="bottom"))), "guide-box")
Sanq_legend = gtable_filter(ggplot_gtable(ggplot_build(Temp_Sanq_Baja+theme(legend.position="bottom"))), "guide-box")


tiff(filename = "Poli_Total.tif", width = 20, height = 15, units = "cm", pointsize = 12, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=3, ncol=3,Tem_Ama, Temp_Guas, Temp_Sanq, Sal_Ama, Sal_Guas, Sal_Sanq,Oxi_Ama, Oxi_Guas, Oxi_Sanq)
dev.off()



p1_Baja=grid.arrange(arrangeGrob(Tem_Ama_Baja+theme(legend.position="none"), Sal_Ama_Baja+theme(legend.position="none"), Oxi_Ama_Baja+theme(legend.position="none"), nrow = 3), nrow=2, heights=c(2, 0.1),Ama_legend )
p2_Baja=grid.arrange(arrangeGrob(Temp_Guas_Baja+theme(legend.position="none"), Sal_Guas_Baja+theme(legend.position="none"), Oxi_Guas_Baja+theme(legend.position="none"), nrow = 3), nrow=2, heights=c(2, 0.1),Guas_legend)
p3_Baja=grid.arrange(arrangeGrob(Temp_Sanq_Baja+theme(legend.position="none"), Sal_Sanq_Baja+theme(legend.position="none"), Oxi_Sanq_Baja+theme(legend.position="none"), nrow = 3), nrow=2, heights=c(2, 0.1),Sanq_legend)


tiff(filename = "Poli_Total_Baja.tif", width = 24, height = 25, units = "cm", pointsize = 20, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=1, ncol=3,p1_Baja,p2_Baja,p3_Baja)
dev.off()

tiff(filename = "Poli_Total.tif", width = 27, height = 30, units = "cm", pointsize = 25, bg = "white", res = 300, compression = "lzw")
grid.arrange(nrow=2, ncol=3,p1, p2, p3, p1_Baja,p2_Baja,p3_Baja)
dev.off()            



