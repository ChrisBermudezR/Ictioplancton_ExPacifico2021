 

 rasterizar_Variable<-function(nombre_variable,longitud, latitud, variable, marea, leyenda, titulo){
 if(!require(tidyverse))install.packages("tidyverse")
 if(!require(oce))install.packages("oce")
  assign("WIRE",interpBarnes(longitud, latitud, variable), envir = parent.frame())
  assign(paste0("pts.grid"),expand.grid(Longitud=WIRE$x, Latitud=WIRE$y), envir = parent.frame())
  assign(paste0("pts.grid"), mutate(pts.grid, variable=as.vector(WIRE$zg)), envir = parent.frame())
  assign(paste0("export"),  raster::rasterFromXYZ(pts.grid), envir = parent.frame())
  assign(paste0(nombre_variable,"_",marea,"_pts.grid"),  rasterFromXYZ(pts.grid), envir = parent.frame())
  raster::writeRaster(export, filename=paste("../SIG_Datos/grids/",nombre_variable,"_",marea, ".tif", sep = ""),overwrite=TRUE)
  
 assign(paste0(nombre_variable,"_plot"), 
  ggplot(pts.grid, aes(Longitud, Latitud)) +
    geom_raster(aes(fill = variable))+
    geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#3aaa05", fill="#3aaa05") +
    geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
    coord_sf(xlim = c(-78.4055, -78.217), ylim = c(2.55, 2.853), expand = FALSE)+   
    geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent") +
    theme_bw()+
    scale_fill_gradientn(colours = c("#4d4d4d", "#999999", "#e0e0e0", "#ffffff", "#fddbc7", "#ef8a62", "#b2182b"))+
    geom_point(data=marea_baja, aes(x= longitud, y= latitud))+
    labs(fill=leyenda, title= titulo), envir = parent.frame())
  
}