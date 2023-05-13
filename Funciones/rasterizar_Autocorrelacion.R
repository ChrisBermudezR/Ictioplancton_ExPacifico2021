 rasterizar_Autocorrelacion<-function(nombre_variable,longitud, latitud, moran, probabilidad, marea, leyenda_moran,leyenda_probabilidad,  titulo){
 if(!require(tidyverse))install.packages("tidyverse")
 if(!require(oce))install.packages("oce")
  assign("WIRE_moran",interpBarnes(longitud, latitud, moran), envir = parent.frame())
  assign(paste0("pts_moran.grid"),expand.grid(Longitud=WIRE_moran$x, Latitud=WIRE_moran$y), envir = parent.frame())
  assign(paste0("pts_moran.grid"), mutate(pts_moran.grid, variable=as.vector(WIRE_moran$zg)), envir = parent.frame())
  assign(paste0("export_moran"),  raster::rasterFromXYZ(pts_moran.grid), envir = parent.frame())
  assign(paste0(nombre_variable,"_",marea,"_pts_moran.grid"),  rasterFromXYZ(pts_moran.grid), envir = parent.frame())
  raster::writeRaster(export_moran, filename=paste("../SIG_Datos/grids/",nombre_variable,"_",marea, ".tif", sep = ""),overwrite=TRUE)
  
 assign(paste0(nombre_variable,"_",marea,"_moran_plot"), 
  ggplot(pts_moran.grid, aes(Longitud, Latitud)) +
    geom_raster(aes(fill = variable))+
    geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#3aaa05", fill="#3aaa05") +
    geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
    coord_sf(xlim = c(-78.4055, -78.217), ylim = c(2.55, 2.853), expand = FALSE)+   
    geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent") +
    theme_bw()+
    scale_fill_gradientn(colours = c("#01665e", "#5ab4ac", "#c7eae5", "#f5f5f5", "#f6e8c3", "#d8b365", "#8c510a"))+
    geom_point(data=marea_baja, aes(x= longitud, y= latitud))+
    labs(fill=leyenda_moran, title= titulo)+
    theme(legend.position = "bottom"),
    
envir = parent.frame())


  assign("WIRE_probabilidad",interpBarnes(longitud, latitud, probabilidad), envir = parent.frame())
  assign(paste0("pts_probabilidad.grid"),expand.grid(Longitud=WIRE_probabilidad$x, Latitud=WIRE_probabilidad$y), envir = parent.frame())
  assign(paste0("pts_probabilidad.grid"), mutate(pts_probabilidad.grid, variable=as.vector(WIRE_probabilidad$zg)), envir = parent.frame())
  assign(paste0("export_probabilidad"),  raster::rasterFromXYZ(pts_probabilidad.grid), envir = parent.frame())
  assign(paste0(nombre_variable,"_",marea,"_pts_probabilidad.grid"),  rasterFromXYZ(pts_probabilidad.grid), envir = parent.frame())
  raster::writeRaster(export_probabilidad, filename=paste("../SIG_Datos/grids/",nombre_variable,"_",marea, ".tif", sep = ""),overwrite=TRUE)
  

assign(paste0(nombre_variable,"_",marea,"_moranProba_plot"), 
  ggplot(pts_probabilidad.grid, aes(Longitud, Latitud)) +
    geom_raster(aes(fill = variable))+
    geom_polygon(data=costa, aes(x= long, y= lat, group=group), colour="#3aaa05", fill="#3aaa05") +
    geom_polygon(data=rios, aes(x= long, y= lat, group=group), colour="#bcebfb", fill="#bcebfb") +
    coord_sf(xlim = c(-78.4055, -78.217), ylim = c(2.55, 2.853), expand = FALSE)+   
    geom_polygon(data=areas_protegidas, aes(x= long, y= lat, group=group), colour="red", fill="transparent") +
    theme_bw()+
    scale_fill_gradientn(colours = c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb", "#4575b4"))+
    geom_point(data=marea_baja, aes(x= longitud, y= latitud))+
    labs(fill=leyenda_probabilidad, title= titulo)+
    theme(legend.position = "bottom"),
     envir = parent.frame())
}