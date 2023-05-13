Analisis_autocorrelacion<-function(nombre_variable){
if(!require(sp))install.packages("sp ")
if(!require(spdep))install.packages("spdep ")
assign("knn", spdep::knn2nb(knearneigh(coordinates(marea_altacoor), k = 5)),envir = parent.frame())
assign("w", spdep::nb2listw(knn, style = "B"), envir = parent.frame())

# Test for global spatial autocorrelation
marea_altacoor@data[[paste0(nombre_variable)]]
variable_alta<-marea_altacoor@data[[paste0(nombre_variable)]]
variable_baja<-marea_bajacoor@data[[paste0(nombre_variable)]]
assign(paste0(nombre_variable,"_moranTest_Alta"),spdep::moran.test(variable_alta, listw = w), envir = parent.frame())
assign(paste0(nombre_variable,"_moranTest_Baja"),spdep::moran.test(variable_baja, listw = w), envir = parent.frame())



# Test for local spatial autocorrelation
assign(paste0(nombre_variable, "_localMoran_Alta"), spdep::localmoran(variable_alta, listw = w), envir = parent.frame())
assign(paste0(nombre_variable, "_localMoran_Baja"), spdep::localmoran(variable_baja, listw = w), envir = parent.frame())

assign(paste0(nombre_variable, "_localMoran_Alta_DF"), as.data.frame(spdep::localmoran(variable_alta, listw = w)), envir = parent.frame())
assign(paste0(nombre_variable, "_localMoran_Baja_DF"),  as.data.frame(spdep::localmoran(variable_baja, listw = w)), envir = parent.frame())
     
assign(paste0(nombre_variable, "_Alta_LM_list"), as.list(as.data.frame(spdep::localmoran(variable_alta, listw = w))), envir = parent.frame())
assign(paste0(nombre_variable, "_Baja_LM_list"), as.list(as.data.frame(spdep::localmoran(variable_baja, listw = w))), envir = parent.frame())

capture.output(paste0("###", nombre_variable),
               "##########################Moran Global",
               "###Alta",
               print(spdep::moran.test(variable_alta, listw = w)),
               "###Baja",
               print(spdep::moran.test(variable_baja, listw = w)),
               "#############################Moran Local",
               "###Alta",
               print(spdep::localmoran(variable_alta, listw = w)),
               "###Baja",
               print(spdep::localmoran(variable_baja, listw = w)),
               file = paste0(nombre_variable, "_MoranTest.txt")
               
)


}