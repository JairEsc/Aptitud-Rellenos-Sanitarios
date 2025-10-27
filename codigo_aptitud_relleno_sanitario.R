##Documentación muy sencilla: 

setwd("relleno sanitario respaldo/input/definitivos/")

r_1_pendiente=raster("r_1_pendiente.tiff")
#
#
#
r7_rios=raster("r7_rios.tiff")
#ranking:
#pendiente 2.480620
#Accesibilidad 5.465116 
#Uso de suelo 10
#geologia 5.426357
#fallas y fracturas 5.426357
#nivel piezometrico 1.240310  
#rios 8.720930
pesos=c(6.4,14.1,25.8,14,14,3.2,22.5)#Definidos a partir de un ranking de importancia.
pesos=pesos/sum(pesos)
pesos*100*10
raster_aptitud=pesos[1]*r_1_pendiente+
  pesos[2]*r2_access_loc_mas_2500_hab+
  pesos[3]*r3_uso_suelo+
  pesos[4]*r4_geologia+
  pesos[5]*r5_fracturas+
  pesos[6]*r6_vulner_acuif+
  pesos[7]*r7_rios
raster_aptitud=raster_aptitud+abs(min(raster_aptitud))
raster_aptitud=raster_aptitud*prohibido_final*pendiente_mas_de_40
raster_aptitud[raster_aptitud|>is.na()]=0
raster_aptitud=raster_aptitud*(base+1)
ver_raster(raster_aptitud,"aptitud")

writeRaster(raster_aptitud,"Documentacion/outputs/Aptitud_relleno_sanitario.tiff",overwrite=T)
