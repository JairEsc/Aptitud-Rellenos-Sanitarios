#Nuevo mapa
"../En"
#Librerías
library(dplyr)
library(foreign)
library(raster)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leaflegend)
library(htmlwidgets)
#""
#Datos
#Locs_Urbanas=read.dbf("POB_Urban.dbf", as.is = TRUE)
Locs_Urbanas=st_read("POB_Urban.dbf", options = "ENCODING=latin1", quiet = TRUE)
Locs_Urb_H = Locs_Urbanas[substr(Locs_Urbanas$CVEGEO,1,2)=="13",]
Encoding(Locs_Urb_H$NOM_MUN) <- "UTF-8"


#Locs_Rural=read.dbf("POB_Rural.dbf", as.is = TRUE)
Locs_Rural=st_read("POB_Rural.dbf", options = "ENCODING=latin1", quiet = TRUE)
Locs_Rur_H = Locs_Rural[substr(Locs_Rural$CVEGEO,1,2)=="13",]

Locs1=read_sf("13l.shp")

Locs2=read_sf("13lpr.shp")

Municipios=read_sf("LIM_MUNICIPALES_FINAL.shp")

Rastercito=raster("outputs/Aptitud_relleno_sanitario.tiff")

#Juntamos
#Urbanas están good todas tienen geometría
Urbans_GEO=merge(x = Locs_Urb_H |> dplyr::select(NOM_MUN,NOMGEO,CVEGEO,POB1) ,y = Locs1 |> dplyr::select(CVEGEO,AMBITO,geometry) ,by="CVEGEO",all.x=T,all.y=F)

#Ahora las rurales
#Primero les intentamos pegar un mulipolygon
Locs1_R=Locs1[Locs1$AMBITO=="Rural",]
# > (unique(Locs_Rur_H$CVEGEO) |> length()) == nrow(Locs_Rur_H)
# [1] TRUE
#Very good, no se repiten

Locs_Rur_H$llave=paste0(Locs_Rur_H$CVEGEO,Locs_Rur_H$CVE_AGEB)
Locs2$llave=substr(Locs2$CVEGEO,1,9)

Pre_Rurals_GEO=merge(x = Locs_Rur_H |> dplyr::select(NOM_MUN,NOMGEO,POB1,CVEGEO),y = Locs1_R |> dplyr::select(CVEGEO,geometry),by="CVEGEO",all.x=T,all.y=F)

Pre_Rurals_GEO_1=Pre_Rurals_GEO[which(!st_is_empty(Pre_Rurals_GEO$geometry)),]
Pre_Rurals_GEO_2=Pre_Rurals_GEO[which(st_is_empty(Pre_Rurals_GEO$geometry)),]
Pre_Rurals_GEO_2=st_drop_geometry(Pre_Rurals_GEO_2)
Pre_Rurals_GEO_2=merge(x = Pre_Rurals_GEO_2 |> dplyr::select(-geometry),y = Locs2 |> dplyr::select(llave,geometry),by.x="CVEGEO",by.y="llave",all.x=T,all.y=F)

Rurals_GEO=rbind(Pre_Rurals_GEO_1,Pre_Rurals_GEO_2)

Urbans_GEO=st_as_sf(Urbans_GEO)
Rurals_GEO=st_as_sf(Rurals_GEO)
Pre_Rurals_GEO_1=st_as_sf(Pre_Rurals_GEO_1)
Pre_Rurals_GEO_2=st_as_sf(Pre_Rurals_GEO_2)
#A CRS4326
Municipios=st_transform(Municipios,crs = 4326)
Urbans_GEO=st_transform(Urbans_GEO,crs = 4326)
Rurals_GEO=st_transform(Rurals_GEO,crs = 4326)
Pre_Rurals_GEO_1=st_transform(Pre_Rurals_GEO_1,crs = 4326)
Pre_Rurals_GEO_2=st_transform(Pre_Rurals_GEO_2,crs = 4326)
#Rastercito=st_transform(Rastercito)
crs(Rastercito)=4326
extent(Rastercito)=extent(Municipios)




legend_html <- "
<div style='background: white; padding: 10px; border-radius: 4px;'>
  <i style='background: rgb(200, 200, 200); width: 20px; height: 15px; display: inline-block; margin-right: 5px;'></i>Sitios de Disposición Final<br>
  <i style='background: rgb(125, 1, 51); width: 25px; height: 3px; display: inline-block; margin-right: 5px;vertical-align: middle;'></i>Limites Municipales<br>
  <i style='background: rgb(176, 176, 176); width: 20px; height: 15px; display: inline-block; margin-right: 5px;'></i>Población Rural<br>
  <i style='background: rgb(54, 54, 54); width: 20px; height: 15px; display: inline-block; margin-right: 5px;'></i>Población Urbana<br>
</div>
"
#Mapa Leaflet
