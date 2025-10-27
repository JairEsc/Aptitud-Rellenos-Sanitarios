library(terra)
library(raster)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmlwidgets)
library(htmltools)
aptitud_raster="outputs/Aptitud_relleno_sanitario.tiff" |> terra::rast()
###Aquí va el código que ya existe para el mapa web
source("Mapita_casi_F.R")


##Info neva
"outputs/Aptitud_relleno_sanitario_suavizado.geojson" |> st_read()->suavizado

paleta_registro = colorNumeric(palette = "YlOrRd", domain = c(min(suavizado$Value_Min), max(suavizado$Value_Max)))
readme_content_html <- readLines("README.html", warn = FALSE) |>
  paste(collapse = "\n")

# 3. Define el contenido HTML del modal (Incluyendo el botón de cierre)
modal_content <- tags$div(
  # El contenido del README renderizado se inserta aquí
  HTML(readme_content_html),
  
  # El botón de cerrar
  tags$button(
    "Cerrar", 
    onclick = "document.getElementById('infoModal').style.display='none'",
    style = "margin-top: 15px; padding: 10px 20px; cursor: pointer;" # Estilo para que el botón se vea bien
  )
)
Map=leaflet() |> addTiles()

Map=Map |> 
  addPolygons(data=suavizado ,fillColor =NA,fillOpacity = 0,opacity = 0,color = NA ,label =paste0(
    "Rango de Aptitud: ",suavizado$Value_Min |> round(3)," - ",suavizado$Value_Max |> round(3) )
  ) |> 
  addEasyButton(
    easyButton(
      icon = "fa-info-circle",
      title = "Información",
      onClick = JS("function(btn, map){ 
      var modal = document.getElementById('infoModal');
      if (modal) {modal.style.display = 'block';
      };
    }")
    )
  ) |> 
  prependContent(
    tags$div(
      id = "infoModal",
      class = "modal",
      style = "display:none; position:fixed; top:20%; left:20%; width:60%; background:white; padding:20px; border:2px solid black; z-index:1000; overflow-y: auto;", # Added overflow-y: auto for scroll if content is long
      modal_content)
  ) |>  addLogo(img ="https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/fondo_transparent.png",position ="bottomleft" ,width = "400px",height = "122px"  ) |> 
  addRasterImage(Rastercito,colors = colorNumeric(palette = "RdYlGn", domain = values(Rastercito), na.color = "transparent"), 
                          opacity = 0.8) |>
  addControl(html = legend_html, position = "bottomright") |> 
  addLegendNumeric(pal = colorNumeric('RdYlGn', seq(0,2.95,0.01)) , values = seq(0,2.95,0.01), position = 'bottomright', 
                   title = 'Pertinencia', orientation = 'horizontal', shape = 'rect', decreasing = FALSE, height = 20, width = 200,labels = c("Baja (0)","Alta (3)") ,tickLength = 0)


Map=Map |> addPolygons(data=Municipios,color = "#7D0133",weight = 2,opacity = 1,fillOpacity = 0,label = ~paste("Municipio:",NOM_MUN),group = "Municipios")


Map=Map |> addPolygons(data=Urbans_GEO |> dplyr::select(NOMGEO,NOM_MUN,POB1,geometry),color = "#363636",fillColor = "#363636",weight = 2,opacity = 1,fillOpacity = 0.7,label = paste0(Urbans_GEO$NOM_MUN," - ",Urbans_GEO$NOMGEO),
                       popup = ~paste("Municipio: ",NOM_MUN,"<br>Localidad: ", NOMGEO,"<br>Población: ",POB1),group="Localidades Urbanas")


Map=Map |> addPolygons(data=Pre_Rurals_GEO_1|> dplyr::select(NOMGEO,NOM_MUN,POB1,geometry),color = "#B0B0B0",fillColor = "#B0B0B0",weight = 2,opacity = 1,fillOpacity = 0.7,label = paste0(Pre_Rurals_GEO_1$NOM_MUN," - ",Pre_Rurals_GEO_1$NOMGEO),
                       popup = ~paste("Municipio: ",NOM_MUN,"<br>Localidad: ", NOMGEO,"<br>Población: ",POB1),group="Localidades Rurales")

Map=Map |> addCircleMarkers(data=Pre_Rurals_GEO_2|> dplyr::select(NOMGEO,NOM_MUN,POB1,geometry),color = "#B0B0B0",radius = 0.5,opacity = 1,fillOpacity = 0.7,label = paste0(Pre_Rurals_GEO_2$NOM_MUN," - ",Pre_Rurals_GEO_2$NOMGEO),
                            popup = ~paste("Municipio: ",NOM_MUN,"<br>Localidad: ", NOMGEO,"<br>Población: ",POB1),group="Localidades Rurales")

Map=Map |> 
  addSearchFeatures(targetGroups = c("Localidades Rurales","Localidades Urbanas"),
                    options = searchFeaturesOptions(
                      zoom = 12, 
                      openPopup = T,
                      firstTipSubmit =F,
                      hideMarkerOnCollapse =F))



#Agrega una capa de polygons invisible (quita la paleta y hazlos transparentes), pero que tenga este label.


#Map3


##Haz la conexion al buig (source("../../Reutilizables/Postgres_BUIG/conexion_buig.R")) para consultar la tabla: 


#"sitios_de_disposicion_final_2021"
#Agregalos como circlemarkers de colorcito café

#sitios_disp_fin=st_read(buig,"sitios_de_disposicion_final_2021")

Map=Map |> 
  #leaflet() |> addTiles() |> 
  addCircleMarkers(data=sitios_disp_fin,fillColor = "#172985", fillOpacity = 0.4,color ="#172985" ,popup = "Sitio de Disposición Final",group ="Sitio de Disposición Final" ) |> 
  onRender(
    "function(el, x) {
      var legend = document.getElementsByClassName('info legend leaflet-control')[1];
      console.log(legend.children[0])
      if (legend && legend.children[0]) { 
        console.log(legend.children)
        var firstSwatch = legend.children[0].children[0];
        firstSwatch.style.borderRadius = '50%'; // Makes it a perfect circle
        firstSwatch.style.width = '12px';      // Adjust size as desired
        firstSwatch.style.height = '12px';     // Keep width and height equal for a circle
        firstSwatch.style.backgroundColor = '#172985';

        firstSwatch.style.border = '2px solid #172985'; 
      }
      document.addEventListener('click', function (e) {
  if(e.target.className != 'modal'){
     document.getElementById('infoModal').style.display='none'
  }
}, false);
    }"
  ) |> leafem::addMouseCoordinates() |> 
  addLayersControl(overlayGroups = c("Localidades Rurales","Localidades Urbanas","Sitio de Disposición Final","Municipios"),options = layersControlOptions(collapsed = F)) |> 
  hideGroup("Municipios")

htmlwidgets::saveWidget(Map,file = "Aptitud_Rellenos_Sanitarios.html", title = "Aptitud Rellenos Sanitarios", selfcontained = TRUE,
)
# Agrega el logo de la dirección. link: https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/fondo_transparent.png


#Guardar como html


