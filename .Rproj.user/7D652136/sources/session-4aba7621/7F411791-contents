
palMediana <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(Barrios_precio_join$mediana))

leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
  addCircles(data = poligono_Precios, color = ~pal(mediana),  radius = ~Precio/10)%>%
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLegend( position = "bottomleft", pal = palMediana, values = na.omit(poligono_Precios$mediana),
             title = 'Precios',
             opacity = 1)%>%
  addMiniMap(tiles = "CartoDB.Voyager")




## Eliminar los datos que no coinciden entre los barrios
data_filtrada <- points_joined_sf %>%
  dplyr::filter(stringr ::str_sub(Barrio, 1, 3) == stringr ::str_sub(barrio, 1, 3))

data_filtrada2 <- sf::st_as_sf(data_filtrada, coords = geometry )
write.csv2(data_filtrada, "data_filtrada.csv")
sf::st_write(data_filtrada2, "./sig/data_filtrada2.shp", append=TRUE)
plot(data_filtrada2$geometry)

#mapa de los datos que coinciden con los barrios
leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Cali_WGS84, group = "Barrios", color = "grey",
              stroke = 0.2, opacity = 1)%>%
  addCircles(data = data_filtrada2, group = "Viviendas", color = ~pal(Precio),  radius = ~Precio)
# Ver los resultados
print(data_filtrada)

library(leaflet)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(datos_Vivienda$Precio))

leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Cali_WGS84, group = "Barrios", color = "grey",
              stroke = 0.2, opacity = 1)%>%
  addCircles(data = joined_sf, group = "Viviendas", color = ~pal(Precio),  radius = ~Precio/100)%>%
  addLayersControl(overlayGroups = c("Barrios", "Viviendas"),    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend('bottomright', pal = pal, values = na.omit(datos_Vivienda$Precio),
            title = 'Precios',
            opacity = 1)

polygons_layer1<-Cali_WGS84

comuna_grouped_layer <- joined_sf %>% group_by(comuna)
comuna_mean_values <- comuna_grouped_layer %>% summarise(mean_value = mean(Precio, na.rm = TRUE))
comuna_median_values <- comuna_grouped_layer %>% summarise(median_value = median(Precio, na.rm = TRUE))

Zona_grouped_layer <- joined_sf %>% group_by(Zona)
Zona_mean_values <- Zona_grouped_layer %>% summarise(mean_value = mean(Precio, na.rm = TRUE))
Zona_median_values <- Zona_grouped_layer %>% summarise(median_value = median(Precio, na.rm = TRUE))


joined_sf_comuna <- sf::st_join(polygons_layer1, Zona_mean_values)

qpal <- colorQuantile("RdYlBu", joined_sf_comuna$mean_value, n = 5)
palvalue <- colorNumeric(
  palette = "YlGnBu",
  domain = joined_sf_comuna$mean_value
)

labels <- sprintf(
  "<strong>%s</strong><br/>%g 000 ",
  Cali_WGS84$barrio , Cali_WGS84$comuna 
) %>% lapply(htmltools::HTML)

leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Cali_WGS84, 
              fillColor = ~pal(shape_area),  weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Cali_WGS84, group = "Barrios", color = "grey",
              stroke = 0.2, opacity = 1)%>%
  addCircles(data = joined_sf, group = "Viviendas", color = ~pal(Precio),  radius = ~Precio/100)%>%
  addLayersControl(overlayGroups = c("Barrios", "Viviendas"),    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend('bottomright', pal = pal, values = na.omit(datos_Vivienda$Precio),
            title = 'Precios',
            opacity = 1)



map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~gdp_md_est,
            title = "Est. GDP (2010)",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )


library(sf)

library(stars)



# Define the raster template

template <- st_as_stars(st_bbox(Cali_WGS84), nx = 100, ny = 100, values = NA_real_)


# Rasterize the point layer

raster <- st_rasterize(grouped_layer$Precio, template, field = "Precio ")

plot(raster)
# Plot the raster

plot(raster)


library(paqueteMETODOS)
library(shiny)

data("vivienda_faltantes")


shiny::sliderInput("bins", "Número de Contenedores:", 30, min = 10, max = 30)

renderPlot({
  Precios   = vivienda_faltantes$preciom 
  bins = seq(min(Precios), max(Precios), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(Precios, breaks = bins, col = '#367BB5', border = 'white', main="Histograma de precios", ylab="Frecuencia", xlab="Precios (millones)")
})


install.packages('units') 
devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")

library(leaflet)
library(ggmap)
library(mapview)
library(raster)
library(magrittr)
UK <- ggmap::geocode("United Kingdom")

#FILE1 <- read.csv("DATASET1.csv")
#FILE2 <- read.csv("DATASET2.csv")
FILE1 <- data.frame('lat' = c(51.31, 51.52, 51.53), 'lon' = c(0.06, 0.11, 0.09))
FILE2 <- data.frame('lat' = c(52.20, 52.25, 52.21), 'lon' = c(0.12, 0.12, 0.12))

map1 <- leaflet(FILE1)%>%
  addTiles()%>%
  addMarkers(clusterOptions = markerClusterOptions())

map2 <- leaflet(FILE2)%>%
  addTiles()%>%
  addMarkers(clusterOptions = markerClusterOptions())

mapview::latticeView(map1, map2, ncol = 2, sync = list(c(1, 2)), sync.cursor = FALSE, no.initial.sync = FALSE)
# Or:
sync(map1, map2)