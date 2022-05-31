pacman::p_load(sf, tidyverse, sfnetworks, openrouteservice, terra, leaflet,raster, mapview, rgee)


Comuna <- read_sf('C:/Users/Mistick/Documents/Datos Espaciales/Proyecto_2/R09/COMUNA_C17.shp')


for (i in 1:nrow(Comuna)) {
  
  if(Comuna$NOM_COMUNA[i] == "TEMUCO"){
    Comuna_g <- st_geometry(Comuna)
  }
} 

plot(Comuna_g, lwd = 3)
plot(st_simplify(Comuna_g, dTolerance = 1e3), add=TRUE, border="red") # la simplificacion con umbral de 1000m
plot(st_simplify(Comuna_g[28], dTolerance = 1e3), add=TRUE, border="green") # 5000m

ee_Initialize(drive = T)

roi <- 
  c(-72.6667, -38.7333) %>%  # Temuco
  st_point(dim = "XYZ") %>% 
  st_buffer(dist = 0.1) %>% 
  sf_as_ee()

# identifico posibles fechas de imagenes del LS8
disponible <- ee$ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
  filterDate('2020-07-01','2022-05-10')$
  filterBounds(roi)$
  filterMetadata('CLOUD_COVER','less_than', 5)

# ordeno las fechas
df_disponible <- ee_get_date_ic(disponible)%>%
  arrange(time_start)
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_233087_20210112")
vizParams <- list(
  min = 0,
  max = 0.5,
  bands = c("B6", "B5", "B4"),
  gamma = c(0.95, 1.1, 1)
)
Map$setCenter(-72.6667, -38.7333, 6.5)
Map$addLayer(image, vizParams)



# Simple Single-Band Vizualization
ndwi <- image$normalizedDifference(c("B3", "B5"))
ndwiViz <- list(
  min = 0.3,
  max = 1,
  palette = c("00FFFF", "0000FF")
)
## Masking
ndwiMasked <- ndwi$updateMask(ndwi$gte(0.4))

## Produce an RGB layers.
imageRGB <- image$visualize(
  list(
    bands = c("B5", "B4", "B3"),
    max = 0.7 
  )
)

ndwiRGB <- ndwiMasked$visualize(
  list(
    min = 0.5,
    max = 1,
    palette = c("00FFFF", "0000FF")
  )
)

# Mosaic the visualization layers and display( or export).
roi <- ee$Geometry$Point(c(-72.6667, -38.7333))$buffer(70000)
Map$centerObject(image$clip(roi))
Map$addLayer(
  eeObject = image$clip(roi),
  visParams = vizParams,
  name = "Landsat 8"
) +
  Map$addLayer(
    eeObject = ndwiMasked$clip(roi),
    visParams = ndwiViz,
    name = "NDWI"
  )