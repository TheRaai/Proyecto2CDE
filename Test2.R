pacman::p_load(sf, tidyverse, sfnetworks, openrouteservice, terra, leaflet,raster, mapview, rgee)

# cargamos un shape de poligonos
Calles <- read_sf('C:/Users/Mistick/Documents/Datos Espaciales/Proyecto_2/R09/CALLES_PAIS_C17.shp')

Comuna <- read_sf('C:/Users/Mistick/Documents/Datos Espaciales/Proyecto_2/R09/COMUNA_C17.shp')

Distrito <- read_sf('C:/Users/Mistick/Documents/Datos Espaciales/Proyecto_2/R09/DISTRITO_C17.shp')

#Entidad <- st_read(system.file("R09/ENTIDAD_C17.shp", package="sf"))

#Lim_DPA <- st_read(system.file("R09/LIM_DPA_CENSAL_C17.shp", package="sf"))

#Lim_Urbano <- st_read(system.file("R09/LIMITE_URBANO_CENSAL_C17.shp", package="sf"))

#Localidad <- st_read(system.file("R09/LOCALIDAD_C17.shp", package="sf"))

#Manzana_Aldea <- st_read(system.file("R09/MANZANA_ALDEA_C17.shp", package="sf"))

#Manzana_IND <- st_read(system.file("R09/MANZANA_IND_C17.shp", package="sf"))

#Manzana_SIN_IND <- st_read(system.file("R09/MANZANA_SIN_INF_C17.shp", package="sf"))

#Provincia <- st_read(system.file("R09/PROVINCIA_C17.shp", package="sf"))

#Region <- st_read(system.file("R09/REGION_C17.shp", package="sf"))

#Zona <- st_read(system.file("R09/ZONA_C17.shp", package="sf"))



#Busca temuco y grafica su region (region de la araucania)

for (i in 1:nrow(Comuna)) {
 
   if(Comuna$NOM_COMUNA[i] == "TEMUCO"){
     Comuna_g <- st_geometry(Comuna)
    }
   } 

plot(Comuna_g, lwd = 3)
plot(st_simplify(Comuna_g, dTolerance = 1e3), add=TRUE, border="red") # la simplificacion con umbral de 1000m
plot(st_simplify(Comuna_g[28], dTolerance = 5e3), add=TRUE, border="green") # 5000m





# generamos poligono
pol <- st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
# segmentamos poligono con parametro de distancia 0.3
pol.seg <- st_segmentize(pol, 0.3)
# dibujamos resultados
plot(pol.seg, col = 'grey')
points(pol.seg[[1]])

# generamos linea
ls <- st_linestring(rbind(c(0,0),c(1,0),c(2,1),c(3,1)))
# segmentamos lineas
ls.seg <- st_segmentize(ls, 0.3)
# dibujamos resultados
plot(ls)
points(ls.seg)


## Acoplar ----

# creamos lineas
lines <- st_multilinestring(list(
  cbind(c(0, 1), c(1, 1.05)),
  cbind(c(0, 1), c(0, -.05)),
  cbind(c(1, .95, 1), c(1.05, .5, -.05))
))
# visualizamos
plot(lines, lwd=2, col='blue')

# creamos polinono
poly <- st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
plot(poly, add=TRUE, pch = 2)

# acoplamos poligono a lineas
snapped <- st_snap(poly, lines, tolerance=.1)
# agregamos al grafico
plot(snapped, col='red', add=TRUE, alpha=0.5)

## Poligonizar ----

# creamos multilinea
mls <- st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0), 4 ,2,byrow=TRUE)))
# poligonizamos
st_polygonize(st_sfc(mls))

# visualizamos
plot(mls)
plot(st_polygonize(st_sfc(mls)), add=TRUE) 

## Triangular ----

# triangulizamos la primera geometria del shape
triang <- st_triangulate(Comuna_g[1])
# dibujamos forma original y triangulos encima
plot(Comuna_g[1],col="black")
plot(triang, col=NULL, border="white", add=TRUE)





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

# extraigo la primera
escena <- df_disponible$id[2]

# defino las bandas que me interesa extraer
l8_bands <- ee$Image(escena)$select(c("B1", "B2", "B3", "B4", 
                                      "B5", "B6", "B7", "B9"))
# B1: Aerosol, B2: Blue, B3: Green, B4: Red
# B5: NIR, B6: SWIR 1, B7: SWIR 2, B9: Cirrus

# extraigo imagenes satelitales 
l8_img <- ee_as_raster(
  image = l8_bands,
  region = roi$bounds(),
  scale = 30)

png(file="Tiff/temuco.png", width=500, height=600)
plotRGB(l8_img, r=4, g=3, b=2, stretch = "lin")
dev.off()

# indices espectrales ----

# llamo funciones
source("R/indices.R")
veg <- NVDI(l8_img)

plot(veg)

funs <- lsf.str()

print(funs)

for(i in 1:length(funs)){
  png(file=paste0("Tiff/",funs[i],"_plot.png"), width=500, height=600)
  plot(do.call(funs[i], list(l8_img)))
  dev.off()
}

# analisis aculeo en el tiempo ----
analisis_aculeo <- function(anio){
  disponible <- ee$ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
    filterDate(paste0(anio,'-11-01'),paste0(anio,'-11-30'))$
    filterBounds(roi)$
    filterMetadata('CLOUD_COVER','less_than', 10)
  
  df_disponible <- ee_get_date_ic(disponible)%>%
    arrange(time_start)
  
  # extraigo la primera
  escena <- df_disponible$id[1]
  
  # defino las bandas que me interesa extraer para el NDWI
  l8_bands <- ee$Image(escena)$select(c("B2", "B3", "B4", "B5"))
  # B1: Aerosol, B2: Blue, B3: Green, B4: Red
  # B5: NIR, B6: SWIR 1, B7: SWIR 2, B9: Cirrus
  
  # extraigo imagenes satelitales 
  l8_img <- ee_as_raster(
    image = l8_bands,
    region = roi$bounds(),
    scale = 30)
  
  # extraigo valores de agua
  agua <- calc(NDWI(l8_img), fun = function(x) ifelse(x <= 0.2, NA, x))
  
  # guardo plot
  png(file=paste0("Tiff/temuco_anio",anio,".png"), width=500, height=600)
  plotRGB(l8_img, r=3, g=2, b=1, stretch = "lin")
  plot(agua, add = TRUE)
  dev.off()
}

analisis_aculeo(2020)
