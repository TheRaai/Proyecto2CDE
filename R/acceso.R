pacman::p_load(sp, data.table, ggplot2, stplanr, osrm, sf, mapview, tidyverse, leaflet, leafem, RColorBrewer)


pos_huerquehue = c(-71.3959, -39.0819)
pos_p_conguillo = c(-71.3900, -38.4000)
pos_cerro_nnienol = c(-72.3234, -38.4341)
pos_china_muerta = c(-71.2600, -38.4200)
pos_santuario_canni = c(-71.7667, -39.2552)
pos_salto_princesa = c(-71.6733, -38.4745)

pos_temuco =c(-72.5905, -38.7384)

lugares <- tribble(~nombre, ~lon, ~lat,
                   'Temuco', pos_temuco[1], pos_temuco[2],
                   'Huerquehue', pos_huerquehue[1], pos_huerquehue[2],
                   'Parque Conguillo', pos_p_conguillo[1], pos_p_conguillo[2],
                   'Cerro Ñienol', pos_cerro_nnienol[1], pos_cerro_nnienol[2],
                   "parque China Muerta",pos_china_muerta[1], pos_china_muerta[2],
                   'Santuario Cañi', pos_santuario_canni[1], pos_santuario_canni[2],
                   'Salto de la Princesa', pos_salto_princesa[1], pos_salto_princesa[2])

lugares_sf <- st_as_sf(lugares, coords = c("lon", "lat"))

list_rutas <- vector(mode = "list", length = 6)

camino <- function(desde, hasta){
  tmp <- route(from = desde,
              to = hasta,
              route_fun = osrmRoute,
              returnclass = "sf")
  return(tmp)
  }

list_rutas[[1]] = camino(pos_temuco,pos_huerquehue)
list_rutas[[2]] = camino(pos_temuco,pos_p_conguillo)
list_rutas[[3]] = camino(pos_temuco,pos_cerro_nnienol)
list_rutas[[4]] = camino(pos_temuco,pos_china_muerta)
list_rutas[[5]] = camino(pos_temuco,pos_santuario_canni)
list_rutas[[6]] = camino(pos_temuco,pos_salto_princesa)

#isocronas cada 30 minutos entre 0 y 1,5 horas
isocrona = osrmIsochrone(loc = pos_temuco, breaks = seq(from = 0, to = 150, by = 30))


m <- mapview(isocrona, legend = FALSE) + mapview(list_rutas, legend = FALSE) + mapview(lugares_sf)

m
