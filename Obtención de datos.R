wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/'
setwd(wd)

library(dplyr)
library(geoR)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

# Identificación única de las estaciones en México
estaciones = read.table('data/estaciones.txt')


# ================================= GRÁFICO ESTACIONES A LO LARGO DE TODO MÉXICO =======================================

mexico = rnaturalearth::ne_states(country = "mexico", returnclass = "sf")  # Datos de México en formato sf
estaciones_sf = st_as_sf(estaciones, coords = c("longitud", "latitud"), crs = 4326)

mexicoMap = ggplot() +
  
  annotation_map_tile(zoom = 5) +  # "osm" para vista satelital, "cartolight" para relieve
  
  # Capa de polígonos estatales de México
  geom_sf(data = mexico, fill = NA, color = "gray40", linewidth = 0.3) +
  
  # Capa de estaciones (puntos)
  geom_sf(data = estaciones_sf, aes(color = situación), size = 0.5, alpha = 0.7) +
  scale_color_manual(values = c("Operando" = "green", "Suspendida" = "red")) +
  
  # Añadir escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Personalización del tema
  theme_void() +
  labs(title = "Estaciones meteorológicas en México",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(filename = 'plots/mexico.jpg', plot = mexicoMap)


# =============================== GRÁFICO ESTACIONES HABILITADAS ZONA METROPOLITANA ====================================

# Debido a que las estaciones varian en altitud, estaría bien escoger estaciones de zonas con comportamiento 
# climático regular y altura más o menos similar. Por esta razón se escogen las estaciones de la zona metropolitana
# del valle de México:

# https://todosloshechos.es/cuales-son-las-zonas-metropolitanas-de-mexico
# La zona metropolitana más grande de México se encuentra conformada por los siguientes estados:
estaciones = estaciones |>  
  filter(estado %in% c('Ciudad de México', 'Estado de México', 'Hidalgo', 'Puebla',
                       'Morelos', 'Queretaro', 'Tlaxcala') & situación != 'Suspendida')

estaciones_sf = estaciones_sf |>  
  filter(estado %in% c('Ciudad de México', 'Estado de México', 'Hidalgo', 'Puebla',
                       'Morelos', 'Queretaro', 'Tlaxcala') & situación != 'Suspendida')

mexico = mexico |> filter(name %in% c('Distrito Federal', 'México', 'Hidalgo', 'Puebla',
                                      'Morelos', 'Querétaro', 'Tlaxcala'))

metropolitana = ggplot() +
  
  # Capa de relieve (usando OpenStreetMap como fondo)
  annotation_map_tile(type = 'osm',zoom = 8) +  # "osm" para vista satelital, "cartolight" para relieve
  
  # Capa de polígonos estatales de México
  geom_sf(data = mexico, fill = NA, color = "gray10", linewidth = 0.4) +
  
  # Añadir escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Capa de estaciones (puntos)
  geom_sf(data = estaciones_sf, color = 'black', size = 1.5, alpha = 0.7, stroke = 0.4, shape = 21) +
  
  # Personalización del tema
  # theme_void() +
  labs(title = "Estaciones meteorológicas habilitadas en la zona metropolitana del Valle de México",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file = 'plots/metropilitana.jpg', plot = metropolitana)


# ================================ OBTENCIÓN DE LAS MEDICIONES EN LAS ESTACIONES ======================================= 

# Se tiene disponible la información de 500 estaciones en la zona escogida.
estaciones |> nrow()

# Sin embargo, en este momento se cuenta únicamente con la información de la ubicación de las estaciones y no con la 
# información de las mediciones de cada estación por lo que se procederá a obtener información (Diaria) para cada una de
# estas estaciones para el mes de Diciembre del año 2024. En general las estaciones miden nivel de precipitación, nivel de
# evaporación, temperatura máxima y temperatura mínima para un día en específico:

datos = data.frame()
pb = txtProgressBar(min = 1, max = nrow(estaciones), style = 3)
for (i in 1:nrow(estaciones)){
  info = estaciones[i,]
  url = paste0('https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/',info$estado..ISO., '/dia', 
               ifelse(nchar(info$ID) == 4, paste0('0',info$ID), info$ID), '.txt')
  file = system(paste("curl -s", url, "| tail -n 125"), intern = TRUE)
  
  
  j = 1
  while (length(file) != 125){
    Sys.sleep(j * 2)
    file = system(paste("curl -s", url, "| tail -n 125"), intern = TRUE)
    j = j+1
  }
  
  datos = rbind(datos, cbind(info$ID,read.table(text = file, sep = '\t', header = F, na.strings = c('NULO'), 
                                                colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric'))))
  setTxtProgressBar(pb, value = i)
}
close(pb)

colnames(datos) = c('ID', 'fecha', 'prec', 'evap', 'tmax', 'tmin')
datos$fecha = as.Date(datos$fecha, format = '%Y-%m-%d')

# Hasta el momento, se tienen datos de todos los días de los últimos 4 meses en el registro de la estación.
table(format(datos$fecha, format = '%Y'))

# Las estaciones se reducen a 246 cuando filtramos los datos de Diciembre de 2024 a Marzo de 2025.
datos = datos |> 
  filter((format(fecha, '%Y') == 2024 & format(fecha, '%m') == 12) | format(fecha, '%Y') == 2025)

save(list =  c('datos', 'estaciones_sf', 'estaciones', 'mexico'), file = 'data/data.RData')

# ===================================== ESCOGENCIA DE LAS VARIABLES A ESTUDIAR  ========================================

# Se escoge el primer día del mes de Febrero de 2025 por ser el de mayor cantidad de datos para la zona específica de 
# estudio. se procede a hacer el filtrado de los datos y su conversión a datos de tipo espacial:

load('data/data.RData')

datos = datos |> 
  filter(fecha == '2025-02-01')

estaciones_sf = estaciones_sf |> right_join(datos, by = c('ID' = 'ID'))

# Para este día se tiene el reporte de 195 estaciones. 

dia = ggplot() +
  
  # opencycle [x]
  # hotstyle [x]
  # Capa de relieve (usando OpenStreetMap como fondo)
  annotation_map_tile(type = 'hotstyle',zoom = 8) +  # "osm" para vista satelital, "cartolight" para relieve
  
  # Capa de polígonos estatales de México
  geom_sf(data = mexico, fill = NA, color = "gray10", linewidth = 0.4) +
  
  # Añadir escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Capa de estaciones (puntos)
  geom_sf(data = estaciones_sf, color = 'red', fill = 'white', size = 1.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  
  # Personalización del tema
  # theme_void() +
  labs(title = "Estaciones meteorológicas con datos para 01-Feb-2025 en la zona metropolitana\ndel Valle de México",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, Humanitarian OpenStreetMap Team") +
  theme(plot.title = element_text(hjust = 0, face = "bold"))

ggsave(file = 'plots/01Feb2025.jpg', plot = dia)

# ===> Conversión de coordenadas geográficas a coordenadas cartesianas: 

finalset = 
  estaciones |> 
  select(ID, latitud, longitud, altitud) |> 
  right_join(datos |> 
               select(-fecha),
             by = c('ID' = 'ID')) 

finalset = cbind(finalset, st_coordinates(st_transform(estaciones_sf, 32614)))
colnames(finalset)[9:10] = c('easting', 'northing')

write.table(x = finalset, file =  'data/info.txt', row.names = F)