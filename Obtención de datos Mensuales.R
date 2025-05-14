# wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/' # Cumpu Campero
wd = 'REPOS GIT/spatial-data-analysis/'   # Cumpu Costeño

setwd(wd)

library(dplyr)
library(geoR)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(prettymapr)

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

datos = data.frame()
pb = txtProgressBar(min = 1, max = nrow(estaciones), style = 3)

for (i in 1:nrow(estaciones)) {
  info = estaciones[i,]
  url = paste0('https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Mensuales/', 
               info$estado..ISO., '/mes', 
               ifelse(nchar(info$ID) == 4, paste0('0', info$ID), info$ID), '.txt')
  
  file = system(paste("curl -s", url), intern = TRUE)
  
  # Si el archivo no tiene líneas, pasar a la siguiente estación
  if (length(file) == 0) {
    setTxtProgressBar(pb, value = i)
    next
  }
  
  # Extraer líneas donde comienzan las tablas (identificadas por AÑO en la primera columna)
  tabla_indices = grep("^AÑO", file)
  
  # Si no se encuentran al menos dos tablas, omitir esta estación
  if (length(tabla_indices) < 2) {
    setTxtProgressBar(pb, value = i)
    next
  }
  
  for (k in seq_along(tabla_indices)) {
    inicio_tabla = tabla_indices[k]
    fin_tabla = ifelse(k < length(tabla_indices), tabla_indices[k+1] - 2, length(file))
    
    # Obtener el nombre de la tabla (está unas líneas antes del encabezado)
    nombre_tabla = file[inicio_tabla - 1]
    nombre_tabla = gsub("\t", " ", nombre_tabla)
    nombre_tabla = trimws(nombre_tabla)
    
    # Extraer la subtabla como texto
    subfile = file[inicio_tabla:fin_tabla]
    texto_tabla = paste(subfile, collapse = "\n")
    
    # Intentar leer la tabla, continuar si falla
    tabla = tryCatch({
      read.table(text = texto_tabla, header = TRUE, sep = '\t', na.strings = c('NULO', '...', ''), 
                 colClasses = 'character', fill = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(tabla)) {
      tabla$ID = info$ID
      tabla$NombreVariable = nombre_tabla
      datos = rbind(datos, tabla)
    }
  }
  
  setTxtProgressBar(pb, value = i)
}
close(pb)


save(list =  c('datos', 'estaciones_sf', 'estaciones', 'mexico'), file = 'data/data2.RData')

load('data/data2.RData')
