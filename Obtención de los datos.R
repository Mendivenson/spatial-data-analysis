# En este script se realiza la operación de obtención y organización de los datos como tablas 
# para su posterior análisis de tendencia y modelación. En ese sentido, se escogerán los estados de la península de Yucatán
# en México (Yucatán, Quintana Roo y Campeche) debido a su superficie regular y sus condiciones climáticas más o menos 
# regulares y además se intentarán estudiar las variables de temperatura máxima, temperatura mínima, evaporación y
# precipitación. En un principio se recolectan datos tanto mensuales como diarios con el fin de revisar cuál de las agregaciones
# del proceso resulta más apta para la modelación espacial de los proceso descritos. 

wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/'
setwd(wd)
library(dplyr)

# La diferencia entre estaciones.txt y estaciones (Char).txt es que en la segunda las coordenadas aparecen con las unidades de 
# medida (Grados y metros sobre el nivel del mar) de forma que las columnas de coordenadas en el último no pueden ser tratadas
# como double en el código.
estaciones = read.table(file = 'data/estaciones.txt', header = T)
 
# ===> FILTRADO DE ESTACIONES PARA LA PENÍNSULA DE YUCATÁN:
estaciones = estaciones |>  filter(estado %in% c('Yucatán', 'Quintana Roo', 'Campeche'))
nrow(estaciones)
sum(estaciones$situación == 'Operando')

# ------------------------------------------------------------------
# | NOTA: Hay 240 estaciones disponibles para la toma de mediciones.| 
# | pero solamente 172 están en estado operativo. Igual se tomará la|
# | información de todas las estaciones en caso de que las          |
# | inoperativas tengan datos de intéres                            |
# ------------------------------------------------------------------


# ===> OBTENCIÓN DE DATOS DIARIOS (ENE/2025 - 08/MAR/2025): 
#      Las tablas se encuentran en una dirección de internet como archivos txt con un formato por columnas con las 
#      siguientes columnas: 
#         - FECHA
#         - PRECIPITACIÓN
#         - EVAP
#         - TMAX
#         - TMIN
#      Al inicio de cada archivo se encuentra la información de la estación (Incluyendo su estado operativo) y los datos
#      se organizan de forma descendente por fecha (Más viejo a más nuevo) 
#      
#      Por el orden de este archivo, a pesar de que se descarga todo el archivo por conveniencia, se conservan solamente 
#      las últimas 130 líneas (Los últimos 4 o 5 meses de datos). 
#      
#      ¡! Los datos nulos, aparecen como NULO en las tablas

diarios = data.frame()
pb = txtProgressBar(min = 1, max = nrow(estaciones), style = 3)
for (i in 1:nrow(estaciones)){
  info = estaciones[i,]
  url = paste0('https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/', info$estado..ISO., '/dia',
               ifelse(nchar(info$ID) == 4, paste0('0',info$ID), info$ID), '.txt')
  file = system(paste("curl -s", url, "| tail -n 130"), intern = TRUE)
  
  
  j = 1
  while (length(file) != 130){
    Sys.sleep(j * 2)
    file = system(paste("curl -s", url, "| tail -n 130"), intern = TRUE)
    j = j+1
  }
  
  diarios = rbind(diarios, cbind(info$ID, info$latitud, info$longitud, info$altitud,
                                 read.table(text = file, sep = '\t', header = F, na.strings = c('NULO'), 
                                            colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric'))))
  setTxtProgressBar(pb, value = i)
}
close(pb)


colnames(diarios) = c('ID','latitud', 'longitud', 'altitud', 'fecha', 'prec', 'evap', 'tmax', 'tmin')
diarios$fecha = as.Date(diarios$fecha, format = '%Y-%m-%d')

diarios = diarios |> filter(format(fecha, '%Y') == '2025')

# ¿QUÉ DÍA ESCOGER? Idealmente el que tenga la mayor cantidad de observaciones :D
for (col in colnames(diarios)[6:9]) {  # Columnas 6 a 9: prec, evap, tmax, tmin
  cat('Para la columna ', col, ' se tiene:\n')
  diarios |> 
    group_by(fecha) |> 
    summarise(n = sum(!is.na(.data[[col]]))) |>  # Usamos .data[[col]] para acceder a la columna
    arrange(desc(n)) |> 
    head(10) |> 
    print()
}

# Se escogería el día 27 del mes de Marzo (mes 03). Sin embargo, es probable que sean muy pocos datos. 

write.table(x = diarios, file = 'data/diarios(2025).txt')

diarios = diarios |> filter(fecha == '2025-03-27')

# ===> OBTENCIÓN DE DATOS MENSUALES: (ENERO/2024 -  MARZO/2025): 
#      Los archivos disponibles en la dirección especificada tienen formato txt. En cada txt hay varias tablas con la 
#      siguiente información:
#         - AÑO
#         - ENE, FEB, ..., DEC
#         - ACUM
#         - PROM
#         - MESES
#     Cada tabla sólo tiene una separación de una línea (\n) entre cada una y comienza con un título que describe la
#     variable medida (i.e. TEMPERATURA MÁXIMA PROMEDIO, LLUVIA MENSUAL TOTAL, etc.).
#     Por la forma del archivo, nos vemos obligados a leer todo el archivo y buscar los inicios y finales de cada tabla.
# 
# ... Esta parte la programó César :D Entonces dudas sobre el código con él ...

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


# NOS INTERESAMOS EN LAS VARIABLES QUE TAMBIÉN SE TOMAN DIARIAMENTE SOLAMENTE Y SOLAMENTE EN 2024 Y 2025:
datos = datos |> 
  filter(NombreVariable %in% 
           c('LLUVIA TOTAL MENSUAL', 'EVAPORACIÓN MENSUAL',
             'TEMPERATURA MÁXIMA PROMEDIO', 'TEMPERATURA MÍNIMA PROMEDIO')) |> 
  filter(AÑO %in% c('2024', '2025')) |> 
  select(-ACUM,-PROM, -MESES)

# Debido a la organización del archivo, se convierte a un formato parecido al que se tiene para los datos diarios:
meses = colnames(datos)[2:13]
mensual = data.frame()
for (id in unique(datos$ID)){
  tabla = as.data.frame(cbind('ID' = id, 
                              'AÑO' = c(rep(2024,12), rep(2025,4)), 
                              'MES' = c(meses,meses[1:4])))
  for (variable in c('LLUVIA TOTAL MENSUAL', 'EVAPORACIÓN MENSUAL', 'TEMPERATURA MÁXIMA PROMEDIO', 'TEMPERATURA MÍNIMA PROMEDIO')){
    columnas = as.double(datos |> 
                           filter(ID == id, NombreVariable == variable, AÑO == '2024') |> 
                           select(all_of(meses)) |>  
                           unlist())
    columnas = c(columnas, 
                 as.double(datos |> 
                             filter(ID == id, NombreVariable == variable, AÑO == '2025') |> 
                             select(all_of(meses[1:4])) |>  
                             unlist()))
    if (length(columnas) != 16) columnas = c(columnas, rep(NA, 16 - length(columnas)))
    tabla = cbind(tabla, columnas)
  }
  mensual = rbind(mensual, tabla)
}

colnames(mensual)[4:7] = c('LLUVIA TOTAL MENSUAL', 'EVAPORACIÓN MENSUAL', 'TEMPERATURA MÁXIMA PROMEDIO', 'TEMPERATURA MÍNIMA PROMEDIO')

# ¿QUÉ MES ESCOGER? Idealmente el que tenga la mayor cantidad de observaciones :D
for (col in colnames(mensual)[4:7]) {  # Columnas 6 a 9: prec, evap, tmax, tmin
  cat('Para la columna ', col, ' se tiene:\n')
  mensual |> 
    group_by(AÑO, MES) |> 
    summarise(n = sum(!is.na(.data[[col]]))) |>  # Usamos .data[[col]] para acceder a la columna
    arrange(desc(n)) |> 
    head(10) |> 
    print()
}

# Nos quedaremos con el mes de Junio del año 2024
write.table(x = mensual, file = 'data/mensual (2024-2025).txt')

mensual = mensual |> 
  filter(AÑO == 2024, MES == 'JUN')


# ===> IDENTIFICACIÓN ESPACIAL DE ESTACIONES CON DATOS PARA CADA VARIABLE:
#      Además de la cantidad de datos es deseable conocer la distribución espacial que tienen, si son uniformes es ideal 
#      seleccionar esa variable y/o día y/o mes. 
#      
#      Por esto a continuación se revisa la distribución de las estaciones operativas en la península de Yucatán y las
#      estaciones que reportan datos para cada uno de los sets seleccionados. 

# ¡! ----> Corra estas líneas para iniciar desde esta parte del script y no recolectar datos de nuevo
# estaciones = read.table('data/estaciones.txt')
# mensual = read.table('data/mensual (2024-2025).txt')
# diarios = read.table('data/diarios(2025).txt')
# mensual = mensual |> filter(AÑO == 2024, MES == 'JUN')
# diarios = diarios |> filter(fecha == '2025-03-27')


library(ggplot2)                 # Librería general para gráficos
library(sf)                      # Librería para manejo de polígonos (i.e Mapas)
library(ggspatial)               # Librería para graficación de mapas
library(rnaturalearth)           # Librerías para los fondos de los mapas
library(rnaturalearthdata)
library(prettymapr)

mexico = rnaturalearth::ne_states(country = "mexico", returnclass = "sf")                  # Polígonos de méxico
estaciones_sf = st_as_sf(estaciones, coords = c("longitud", "latitud"), crs = 4326)        # Coordenadas de las estaciones

# DISTRIBUCIÓN DE LAS ESTACIONES A LO LARGO DE LA PENÍNSULA: 
estaciones = ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +

  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray60", linewidth = 1) +

  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf,
    aes(color = situación, fill = NULL),  # Color del borde según 'situación'
    shape = 21,          # Forma que permite borde (color) y relleno (fill)
    size = 1.5,          # Tamaño del punto
    stroke = 0.8,        # Grosor del borde
    alpha = 0.9          # Transparencia
  ) +
  scale_color_manual(
    values = c("Operando" = "green3", "Suspendida" = "red3"),  # Bordes
    guide = guide_legend(override.aes = list(fill = "white"))) +  # Leyenda con relleno blanco
  scale_fill_manual(values = "white", guide = "none") +  # Relleno fijo (blanco)

  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +

  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))
estaciones

# ggsave(filename = 'plots/estaciones.jpg', plot = estaciones)


# DISTIBRUCIÓN DE LAS ESTACIONES CON INFORMACIÓN EL DÍA 27 DE MARZO DE 2025:


# --- Para la precipitación: 

diaprec =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(diarios |> 
                                         filter(!is.na(prec)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la precipitación el día 27/Marzo/2025",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Diaria)/precipitación.jpg', plot = diaprec)


# --- Para la evaporación:

diaevap =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(diarios |> 
                                         filter(!is.na(evap)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la evaporación el día 27/Marzo/2025",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Diaria)/evaporación.jpg', plot = diaevap)


# --- Para la temperatura máxima y mínima: 


diatem =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(diarios |> 
                                         filter(!is.na(tmax)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la temperatura máxima y mínima el día 27/Marzo/2025",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Diaria)/temperatura.jpg', plot = diatem)



# DISTIBRUCIÓN DE LAS ESTACIONES CON INFORMACIÓN PARA JUNIO DE 2024:

mensual = mensual |> mutate(ID = as.integer(ID))

# --- Para la precipitación: 

mesprec =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(mensual|> 
                                         filter(!is.na(`LLUVIA TOTAL MENSUAL`)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la precipitación el mes de Junio de 2024",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Mensuales)/precipitación.jpg', plot = mesprec)


# --- Para la evaporación:

mesevap =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(mensual|> 
                                         filter(!is.na(`EVAPORACIÓN MENSUAL`)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la evaporación el mes de Junio de 2024",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Mensuales)/evaporación.jpg', plot = mesevap)


# --- Para la temperatura máxima y mínima: 


mestem =
  ggplot() +
  annotation_map_tile(zoom = 7, type = 'hotstyle') +
  
  # Capa de polígonos estatales
  geom_sf(data = mexico |>
            filter(name %in% c('Campeche', 'Quintana Roo', 'Yucatán')),
          fill = NA, color = "gray40", linewidth = 0.5) +
  
  # Capa de estaciones (puntos con borde colorido y relleno blanco)
  geom_sf(
    data = estaciones_sf |> right_join(mensual|> 
                                         filter(!is.na(`TEMPERATURA MÁXIMA PROMEDIO`)),
                                       by = c('ID' = 'ID')),
    color = 'red', fill = 'white', size = 2.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  # Escala y norte
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  
  # Títulos y tema
  labs(title = "Estaciones meteorológicas en la Península de Yucatán con\ndatos para la temperatura máxima y mínima (Promedio) el mes de Junio de 2024",
       subtitle = "Datos de la Comisión Nacional de Aguas (CONAGUAS)",
       caption = "Fuente: Natural Earth, OpenStreetMap",
       color = 'Situación de la estación') +
  # theme_void() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 10))

# ggsave(filename = 'plots/estaciones (Mensuales)/temperatura.jpg', plot = mestem)

save(mexico, estaciones_sf, file = 'data/mapas.RData')
