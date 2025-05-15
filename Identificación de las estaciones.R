# Los datos son recolectados con corte a la información disponible en Mayo de 2025. 

library(rvest)
library(dplyr)

wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/'
setwd(wd)

iso_codes = data.frame('ISO' = c('ags', 'bc', 'bcs', 'camp', 'coah', 'col', 'chis', 'chih', 'df', 'dgo', 'gto', 'gro', 'hgo', 'jal','mex', 
              'mich', 'mor', 'nay', 'nl', 'oax', 'pue', 'qro', 'qroo', 'slp', 'sin', 'son', 'tab', 'tamps', 'tlax', 'ver',
              'yuc', 'zac'),
              'estado' = c('Aguascalientes', 'Baja California', 'Baja California Sur', 'Campeche',
                           'Coahuila', 'Colima', 'Chiapas', 'Chihuahua', 'Ciudad de México', 
                           'Durango', 'Guanajuato', 'Guerrero', 'Hidalgo', 'Jalisco', 'Estado de México', 
                           'Michoacán', 'Morelos', 'Nayarit', 'Nuevo León', 'Oaxaca', 'Puebla','Queretaro', 
                           'Quintana Roo', 'San Luis Potosí', 'Sinaloa', 'Sonora', 'Tabasco', 'Tamaulipas', 
                           'Tlaxcala', 'Veracruz', 'Yucatán', 'Zacatecas'))
estaciones = data.frame()

for (i in 1:nrow(iso_codes)){
  url = paste0(url <- "https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/catalogo/cat_",iso_codes$ISO[i],".html")
  datos <- read_html(url) |> 
    html_table(fill = TRUE)
  estaciones = rbind(estaciones, cbind(datos[[1]][-c(1:2),1:3], iso_codes$estado[i], iso_codes$ISO[i] , datos[[1]][-c(1:2),4]))
} 

colnames(estaciones) = c('ID', 'nombre', 'municipio', 'estado', 'estado (ISO)', 'situación')

nrow(estaciones)                              # Hay 5504 estaciones de las cuales
sum(estaciones$situación == 'Operando')       # 2780 siguen operando.

estaciones |>  
  group_by(estado) |> 
  summarise(n = n()) |> 
  arrange(-n)

estaciones[,c('latitud', 'longitud', 'altitud')] = 0
pb = txtProgressBar(min = 1, max = nrow(estaciones), style = 3)
# Faltan las ubicaciones de las estaciones: 
for (i in 1:nrow(estaciones)){
  coordenadas = readLines(paste0('https://smn.conagua.gob.mx/tools/RESOURCES/Normales_Climatologicas/Diarios/', estaciones$`estado (ISO)`[i],
                                 '/dia', ifelse(nchar(estaciones$ID[i]) == 4, paste0('0',estaciones$ID[i]), estaciones$ID[i]) ,'.txt'), n = 19)
  coordenadas = unlist(strsplit(coordenadas[17:19], ':'))[c(2,4,6)]
  estaciones[i,c('latitud', 'longitud', 'altitud')] = coordenadas
  setTxtProgressBar(pb, value = i)
}
close(pb)

# Las coordenadas geográficas se encuentran guardas con sus respectivas unidades (Grados o msnm) por lo que en un principio
# no pueden convertirse en ningún dato de tipo numérico.

altitud = unlist(strsplit(estaciones$altitud, ' '))
altitud = altitud[altitud != '']
altitud = matrix(altitud, ncol = 2, byrow = T)
altitud = as.numeric(altitud[,1])
estaciones$altitud = altitud

latitud = unlist(strsplit(estaciones$latitud, ' '))
latitud = latitud[latitud != '']
latitud = matrix(latitud, ncol = 2, byrow = T)
latitud = as.numeric(latitud[,1])
estaciones$latitud = latitud

longitud = unlist(strsplit(estaciones$longitud, ' '))
longitud = longitud[longitud != '']
longitud = matrix(longitud, ncol = 2, byrow = T)
longitud = as.numeric(longitud[,1])
estaciones$longitud = longitud

write.table(x = estaciones, file = 'data/estaciones.txt')
