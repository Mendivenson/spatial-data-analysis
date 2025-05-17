# Debido a la poca cantidad de puntos para las variables de evaporación y precipitación es preferible analizar las 
# varaibles agregadas mensualmente por lo que estás serán las mediciones que se analizarán de aquí en más. 
# En este script se realiza la conversión de coordenadas geográficas a cartesianas utilizando el sistema UTM, se transforman
# luego los datos a objetos de tipo geodata para el análisis de tendencia y la posterior estimación empírica del variograma
# respectivo a cada proceso. 

library(geoR)
library(sf)
library(dplyr)

mensual = read.table('data/mensual (2024-2025).txt')
mensual = mensual |> filter(AÑO == 2024, MES == 'JUN')                         # Observaciones de las variables
estaciones = read.table('data/estaciones.txt')                                 # Información de las estaciones

# ===> AÑADIR COORDENADAS GEOGRÁFICAS AL DATASET DE LOS DATOS MENSUALES: 
#      Los datos mensuales aún no tienen las coordenadas (latitud, longitud) asociadas a cada estación, sólo cuentan con
#      el ID de estación.

mensual = mensual |>                                                           
  left_join(estaciones |> 
              select(ID, latitud, longitud), by = c('ID' = 'ID'))

# ---------------------------------------------------------
# | NOTA: Las estaciones también cuentan con altitud, pero |
# | debido a la zona esta no varía demasiado.              |
# ---------------------------------------------------------


# ===> TRANSFORMACIÓN DE COORDENADAS:
#      La península de Yucatán se encuentra sobre la zona UTM 16N (EPSG:32616) completamente. Como se incluyen puntos en 
#      la zona occidental de Quintana Roo es necesario utilizar dos zonas UTM por lo que es mejor utilizar una proyección
#      UTM personalizada centrada en el centro de la zona de la península.

pers_tmerc <- "+proj=tmerc +lat_0=19.9 +lon_0=-89.31 +k=1 +x_0=310000 +y_0=210000 +ellps=GRS80 +units=m +no_defs"
mensual = cbind(mensual, st_coordinates(st_transform(st_as_sf(mensual, coords = c('longitud', 'latitud'), crs = 4326), crs = pers_tmerc)))
colnames(mensual)[10:11] = c('easting', 'northing')

# ===> ANÁLISIS DE TENDENCIA:
#      Debido a que se necesita la estacionariedad (Bien sea intrínseca o de segundo orden) de los datos es importante que
#      espacialmente no demuestren tendencias de ningún tipo. Por esta razón se revisan posibles relaciones entre el 
#      valor de la variable de intéres y las coordenadas deseadas. 

# ---DISPERSOGRAMA X COORDENADAS
# jpeg('plots/Análisis descriptivo/Tendencia (Dispersograma).jpg', width = 800, height = 900, quality = 80, res = 100)
l = matrix(0, nrow = 17, ncol = 2, byrow = T)
l[1,] = 1
l[2,] = l[3,] = c(2,3)
l[4,] = l[5,] = c(2,3)
l[6,] = l[7,] = c(4,5)
l[8,] = l[9,] = c(4,5)
l[10,] = l[11,] = c(6,7)
l[12,] = l[13,] = c(6,7)
l[14,] = l[15,] = c(8,9)
l[16,] = l[17,] = c(8,9)
layout(l)
par(mar = c(1,1,1,1))
plot(0,0, type = 'n', axes = F)
text(x = 0, y = 0.2, labels = 'Análisis de tendencia', xpd = T, font = 2, cex = 2.5)
nombres = stringr::str_to_title(stringr::str_replace_all(colnames(mensual)[4:7], '\\.', ' '))
for (i in 1:4){
  ind = c(4:7)[i]
  par(mar = c(2,4,2,2))
  plot(x = mensual$easting, mensual[,ind], ylab = '', pch = 20)
  title(ylab = nombres[i], cex.lab = 1.3, line = 2)
  if (i == 1){
    title(main = 'easting', font.main = 4, cex.main = 1.5)
  }
  par(mar = c(2,2,2,2))
  plot(x = mensual$northing, mensual[,ind], pch = 20)
  if (i == 1){
    title(main = 'northing', font.main = 4, cex.main = 1.5)
  }
}
# dev.off()

# --- CUARTILES
# jpeg('plots/Análisis descriptivo/Tendencia (Cuartiles).jpg', width = 1300, height = 1300, quality = 80, res = 150)
par(mfrow = c(2,2))
colores = rev(c('red', 'yellow', 'green',  'blue'))
# plot(mensual[,c("easting", "northing")], col = 'blue', pch = 1,
#      ylab = 'Y Coord', xlab = 'X Coord', main = 'Precipitación')
sim = 1:4
for (i in 1:4){
  ind = c(4:7)[i]
  puntos = mensual[,ind]
  puntos = puntos[!is.na(puntos)]
  coordenadas = mensual[!is.na(mensual[,ind]), c("easting", "northing")]
  cuartiles = quantile(puntos, probs = c(0.25, 0.5, 0.75), na.rm = T)
  clasificacion <- cut(
    puntos,
    breaks = c(-Inf, cuartiles, Inf),
    labels = 1:4)
  plot(coordenadas, col = colores[clasificacion], pch = sim[clasificacion], ylab = 'Y Coord', xlab = 'X Coord',
       main = nombres[i])
}
# dev.off()

# ===> ELIMINACIÓN DE LA TENDENCIA: 
#      Las variables lluvia total mensual y evaporación total mensual son las únicas que podrías presentar cierto tipo de
#      tendencia en relación a las coordenadas X e Y  que o bien podrían ser mínimas o deberse a datos de un valor muy bajo
#      en relación con sus vecinos (Puntos cercanos) en especial en lo que se refiere a la evaporación mensual

mensual$evap_res[!is.na(mensual$EVAPORACIÓN.MENSUAL)] = lm(EVAPORACIÓN.MENSUAL ~ easting + northing, 
            data =  mensual |> 
              filter(!is.na(EVAPORACIÓN.MENSUAL)))$residuals


mensual$prec_res[!is.na(mensual$LLUVIA.TOTAL.MENSUAL)] = lm(LLUVIA.TOTAL.MENSUAL ~ easting + northing, 
            data =  mensual |> 
              filter(!is.na(LLUVIA.TOTAL.MENSUAL)))$residuals




par(mfrow = c(1,2))
colores = rev(c('red', 'yellow', 'green',  'blue'))
# plot(mensual[,c("easting", "northing")], col = 'blue', pch = 1,
#      ylab = 'Y Coord', xlab = 'X Coord', main = 'Precipitación')
sim = 1:4
for (i in 1:2){
  ind = c(13:12)[i]
  puntos = mensual[,ind]
  puntos = puntos[!is.na(puntos)]
  coordenadas = mensual[!is.na(mensual[,ind]), c("easting", "northing")]
  cuartiles = quantile(puntos, probs = c(0.25, 0.5, 0.75), na.rm = T)
  clasificacion <- cut(
    puntos,
    breaks = c(-Inf, cuartiles, Inf),
    labels = 1:4)
  plot(coordenadas, col = colores[clasificacion], pch = sim[clasificacion], ylab = 'Y Coord', xlab = 'X Coord',
       main = nombres[i])
}

mensual = mensual |> select(-evap_res, -prec_res)


# ------------------------------------------------------------------------------------------
# | NOTA: Como el orden de los puntos no demuesttra ningún cambio significativo en su      |
# |       estructura luego de la regresión se dejan los puntos originales para posteriores |
# |       análisis de variogramas.                                                         |
# ------------------------------------------------------------------------------------------


# ===> VALIDACIÓN DE LOS VARIOGRAMAS EMPÍRICOS: 
#      En este paso, se crearán los objetos de tipo RData y se construiran los variogramas empiricos de cada una de las 
#      variables. 

geoData = as.list(rep(NA,4))
names(geoData) = colnames(mensual)[4:7]
for (i in 4:7){
  geoData[[i-3]] = as.geodata(mensual, coords.col = 10:11, data.col = i)
}

variog_emp = as.list(rep(NA,4))
names(variog_emp) = colnames(mensual)[4:7]
for (i in 1:4){
  variog_emp[[i]] = variog(geoData[[i]], pairs.min = 10)
}

# jpeg('plots/Análisis descriptivo/Variograma empírico.jpg', width = 1000, height = 900, quality = 80, res = 100)
par(mfrow = c(2,2))
for (i in names(variog_emp)){
  plot(variog_emp[[i]],
       main = stringr::str_to_title(stringr::str_replace_all(i, '\\.', ' ')), cex.main = 1.6)
}
# dev.off()

# Como los variogramas de temperatura máxima y mínima promedio parecen no representar ninguna curva en particular y más 
# más bien parecen una línea paralela al eje x (Efecto pepita puro) se hará el ajuste de modelos con las otras dos 
# variables: Evaporación total mensual y lluvia total mensual.

geoData = geoData[c("LLUVIA.TOTAL.MENSUAL","EVAPORACIÓN.MENSUAL")]
variog_emp = variog_emp[c("LLUVIA.TOTAL.MENSUAL","EVAPORACIÓN.MENSUAL")]

save(geoData, variog_emp, file = 'data/geoData.RData')
