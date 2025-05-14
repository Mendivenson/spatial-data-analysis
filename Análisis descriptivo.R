# En este script se utiliza la información obtenida de las mediciones de las estaciones metereológicas ubicadas en la zona
# metropolitana del valle de México para analizar si las diferentes variables cuentan o no con tendencia asociada a las 
# coordenadas espaciales y se analizan también los variogramas empíricos de cada una de las variables para finalmente 
# seleccionar únicamente dos de las 4 variables que mide cada estación (A saber precipitación, evaporación, temperatura
# máxima y mínima).

wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/'
setwd(wd)
library(geoR) 
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)

data = read.table(file = 'data/info.txt', header = T)

load(file = 'data/data.RData')

# =============================== REVISIÓN DISTRIBUCIÓN DE LAS ESTACIONES POR VARIABLE =================================
# Lastimosamente no todas las variables tienen la misma cantidad de registros para el día seleccionado. Por lo que la
# selección de las variables de estudio debe tener esto en cuenta.

prec = ggplot() +
  annotation_map_tile(zoom = 8, type = 'hotstyle') +
  geom_sf(data = mexico, fill = NA, color = "gray40", linewidth = 0.3) +
  geom_sf(data = estaciones_sf |> right_join(data |> filter(!is.na(prec))), color = 'red', fill = 'white', 
          size = 1.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  labs(title = "Estaciones meteorológicas en la zona metropolitana del valle de\nMéxico con datos para la precipitación el día 01-Feb-2025",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0, face = "bold"))
ggsave(filename = 'plots/Análisis descriptivo/precipitación.jpg', plot = prec)

evap = ggplot() +
  annotation_map_tile(zoom = 8, type = 'hotstyle') +
  geom_sf(data = mexico, fill = NA, color = "gray40", linewidth = 0.3) +
  geom_sf(data = estaciones_sf |> right_join(data |> filter(!is.na(evap))), color = 'red', fill = 'white', 
          size = 1.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  labs(title = "Estaciones meteorológicas en la zona metropolitana del valle de\nMéxico con datos para la evaporación el día 01-Feb-2025",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0, face = "bold"))
ggsave(filename = 'plots/Análisis descriptivo/evaporación.jpg', plot = evap)

max = ggplot() +
  annotation_map_tile(zoom = 8, type = 'hotstyle') +
  geom_sf(data = mexico, fill = NA, color = "gray40", linewidth = 0.3) +
  geom_sf(data = estaciones_sf |> right_join(data |> filter(!is.na(tmax))), color = 'red', fill = 'white', 
          size = 1.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  labs(title = "Estaciones meteorológicas en la zona metropolitana del valle de\nMéxico con datos para la temperatura máxima el día 01-Feb-2025",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0, face = "bold"))
ggsave(filename = 'plots/Análisis descriptivo/máxima.jpg', plot = max)

min = ggplot() +
  annotation_map_tile(zoom = 8, type = 'hotstyle') +
  geom_sf(data = mexico, fill = NA, color = "gray40", linewidth = 0.3) +
  geom_sf(data = estaciones_sf |> right_join(data |> filter(!is.na(tmin))), color = 'red', fill = 'white', size = 1.5, alpha = 0.85, stroke = 0.4, shape = 21) +
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "bl", which_north = "false") +
  labs(title = "Estaciones meteorológicas en la zona metropolitana del valle de\nMéxico con datos para la temperatura mínima el día 01-Feb-2025",
       subtitle = "Datos del Servicio Meteorológico Nacional",
       caption = "Fuente: Natural Earth, OpenStreetMap") +
  theme(plot.title = element_text(hjust = 0, face = "bold"))
ggsave(filename = 'plots/Análisis descriptivo/mínima.jpg', plot = min)

# =============================================== ANÁLISIS DE TENDENCIA ================================================
# Las variables evaporación, temperatura máxima y mínima aparentemente tienen una relación lineal con la coordenada en Y,
# mientras que la varaible precipitación presenta exceso de ceros. 

jpeg('plots/Análisis descriptivo/tendencia.jpg', width = 600, height = 900, quality = 80, res = 100)
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
text(x = 0, y = 0, labels = 'Análisis de tendencia', xpd = T, font = 2, cex = 2.5)
nombres = c('Precipitación', 'Evaporación', 'Temperatura máxima', 'Temperatura mínima')
for (i in 5:8){
  par(mar = c(2,4,2,2))
  plot(x = data$easting, data[,i], ylab = '', pch = 20)
  title(ylab = nombres[i-4], cex.lab = 1.3, line = 2)
  if (i == 5){
    title(main = 'easting', font.main = 4, cex.main = 1.5)
  }
  par(mar = c(2,2,2,2))
  plot(x = data$northing, data[,i], pch = 20)
  if (i == 5){
    title(main = 'northing', font.main = 4, cex.main = 1.5)
  }
}
dev.off()

jpeg('plots/Análisis descriptivo/cuartiles.jpg', width = 1300, height = 1300, quality = 80, res = 150)
par(mfrow = c(2,2))
colores = rev(c('red', 'yellow', 'green',  'blue'))
plot(data[,c("easting", "northing")], col = 'blue', pch = 1, 
     ylab = 'Y Coord', xlab = 'X Coord', main = 'Precipitación')
sim = 1:4
for (i in 6:8){
  puntos = data[,i]
  puntos = puntos[!is.na(puntos)]
  coordenadas = data[!is.na(data[,i]), c("easting", "northing")]
  cuartiles = quantile(puntos, probs = c(0.25, 0.5, 0.75), na.rm = T)
  clasificacion <- cut(
    puntos,
    breaks = c(-Inf, cuartiles, Inf),
    labels = 1:4)
  plot(coordenadas, col = colores[clasificacion], pch = sim[clasificacion], ylab = 'Y Coord', xlab = 'X Coord', 
       main = nombres[i-4])
}
dev.off()

# ===> Se quita la tendencia de las variables evaporación, temperatura máxima y mínima

data = data |> arrange(northing)
data$evap_res[!is.na(data[,"evap"])] = lm(evap ~ northing, data = data)$residuals
max_loess = loess(data$tmax[!is.na(data$tmax)] ~ data$northing[!is.na(data$tmax)])
data$max_loess[!is.na(data[,"tmax"])] = max_loess$fitted
data$max_res[!is.na(data[,"tmax"])] = max_loess$residuals
min_loess = loess(data$tmin[!is.na(data$tmin)] ~ data$northing[!is.na(data$tmin)])
data$min_loess[!is.na(data[,"tmin"])] =  min_loess$fitted
data$min_res[!is.na(data[,"tmin"])] = min_loess$residuals

jpeg('plots/Análisis descriptivo/tendenciaSin.jpg', width = 600, height = 900, quality = 80, res = 100)
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
text(x = 0, y = -1.7, labels = '(Tendencia eliminada)', xpd = T, cex = 1.4)
nombres = c('Precipitación', 'Evaporación', 'Temperatura máxima', 'Temperatura mínima')
for (i in 1:4){
  ind = c(5,11,13,15)[i]
  par(mar = c(2,4,2,2))
  plot(x = data$easting, data[,ind], ylab = '', pch = 20)
  title(ylab = nombres[i], cex.lab = 1.3, line = 2)
  if (i == 1){
    title(main = 'easting', font.main = 4, cex.main = 1.5)
  }
  par(mar = c(2,2,2,2))
  plot(x = data$northing, data[,ind], pch = 20)
  if (i == 1){
    title(main = 'northing', font.main = 4, cex.main = 1.5)
  }
}
dev.off()


jpeg('plots/Análisis descriptivo/cuartilesSin.jpg', width = 1300, height = 1300, quality = 80, res = 150)
par(mfrow = c(2,2))
colores = rev(c('red', 'yellow', 'green',  'blue'))
plot(data[,c("easting", "northing")], col = 'blue', pch = 1, 
     ylab = 'Y Coord', xlab = 'X Coord', main = 'Precipitación')
sim = 1:4
for (i in 2:4){
  ind = c(5,11,13,15)[i]
  puntos = data[,ind]
  puntos = puntos[!is.na(puntos)]
  coordenadas = data[!is.na(data[,ind]), c("easting", "northing")]
  cuartiles = quantile(puntos, probs = c(0.25, 0.5, 0.75), na.rm = T)
  clasificacion <- cut(
    puntos,
    breaks = c(-Inf, cuartiles, Inf),
    labels = 1:4)
  plot(coordenadas, col = colores[clasificacion], pch = sim[clasificacion], ylab = 'Y Coord', xlab = 'X Coord', 
       main = nombres[i])
}
dev.off()

temMax = as.geodata(data, coords.col = 9:10, data.col = 15)
temMin = as.geodata(data, coords.col = 9:10, data.col = 13)
evap = as.geodata(data, coords.col = 9:10, data.col = 11)
prec = as.geodata(data, coords.col = 9:10, data.col = 5)

jpeg('plots/Análisis descriptivo/variog.jpg', width = 1600, height = 1300, quality = 80, res = 150)
par(mfrow = c(2,2))
plot(variog(temMax, trend = '1st'), main = 'Temperatura máxima')
plot(variog(temMin, trend = '1st'), main = 'Temperatura mínima')
plot(variog(evap, trend = '1st'), main = 'Evaporación')
plot(variog(prec), main = 'Precipitación')
dev.off()
