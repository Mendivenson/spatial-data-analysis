finalset = 
  estaciones |> 
  select(ID, latitud, longitud, altitud) |> 
  right_join(datos |> 
               select(-fecha),
             by = c('ID' = 'ID')) 

finalset = cbind(finalset, st_coordinates(st_transform(estaciones_sf, 32614)))
colnames(finalset)[9:10] = c('easting', 'northing')

geo1 = as.geodata(finalset, coords.col = 9:10, data.col = 7)


plot(geo1, scatter3d = T)


a = variog(geo1, trend = '1st')
eyefit(a)

