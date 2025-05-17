# A continuación se definen las funciones para ajuste por Mínimos Cuadrados Ponderados y Verosimilitud de los modelos de
# semivariograma válidos utilizando diferentes matrices de ponderación.

# Tenga en cuenta que el efecto pepita o nugget (Así como los parámetros de suavizamiento cuando el modelo los tenga) no
# hacen parte de los parámetros a estimar por lo que funcionan más como hiperparámetros que deben ser definidos de antemano
# utilizando, por ejemplo, eyefit.

library(geoR) 
library(dplyr)

wd = '~/Documents/2) EP - Estadística Espacial/Proyecto/CONAGUAS/'
setwd(wd)
load(file = 'data/geoData.RData')

# ===> CÁLCULO DE LA VARIANZA TEÓRICA: Para poder empezar la mminimización, debemos tener una estimación de los objetivos. 
#      Esta función calcula el variograma teórica para un modelo dado. Obj es un objeto de tipo variograma obtenido aplicando
#      la función variogram a un objeto de tipo geodata.

variog_est <- function(obj, silla_init = 10, rango_init = 10, modelo = 'gauss', nugget = 0, kappa1 = 1) {
  
  # Definir todos los parámetros necesarios en el entorno de la función
  pepita <- nugget
  silla <- silla_init
  rango <- rango_init
  kappa <- kappa1
  lambda <- kappa1
  
  # Selección del modelo corregida
  modelo_func <- if(is.function(modelo)) {
    modelo
  } else {
    switch(modelo,
           esferico = function(h) {
             ifelse(h <= rango, 
                    pepita + silla * (1.5*h/rango - 0.5*(h/rango)^3),
                    pepita + silla)
           },
           exponencial = function(h) {
             pepita + silla * (1 - exp(-3 * h / rango))
           },
           gauss = function(h) {
             pepita + silla * (1 - exp(-3 * (h / rango)^2))
           },
           mattern = function(h) {
             scaled_h <- (sqrt(2*kappa)*h)/rango
             term <- (scaled_h^kappa) * (2^(1-kappa))/gamma(kappa) * besselK(scaled_h, kappa)
             pepita + silla * (1 - term)
           },
           cuadratico_racional = function(h) {
             constante <- 19 * (h/rango)^2
             pepita + silla * (constante)/(1 + constante)
           },
           potencial = function(h) {
             if(lambda < 0 | lambda >= 2) stop('Lambda debe estar en [0,2)')
             pepita + silla * h^lambda
           },
           seno_cardinal = function(h) {
             pepita + silla * (1 - (rango/h)*sin(h/rango))
           },
           coseno = function(h) {
             pepita + silla * (1 - cos(pi * h / rango))
           },
           logaritmico = function(h) {
             pepita + silla * log(h + 1e-9)  # Evitar log(0)
           },
           circular = function(h) {
             ifelse(h >= rango,
                    pepita + silla,
                    pepita + silla * (1 - (2/pi) * (h/rango * sqrt(1 - (h/rango)^2) + asin(h/rango))))
           },
           tetraesferico = function(h) {
             vec <- h / rango
             ifelse(h >= rango, 
                    pepita + silla,
                    pepita + silla * (1 - (15/8)*vec + (5/4)*vec^3 - (3/8)*vec^5))
           },
           estable = function(h) {
             if(lambda < 0 | lambda >= 2) stop('Lambda debe estar en [0,2)')
             pepita + silla * (1 - exp(-3 * (h / rango)^lambda))
           },
           stop("Modelo no reconocido")
    )
  }
  
  resultado = cbind('var_est' = obj$v, 'var' = modelo_func(obj$u), 'n' = obj$n)
  
  return(resultado)
}


# --------------------------------------------------------------------------------
# | NOTA: Los variogramas se están calculando de forma similar pues dan lo mismo |
# --------------------------------------------------------------------------------

resultado = variog_est(obj = variog_emp$LLUVIA.TOTAL.MENSUAL, silla_init = 25800, rango_init = 118513,modelo = 'esferico', nugget = 0) 
all.equal(resultado[,2], 
          25800 - cov.spatial(obj = variog_emp$EVAPORACIÓN.MENSUAL$u, cov.model = 'spherical', cov.pars = c(25800, 118513)))


# ===> FUNCIÓN OBJETIVO: Para poder minimizar apropiadamente y estimar el valor de los parámetros, la siguiente función calcula
#      el valor de la suma de cuadrados ponderados (Cressi o npairs o a partir de una matriz personalizada) a partir de una 
#      matriz de resultados proveniente de la función variog_est.  

mco = function(resultados, pond = 'cressie'){
  
  var_est = resultados[,"var_est"]
  var = resultados[,"var"]
  n = resultados[,"n"]
  
  # Cálculo de la métrica de ajuste
  if(is.character(pond)) {
    switch(pond,
           cressie = {
             sum(n * ((var_est / var) - 1) ^ 2)
           },
           size = {
             sum(n * (var_est - var)^2)
           },
           stop("Método de ponderación no reconocido")
    )
  } else if(is.matrix(pond)) {
    # Implementar lógica para matriz de pesos personalizada
    sum(n * (var_est - var)^2)
  } else {
    stop("Tipo de ponderación no válido")
  }
}

# ===> MINIMIZACIÓN: Combinando la función de cálculo de los valores del variograma teórico y la del cálculo de la suma de cuadrados
#      es posible conseguir una sola función que genere la optimización correspondiente utilizando optim. Tenga en cuenta que
#      sólo se optimizan para los parámetros de silla y rango por lo que el efecto pepita y los valores de suavizamiento (Cuando se
#      utilizan) son fijos.

estimar_mco <- function(obj, silla_init = 1000, rango_init = 1000, modelo = 'gauss', 
                        nugget = 0, kappa = 1, pond = 'cressie', metodo = "L-BFGS-B") {
  
  if (modelo == 'lineal'){
    X = (obj$u - mean(obj$u))
    beta1 = sum(X * (obj$v - mean(obj$v)))/sum(X^2) 
    beta0 = mean(obj$v) - beta1 * mean(obj$u)
    return(list(
      pepita = beta0,
      pendiente = beta1, 
      cressie = sum(obj$n * (obj$v/(beta0 + beta1 * obj$u)- 1)^2)
    ))
  }
  # Unión de als funciones a optiimizar.
  fn_optim <- function(params) {
    resultados <- variog_est(obj = obj, 
                             silla_init = params[1],  # sill
                             rango_init = params[2],  # range
                             modelo = modelo, 
                             nugget = nugget, 
                             kappa = kappa)
    
    mco(resultados, pond = pond)
  }
  
  # Optimización:
  opt_result <- optim(par = c(silla_init, rango_init),
                      fn = fn_optim,
                      method = metodo, 
                      lower = 0, upper = Inf)
  
  # Presentación de resultados
  list(
    sill = opt_result$par[1],
    range = opt_result$par[2],
    value = opt_result$value,
    convergence = opt_result$convergence,
    message = opt_result$message
  )
}

ajuste1 <- estimar_mco(obj = variog_emp$LLUVIA.TOTAL.MENSUAL,
                       silla_init = 25800,
                       rango_init = 118513,
                       metodo = "Nelder-Mead",
                       modelo = "esferico", 
                       pond = 'size',
                       nugget = 2721)
ajuste1
variofit(variog_emp$LLUVIA.TOTAL.MENSUAL, ini.cov.pars = c(25800, 118513), cov.model = 'spherical', fix.nugget = T, nugget = 2721, weights = 'npairs')

# --------------------------------------------------------------------
# | NOTA: Los valores estimados son iguales para diferentes modelos  |
# | por lo que aparentemente la función de estimación está bien hecha| 
# --------------------------------------------------------------------


# ===> AJUSTE DE MODELOS: Posterior al ajuste de parámetros iniciales por medio de eyefit, se realiza ajuste de modelos con
#      la función creada para algunos modelos seleccionados. 


# --- AJUSTE LLUVIA TOTAL MENSUALDE
lluvia = as.data.frame(rbind(c('exponencial', 'exponential',20000, 67000, 15000, 1), 
                             c('seno_cardinal', 'wave', 19000, 50000, 15000, 1), 
                             c('mattern', 'matern', 20000, 55000, 15000, 2),
                             c('gauss', 'gaussian', 30000, 169000, 15000, 1)))
colnames(lluvia) = c('modelo', 'call', 'silla', 'rango', 'pepita', 'kappa') 
lluvia = lluvia |> 
  mutate(silla = as.double(silla),  
         rango = as.double(rango), 
         pepita = as.double(pepita), 
         kappa = as.double(kappa))
ajuste_lluvia = as.list(rep(NA, 4))
names(ajuste_lluvia) = lluvia[,1]

resultados = c()
for (i in 1:4){
  modelo = lluvia$modelo[i]
  silla = lluvia$silla[i]
  rango = lluvia$rango[i]
  pepita = lluvia$pepita[i]
  kappa = lluvia$kappa[i]
  
  ajuste_lluvia[[i]] = estimar_mco(obj = variog_emp$LLUVIA.TOTAL.MENSUAL, silla_init = silla, rango_init = rango, 
                                   modelo = modelo, nugget = pepita, kappa = kappa, pond = 'cressie')
  
  resultados = rbind(resultados,
                     c(pepita, ifelse(modelo == 'mattern', kappa, NA), 
                       ajuste_lluvia[[i]]$sill, ajuste_lluvia[[i]]$range, 
                       ajuste_lluvia[[i]]$value))
}

rownames(resultados) = lluvia$call; colnames(resultados) = c('nugget', 'kappa', 'silla', 'rango', 'Valor mínimo')
ajuste_lluvia = unlist(lapply(ajuste_lluvia, FUN = function(x) x[1:2]))
names(ajuste_lluvia) = NULL
ajuste_lluvia = matrix(ajuste_lluvia, ncol = 2, byrow = T)
lluvia = cbind(lluvia, ajuste_lluvia)
colnames(lluvia)[7:8] = c('silla_est', 'rango_est')


h = 1:6000 * 100
estimaciones = c()
for (i in 1:4){
  estimaciones = cbind(estimaciones, 
                       variog_est(list(u = h, v = h, n = 1), 
                                  silla_init = lluvia$silla_est[i],
                                  rango_init = lluvia$rango_est[i],
                                  modelo = lluvia$modelo[i],
                                  nugget = lluvia$pepita[i], 
                                  kappa1 = lluvia$kappa[i])[,'var'])
}
estimaciones = as.data.frame(estimaciones)
colnames(estimaciones) = stringr::str_to_title(lluvia$call)


# jpeg('plots/ajuste/Lluvia (MCO).jpg', width = 1500, height = 1300, quality = 80, res = 150)
plot(variog_emp$LLUVIA.TOTAL.MENSUAL, main = 'Lluvia total mensual\nen la península de Yucatán', 
     pch = 16, type = 'n', ylab = 'Semivarianza', xlab = 'Distancia', cex.main = 1.5)
colores = RColorBrewer::brewer.pal(n = 4, name = 'Set1')
for (i in 1:4){
  lines(x = h, y = estimaciones[,i], col = colores[i], lty = 'solid', lwd = 1.5)
}
points(x = variog_emp$LLUVIA.TOTAL.MENSUAL$u, y = variog_emp$LLUVIA.TOTAL.MENSUAL$v, pch = 21, col = 'black', bg = 'white')
legend('bottomright', legend = colnames(estimaciones), bty = 'n', lty = 'solid', col = colores, lwd = 2, inset = c(0.01,0.01))
# dev.off()

write.table(x = resultados, file = 'data/ajuste/lluvia (MCO).txt')


# --- AJUSTE EVAPORACIÓN MENSUAL

evap = as.data.frame(rbind(c('exponencial', 'exponential',3800, 250000, 800, 1), 
                           c('esferico', 'spherical', 4000, 508000, 800, 1), 
                           c('mattern', 'matern', 3850, 27800, 800, 1),
                           c('gauss', 'gaussian', 4000, 250000, 800, 0.5)))
colnames(evap) = c('modelo', 'call', 'silla', 'rango', 'pepita', 'kappa') 
evap = evap |> 
  mutate(silla = as.double(silla),  
         rango = as.double(rango), 
         pepita = as.double(pepita), 
         kappa = as.double(kappa))
ajuste_evap = as.list(rep(NA, 4))
names(ajuste_evap) = evap[,1]

resultados = c()
for (i in 1:4){
  modelo = evap$modelo[i]
  silla = evap$silla[i]
  rango = evap$rango[i]
  pepita = evap$pepita[i]
  kappa = evap$kappa[i]
  
  ajuste_evap[[i]] = estimar_mco(obj = variog_emp$EVAPORACIÓN.MENSUAL, silla_init = silla, rango_init = rango, 
                                 modelo = modelo, nugget = pepita, kappa = kappa, pond = 'cressie')
  
  resultados = rbind(resultados,
                     c(pepita, ifelse(modelo == 'mattern', kappa, NA), 
                       ajuste_evap[[i]]$sill, ajuste_evap[[i]]$range, 
                       ajuste_evap[[i]]$value))
}

rownames(resultados) = evap$call; colnames(resultados) = c('nugget', 'kappa', 'silla', 'rango', 'Valor mínimo')
ajuste_evap = unlist(lapply(ajuste_evap, FUN = function(x) x[1:2]))
names(ajuste_evap) = NULL
ajuste_evap = matrix(ajuste_evap, ncol = 2, byrow = T)
evap = cbind(evap, ajuste_evap)
colnames(evap)[7:8] = c('silla_est', 'rango_est')

evap_lineal = estimar_mco(obj = variog_emp$EVAPORACIÓN.MENSUAL, modelo = 'lineal')

h = 1:6000 * 100
estimaciones = c()
for (i in 1:4){
  estimaciones = cbind(estimaciones, 
                       variog_est(list(u = h, v = h, n = 1), 
                                  silla_init = evap$silla_est[i],
                                  rango_init = evap$rango_est[i],
                                  modelo = evap$modelo[i],
                                  nugget = evap$pepita[i], 
                                  kappa1 = evap$kappa[i])[,'var'])
}
estimaciones = as.data.frame(estimaciones)
colnames(estimaciones) = stringr::str_to_title(evap$call)

estimaciones = cbind(estimaciones,
                     lineal = evap_lineal$pepita + evap_lineal$pendiente * h)

# jpeg('plots/ajuste/evap (MCO).jpg', width = 1500, height = 1300, quality = 80, res = 150)
plot(variog_emp$EVAPORACIÓN.MENSUAL, main = 'Evaporación total mensual\nen la península de Yucatán', 
     pch = 16, type = 'n', ylab = 'Semivarianza', xlab = 'Distancia', cex.main = 1.5)
colores = RColorBrewer::brewer.pal(n = 5, name = 'Set1')
for (i in 1:5){
  lines(x = h, y = estimaciones[,i], col = colores[i], lty = 'solid', lwd = 1.5)
}
points(x = variog_emp$EVAPORACIÓN.MENSUAL$u, y = variog_emp$EVAPORACIÓN.MENSUAL$v, pch = 21, col = 'black', bg = 'white')
legend('bottomright', legend = c(colnames(estimaciones), 'lineal'), bty = 'n', lty = 'solid', col = colores, lwd = 2, inset = c(0.01,0.01))
# dev.off()

resultados = rbind(resultados, lineal = c(evap_lineal$pepita, NA, NA, NA, evap_lineal$cressie))

write.table(x = resultados, file = 'data/ajuste/evaporación (MCO).txt')
