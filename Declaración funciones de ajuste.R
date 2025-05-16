# A continuación se definen las funciones para ajuste por Mínimos Cuadrados Ponderados y Verosimilitud de los modelos de
# semivariograma válidos utilizando diferentes matrices de ponderación.

# Tenga en cuenta que el efecto pepita o nugget (Así como los parámetros de suavizamiento cuando el modelo los tenga) no
# hacen parte de los parámetros a estimar por lo que funcionan más como hiperparámetros que deben ser definidos de antemano
# utilizando, por ejemplo, eyefit.

library(geoR) 

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

all.equal(variog_est(obj = variog_emp$LLUVIA.TOTAL.MENSUAL, silla_init = 25800, rango_init = 118513,modelo = 'esferico', nugget = 0)[,2], 
          25800 - cov.spatial(obj = resultado[,"x"], cov.model = 'spherical', cov.pars = c(25800, 118513)))


# ===> FUNCIÓN OBJETIVO: Para poder minimizar apropiadamente y estimar el valor de los parámetros, la siguiente función calcula
#      el valor de la suma de cuadrados ponderados (Cressi o npairs o a partir de una matriz personalizada) a partir de una 
#      matriz de resultados proveniente de la función variog_est.  
    
mco = function(resultados, pond = 'cressi'){
  
  var_est = resultados[,"var_est"]
  var = resultados[,"var"]
  n = resultados[,"n"]
  
  # Cálculo de la métrica de ajuste
  if(is.character(pond)) {
    switch(pond,
           cressi = {
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
                        nugget = 0, pond = 'cressi', metodo = "L-BFGS-B") {
  
  # Unión de als funciones a optiimizar.
  fn_optim <- function(params) {
    resultados <- variog_est(obj = obj, 
                             silla_init = params[1],  # sill
                             rango_init = params[2],  # range
                             modelo = modelo, 
                             nugget = nugget)
    
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