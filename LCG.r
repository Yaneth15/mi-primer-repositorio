
library(profvis)
lcg <- function(n, seed = 123, a = 1664525, c = 1013904223, m = 2^32) {
  x <- numeric(n + 1)
  x[1] <- seed
  for (i in 2:(n + 1)) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  return(x[-1] / m) 
}

xorshift <- function(n, seed = 2463534242) {
  state <- as.integer(seed) # Asegurar que el estado sea un entero
  x <- numeric(n)
  for (i in 1:n) {
    state <- bitwXor(state, bitwShiftL(state, 13L))
    state <- bitwXor(state, bitwShiftR(state, 17L))
    state <- bitwXor(state, bitwShiftL(state, 5L))
    x[i] <- state
  }
  return((x - min(x)) / (max(x) - min(x)))
}

mersenne <- function(n) {
  runif(n)
}
# Evaluación de uniformidad
evaluar_uniformidad <- function(valores, nombre) {
  # Verificar si 'valores' es nulo o vacío
  if (is.null(valores) || length(valores) == 0) {
    message(paste("No se puede evaluar la uniformidad para", nombre, "porque no hay datos."))
    return(invisible(NULL))
  }
  
  # Preparar el diseño del gráfico y mostrar los plots
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1)) 
  hist(valores,
       main = paste("Histograma -", nombre),
       col = "lightblue",
       freq = FALSE,
       xlab = "Valores Generados",
       ylab = "Densidad")
  qqplot(qunif(ppoints(length(valores))), valores,
         main = paste("QQ-Plot -", nombre),
         xlab = "Cuantiles Teóricos U(0,1)",
         ylab = "Cuantiles Observados")
  abline(0, 1, col = "red", lwd = 2) # Aumentar el ancho de la línea para que sea más visible
  
  # Realizar y mostrar la prueba de Kolmogórov-Smirnov
  ks <- ks.test(valores, "punif", 0, 1)
  cat("\n")
  print(paste("Prueba de Kolmogórov-Smirnov para", nombre, ":"))
  print(ks)
  cat("\n---\n")
}
# 3. Medición de rendimiento con profvis
profvis({
  n <- 1e6
  
  # Generar datos
  lcg_vals <- lcg(n, seed = 123)
  xor_vals <- xorshift(n, seed = 2463534242)
  mt_vals  <- mersenne(n)
  
  # Guardar en data.frame
  df <- data.frame(
    LCG = lcg_vals,
    Xorshift = xor_vals,
    MT = mt_vals
  )
  
  # Operaciones para que aparezca en la pestaña "Data"
  medias <- apply(df, 2, mean)
  desv   <- apply(df, 2, sd)
  subset_df <- df[1:1000, ]
  df$Nueva <- df$LCG + df$MT
})

set.seed(123)
evaluar_uniformidad(lcg(1000), "LCG")

set.seed(123)
evaluar_uniformidad(xorshift(1000), "Xorshift")

set.seed(123)
evaluar_uniformidad(mersenne(1000), "Mersenne Twister")