set.seed(123)

simulacion <- function(meta=10){
  # Estado inicial
  caramelos <- c(amarillo=15, cafe=15, naranja=15)
  chupetines <- 0
  iteraciones <- 0
  
  while(chupetines < meta){
    iteraciones <- iteraciones + 1
    
    # Regla 1: si hay al menos 1 de cada color -> formar chupetín
    if(all(caramelos >= 1)){
      caramelos <- caramelos - 1
      chupetines <- chupetines + 1
    } else {
      # Regla 2: si no se puede, aplicar intercambio
      if(chupetines >= 1){
        chupetines <- chupetines - 1
        
        # quitar un caramelo aleatorio
        color_rem <- sample(names(caramelos), 1)
        if(caramelos[color_rem] > 0){
          caramelos[color_rem] <- caramelos[color_rem] - 1
        }
        
        # añadir 4 caramelos aleatorios
        for(i in 1:4){
          color_add <- sample(names(caramelos), 1)
          caramelos[color_add] <- caramelos[color_add] + 1
        }
      }
    }
  }
  return(iteraciones)
}

# Ejecutamos varias simulaciones para ver el promedio
resultados <- replicate(1000, simulacion(meta=10))
cat("Iteraciones promedio:", mean(resultados), "\n")
hist(resultados, main="Distribución de iteraciones", xlab="Iteraciones")
