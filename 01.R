set.seed(1)
pesos_10 <- sample(1:100, 10, replace = TRUE)
set.seed(2)
pesos_30 <- sample(1:100, 30, replace = TRUE)
set.seed(3)
pesos_50 <- sample(1:100, 50, replace = TRUE)
set.seed(4)
pesos_80 <- sample(1:100, 80, replace = TRUE)
set.seed(5)
pesos_100 <- sample(1:100, 100, replace = TRUE)
set.seed(6)
pesos_1000 <- sample(1:100, 1000, replace = TRUE)

evaluar_normalidad <- function(variable, nombre, n) {
  cat("\n", nombre, " (n=", n, "):\n", sep="")
  
  if(n <= 50) {
    
    shapiro_result <- shapiro.test(variable)
    cat("Shapiro-Wilk test: W =", round(shapiro_result$statistic, 6), 
        ", p-value =", round(shapiro_result$p.value, 6), "\n")
    
    if(shapiro_result$p.value > 0.05) {
      cat("Shapiro: Los datos SIGUEN una distribución normal (p > 0.05).\n")
      shapiro_normal <- TRUE
    } else {
      cat("Shapiro: Los datos NO siguen una distribución normal (p ≤ 0.05).\n")
      shapiro_normal <- FALSE
    }
    return(shapiro_normal)
  } else {
    lillie_result <- lillie.test(variable)
    cat("Lillie test: L=", round(lillie_result$statistic,6),
        ", p-value =", round(lillie_result$p.value, 6), "\n")
    if(lillie_result$p.value > 0.05) {
      cat("Los datos siguen una distribución normal.\n")
      return(TRUE)
    } else {
      cat("Los datos no siguen una distribución normal.\n")
      return(FALSE)
    }
  }
}
resultado_10 <- evaluar_normalidad(peso_10, "peso_10", 10)
resultado_30 <- evaluar_normalidad(peso_30, "peso_30", 30)
resultado_50 <- evaluar_normalidad(peso_50, "peso_50", 50)
resultado_80 <- evaluar_normalidad(peso_80, "peso_80", 80)
resultado_100 <- evaluar_normalidad(peso_100, "peso_100", 100)
resultado_1000 <- evaluar_normalidad(peso_1000, "peso_1000", 1000)