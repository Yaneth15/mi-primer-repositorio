# ANÁLISIS DEL COEFICIENTE DE VARIACIÓN

rm(list = ls())

set.seed(42)
n <- 120

estudiantes <- data.frame(
  edad = round(rnorm(n, 21, 2)),                    
  calificaciones = round(rnorm(n, 8.2, 1.3), 1),   
  horas_estudio = round(rnorm(n, 25, 8)),           
  ingresos_familia = round(rnorm(n, 35000, 12000)), 
  gastos_mes = round(rnorm(n, 800, 250)),           
  materias = round(rnorm(n, 6, 1.5))            
)

estudiantes$edad[estudiantes$edad < 17] <- 17
estudiantes$edad[estudiantes$edad > 26] <- 26
estudiantes$calificaciones[estudiantes$calificaciones < 1] <- 1
estudiantes$calificaciones[estudiantes$calificaciones > 10] <- 10
estudiantes$horas_estudio[estudiantes$horas_estudio < 5] <- 5
estudiantes$ingresos_familia[estudiantes$ingresos_familia < 15000] <- 15000
estudiantes$gastos_mes[estudiantes$gastos_mes < 300] <- 300
estudiantes$materias[estudiantes$materias < 3] <- 3
estudiantes$materias[estudiantes$materias > 9] <- 9

cat("DATOS DE ESTUDIANTES UNIVERSITARIOS\n")
cat("=====================================\n")
cat("Número de estudiantes:", nrow(estudiantes), "\n")
cat("Variables analizadas:", ncol(estudiantes), "\n\n")
cat("Primeras 6 observaciones:\n")
print(head(estudiantes))


calcular_cv <- function(datos) {
  promedio <- mean(datos, na.rm = TRUE)
  desviacion <- sd(datos, na.rm = TRUE)
  cv_porcentaje <- (desviacion / promedio) * 100
  return(round(cv_porcentaje, 2))
}


interpretar_cv <- function(cv) {
  if(cv < 15) return("BAJA variabilidad - Datos homogeneos")
  if(cv < 25) return("MODERADA variabilidad")
  return("ALTA variabilidad - Datos heterogeneos")
}


cat("\nANÁLISIS DEL COEFICIENTE DE VARIACIÓN\n")
cat("=========================================\n")


resultados <- data.frame(
  Variable = character(),
  Promedio = numeric(),
  Desv_Std = numeric(),
  CV = numeric(),
  Interpretacion = character(),
  stringsAsFactors = FALSE
)

for(i in 1:ncol(estudiantes)) {
  variable_nombre <- names(estudiantes)[i]
  datos_variable <- estudiantes[, i]
  
  promedio <- round(mean(datos_variable), 2)
  desviacion <- round(sd(datos_variable), 2)
  cv <- calcular_cv(datos_variable)
  interpretacion <- interpretar_cv(cv)
  
  # Agregar a la tabla
  resultados <- rbind(resultados, data.frame(
    Variable = variable_nombre,
    Promedio = promedio,
    Desv_Std = desviacion,
    CV = cv,
    Interpretacion = interpretacion,
    stringsAsFactors = FALSE
  ))
  
  cat("\n", toupper(variable_nombre), ":\n", sep = "")
  cat("   Promedio:", promedio, "\n")
  cat("   Desviación estándar:", desviacion, "\n")
  cat("   Coeficiente de Variación:", cv, "%\n")
  cat("   ", interpretacion, "\n")
}


cat("\nTABLA RESUMEN\n")
cat("=================\n")
print(resultados)

resultados_ordenados <- resultados[order(resultados$CV), ]

cat("\nRANKING DE VARIABILIDAD (de menor a mayor)\n")
cat("=============================================\n")
for(i in 1:nrow(resultados_ordenados)) {
  variable <- resultados_ordenados$Variable[i]
  cv <- resultados_ordenados$CV[i]
  cat(i, ". ", variable, ": ", cv, "%\n", sep = "")
}

cat("\nGenerando gráficos...\n")


par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))

# Gráfico 1: Barras del CV
colores <- c("lightgreen", "yellow", "orange", "red", "purple", "pink")
barplot(resultados_ordenados$CV, 
        names.arg = resultados_ordenados$Variable,
        main = "Coeficiente de Variación por Variable",
        ylab = "CV (%)",
        col = colores[1:nrow(resultados_ordenados)],
        las = 2,
        cex.names = 0.8)


variable_alta_cv <- resultados_ordenados$Variable[nrow(resultados_ordenados)]
boxplot(estudiantes[, variable_alta_cv],
        main = paste("Distribución de", variable_alta_cv),
        ylab = variable_alta_cv,
        col = "lightblue")


hist(estudiantes[, variable_alta_cv],
     main = paste("Histograma:", variable_alta_cv),
     xlab = variable_alta_cv,
     col = "lightcoral",
     breaks = 15)


variable_baja_cv <- resultados_ordenados$Variable[1]
plot(estudiantes[, variable_baja_cv], estudiantes[, variable_alta_cv],
     main = "Comparación de Dispersión",
     xlab = paste(variable_baja_cv, "(CV bajo)"),
     ylab = paste(variable_alta_cv, "(CV alto)"),
     pch = 16, col = "blue")


par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

for(i in 1:ncol(estudiantes)) {
  variable_nombre <- names(estudiantes)[i]
  cv_valor <- resultados$CV[resultados$Variable == variable_nombre]
  
  hist(estudiantes[, i],
       main = paste(variable_nombre, "\nCV =", cv_valor, "%"),
       xlab = variable_nombre,
       col = rainbow(ncol(estudiantes))[i],
       breaks = 15)
}


cat("\nCONCLUSIONES DEL ANÁLISIS\n")
cat("=============================\n")

cv_promedio <- round(mean(resultados$CV), 2)
cat("CV promedio del conjunto de datos:", cv_promedio, "%\n")

# Identificar variables extremas
var_menos_variable <- resultados_ordenados$Variable[1]
var_mas_variable <- resultados_ordenados$Variable[nrow(resultados_ordenados)]
cv_menor <- resultados_ordenados$CV[1]
cv_mayor <- resultados_ordenados$CV[nrow(resultados_ordenados)]

cat("\nVariable MÁS homogénea:", var_menos_variable, "(CV =", cv_menor, "%)\n")
cat("Variable MÁS heterogénea:", var_mas_variable, "(CV =", cv_mayor, "%)\n")

cat("\nInterpretación práctica:\n")
cat("- CV < 15%: Los datos son muy similares entre sí\n")
cat("- CV 15-25%: Hay variabilidad moderada en los datos\n")  
cat("- CV > 25%: Los datos son muy diferentes entre sí\n")

cat("\nEl coeficiente de variación nos ayuda a entender\n")
cat("qué tan dispersos están nuestros datos respecto\n")
cat("a su promedio, sin importar las unidades de medida.\n")

# Restablecer configuración gráfica
par(mfrow = c(1, 1))

cat("\n¡Análisis completado exitosamente!\n")


cat("\nRESUMEN ESTADÍSTICO COMPLETO\n")
cat("============================\n")
print(summary(estudiantes))

cat("\nMATRIZ DE CORRELACIONES\n")
cat("=======================\n")
if(exists("estudiantes")) {
  matriz_cor <- cor(estudiantes)
  print(round(matriz_cor, 3))
} else {
  cat("Error: Primero ejecute todo el código desde el inicio\n")
}