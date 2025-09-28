
# Evaluación estadística del generador Mersenne Twister
# Análisis visual y estadístico de números pseudoaleatorios
require(ggplot2)
require(GGally)  
require(plotly)

# Configuración inicial del generador
set.seed(456789, kind = "Mersenne-Twister")

# Generar muestra de números aleatorios
cantidad <- 12000
numeros_aleatorios <- runif(cantidad)

# Crear conjunto de datos para visualizaciones
datos_graf <- data.frame(
  anterior = numeros_aleatorios[1:(cantidad-1)],
  siguiente = numeros_aleatorios[2:cantidad]
)

# Preparar datos para el histograma
df_histograma <- data.frame(valores = numeros_aleatorios)

grafico1 <- ggplot(df_histograma, aes(x = valores)) +
  geom_histogram(bins = 35, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(
    title = "Distribución de números pseudoaleatorios",
    subtitle = "Generador: Mersenne Twister",
    x = "Valores generados",
    y = "Conteo"
  ) +
  theme_classic()

print("Mostrando histograma:")
ggplotly(grafico1)

# Visualización 2: Gráfico de dispersión para pares


grafico2 <- ggplot(datos_graf, aes(x = anterior, y = siguiente)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 0.8) +
  labs(
    title = "Relación entre números consecutivos",
    x = "Número en posición i",
    y = "Número en posición i+1"
  ) +
  theme_bw()

print("Gráfico de dispersión:")
ggplotly(grafico2)

# Visualización 3: Análisis Cuantil-Cuantil

# Calcular cuantiles para comparación
cuantiles_teoricos <- qunif(ppoints(cantidad))
datos_qq <- data.frame(
  teorico = cuantiles_teoricos,
  observado = sort(numeros_aleatorios)
)

grafico3 <- ggplot(datos_qq, aes(x = teorico, y = observado)) +
  geom_point(color = "forestgreen", alpha = 0.6, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "orange", 
              linetype = "dashed", size = 1.2) +
  labs(
    title = "Análisis QQ contra distribución uniforme",
    x = "Cuantiles teóricos",
    y = "Cuantiles de la muestra"
  ) +
  theme_light()

print("QQ-Plot:")
ggplotly(grafico3)


# Visualización 4: Análisis de correlaciones múltiples

# Construir matriz de datos tridimensional
matriz_datos <- data.frame(
  V1 = numeros_aleatorios[1:(cantidad-2)],
  V2 = numeros_aleatorios[2:(cantidad-1)],
  V3 = numeros_aleatorios[3:cantidad]
)

# Seleccionar muestra representativa para el análisis
muestra_indices <- sample(1:nrow(matriz_datos), 800)
muestra_analisis <- matriz_datos[muestra_indices, ]

print("Análisis multidimensional:")
ggpairs(
  muestra_analisis,
  title = "Correlaciones entre secuencias consecutivas (Mersenne Twister)"
)

# Calcular estadísticas básicas
media_muestra <- mean(numeros_aleatorios)
desviacion_std <- sd(numeros_aleatorios)
valor_min <- min(numeros_aleatorios)
valor_max <- max(numeros_aleatorios)

# Test de uniformidad
resultado_ks <- ks.test(numeros_aleatorios, "punif")

# Correlación serial
correlacion <- cor(numeros_aleatorios[-cantidad], numeros_aleatorios[-1])

# Mostrar resultados principales
print(paste("Media observada:", round(media_muestra, 5)))
print(paste("Desviación estándar:", round(desviacion_std, 5)))
print(paste("Test KS p-valor:", round(resultado_ks$p.value, 5)))
print(paste("Correlación serial:", round(correlacion, 5)))
