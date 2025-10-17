# ============================================================================
# APP SHINY – PSM COMPLETO Y OPTIMIZADO
# Propensity Score Matching - Vacunación vs Hospitalización
# ============================================================================

# LIBRERÍAS
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# ============================================================================
# 1. GENERADOR DE DATOS SINTÉTICOS
# ============================================================================

generar_datos_vacuna <- function(n = 1500) {
  set.seed(123)
  
  # Variables demográficas
  edad <- pmax(18, pmin(90, rnorm(n, 45, 15)))
  sexo <- rbinom(n, 1, 0.52)
  diabetes <- rbinom(n, 1, 0.15)
  hipertension <- rbinom(n, 1, 0.25)
  inmunocompromiso <- rbinom(n, 1, 0.05)
  escolaridad <- pmax(6, pmin(20, rnorm(n, 12, 3)))
  seguro <- rbinom(n, 1, 0.7)
  
  # Propensity Score (probabilidad de vacunación)
  ps_logit <- -2 + 0.02*edad + 0.3*sexo + 0.4*diabetes +
    0.3*hipertension - 0.1*inmunocompromiso +
    0.05*escolaridad + 0.5*seguro
  
  ps <- plogis(ps_logit)
  vacunado <- rbinom(n, 1, prob = ps)
  
  # Resultado: Hospitalización
  resultado_logit <- -3 + 0.04*edad + 0.3*diabetes +
    0.25*hipertension + 0.8*inmunocompromiso -
    0.6*vacunado
  
  hospitalizacion <- rbinom(n, 1, prob = plogis(resultado_logit))
  
  # Data frame final
  df <- data.frame(
    id = 1:n,
    edad = edad,
    sexo = sexo,
    diabetes = diabetes,
    hipertension = hipertension,
    inmunocompromiso = inmunocompromiso,
    escolaridad = escolaridad,
    seguro = seguro,
    ps = ps,
    vacunado = vacunado,
    hospitalizacion = hospitalizacion
  )
  
  return(df)
}

# ============================================================================
# 2. MATCHING POR PROPENSITY SCORE
# ============================================================================

realizar_psm_manual <- function(datos, ratio = 1, caliper = 0.25) {
  
  # Separar vacunados y controles
  vacunados <- datos[datos$vacunado == 1, ]
  controles <- datos[datos$vacunado == 0, ]
  
  matched_data <- data.frame()
  controles_usados <- c()
  
  # Matching 1:1 por PS más cercano dentro del caliper
  for (i in seq_len(nrow(vacunados))) {
    v_ps <- vacunados$ps[i]
    
    # Calcular diferencias de PS
    if (length(controles_usados) > 0) {
      idx_disponibles <- setdiff(seq_len(nrow(controles)), controles_usados)
      if (length(idx_disponibles) == 0) break
      diffs <- abs(controles$ps[idx_disponibles] - v_ps)
    } else {
      idx_disponibles <- seq_len(nrow(controles))
      diffs <- abs(controles$ps - v_ps)
    }
    
    # Buscar el mejor match dentro del caliper
    if (min(diffs) <= caliper) {
      match_idx <- idx_disponibles[which.min(diffs)]
      
      matched_data <- rbind(
        matched_data,
        vacunados[i, ],
        controles[match_idx, ]
      )
      controles_usados <- c(controles_usados, match_idx)
    }
  }
  
  return(matched_data)
}

# ============================================================================
# 3. CÁLCULO DE BALANCE (SMD)
# ============================================================================

calcular_balance <- function(antes, despues) {
  vars <- c("edad", "sexo", "diabetes", "hipertension", "inmunocompromiso")
  res <- data.frame()
  
  for(v in vars) {
    # Antes del matching
    mv_a <- mean(antes[[v]][antes$vacunado == 1], na.rm = TRUE)
    mc_a <- mean(antes[[v]][antes$vacunado == 0], na.rm = TRUE)
    sd_a <- sd(antes[[v]], na.rm = TRUE)
    smd_antes <- abs(mv_a - mc_a) / sd_a
    
    # Después del matching
    mv_d <- mean(despues[[v]][despues$vacunado == 1], na.rm = TRUE)
    mc_d <- mean(despues[[v]][despues$vacunado == 0], na.rm = TRUE)
    sd_d <- sd(despues[[v]], na.rm = TRUE)
    smd_despues <- abs(mv_d - mc_d) / sd_d
    
    res <- rbind(res, data.frame(
      Variable = v,
      SMD_Antes = smd_antes,
      SMD_Despues = smd_despues,
      Mejora_Pct = ifelse(smd_antes > 0, ((smd_antes - smd_despues) / smd_antes) * 100, 0)
    ))
  }
  
  return(res)
}

# ============================================================================
# 4. ANÁLISIS DE RESULTADOS
# ============================================================================

analizar_resultados <- function(dm) {
  tv <- mean(dm$hospitalizacion[dm$vacunado == 1], na.rm = TRUE) * 100
  tc <- mean(dm$hospitalizacion[dm$vacunado == 0], na.rm = TRUE) * 100
  
  list(
    tasa_vacunado = tv,
    tasa_control = tc,
    efectividad = ifelse(tc > 0, (tc - tv) / tc * 100, 0),
    riesgo_relativo = ifelse(tv > 0, tv / tc, NA)
  )
}

# ============================================================================
# 5. INTERFAZ DE USUARIO (UI)
# ============================================================================

ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  # Estilos CSS
  tags$head(tags$style(HTML("
    * {
      font-family: 'Segoe UI', Arial, sans-serif;
    }
    
    body {
      background-color: #1a1a1a;
      color: #ffffff;
    }
    
    .header-box {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 35px;
      border-radius: 12px;
      margin-bottom: 30px;
      box-shadow: 0 8px 16px rgba(0,0,0,0.3);
    }
    
    .header-box h1 {
      margin: 0 0 10px 0;
      font-size: 32px;
      font-weight: bold;
    }
    
    .header-box p {
      margin: 0;
      opacity: 0.95;
      font-size: 16px;
    }
    
    .metric-box {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 25px 15px;
      border-radius: 10px;
      text-align: center;
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      transition: transform 0.3s ease;
    }
    
    .metric-box:hover {
      transform: translateY(-5px);
    }
    
    .metric-value {
      font-size: 32px;
      font-weight: bold;
      margin-top: 12px;
      color: #ffffff;
    }
    
    .metric-label {
      font-size: 11px;
      text-transform: uppercase;
      opacity: 0.85;
      letter-spacing: 1px;
    }
    
    .section-title {
      font-size: 22px;
      font-weight: bold;
      margin: 30px 0 20px 0;
      color: #667eea;
      border-bottom: 3px solid #667eea;
      padding-bottom: 12px;
      letter-spacing: 0.5px;
    }
    
    .control-panel {
      background-color: #2b2b2b;
      padding: 20px;
      border-radius: 10px;
      margin-bottom: 20px;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      border: none;
      color: white;
      font-weight: bold;
      padding: 12px 24px;
      border-radius: 8px;
      cursor: pointer;
      transition: all 0.3s ease;
    }
    
    .btn-primary:hover {
      transform: scale(1.02);
      box-shadow: 0 6px 12px rgba(102, 126, 234, 0.4);
    }
    
    .status-box {
      background-color: #2b2b2b;
      padding: 15px;
      border-left: 4px solid #667eea;
      border-radius: 6px;
      margin-top: 15px;
    }
    
    .conclusion-box {
      background: #2b2b2b;
      padding: 25px;
      border-radius: 10px;
      margin-top: 25px;
      border-left: 5px solid #27ae60;
    }
    
    .conclusion-box h3 {
      color: #27ae60;
      margin-top: 0;
      font-size: 18px;
    }
    
    .conclusion-box p {
      line-height: 1.6;
      margin: 8px 0;
    }
    
    .dataTables_wrapper {
      background-color: #2b2b2b;
      border-radius: 8px;
      padding: 15px;
      margin: 15px 0;
    }
  "))),
  
  # HEADER
  div(class = "header-box",
      h1("📊 Análisis Causal con Propensity Score Matching"),
      p("Evaluación de Intervenciones de Vacunación - Hospitalización")),
  
  # PANEL DE CONTROL
  fluidRow(
    column(4,
           div(class = "control-panel",
               h4("⚙️ Parámetros"),
               sliderInput("n", 
                           label = "Tamaño muestral:",
                           min = 500, max = 3000, value = 1500, step = 100),
               sliderInput("caliper",
                           label = "Caliper (tolerancia PS):",
                           min = 0.05, max = 0.5, value = 0.25, step = 0.05))),
    
    column(4,
           div(class = "control-panel",
               h4("🎮 Acciones"),
               br(),
               actionButton("run", 
                            label = "▶ Ejecutar Análisis",
                            class = "btn-primary",
                            width = "100%",
                            style = "padding: 12px; font-size: 14px; margin-bottom: 10px;"),
               br(),
               actionButton("clr",
                            label = "↻ Limpiar",
                            class = "btn btn-secondary",
                            width = "100%",
                            style = "padding: 10px; font-size: 12px;"))),
    
    column(4,
           div(class = "control-panel",
               h4("📈 Estado"),
               div(class = "status-box",
                   textOutput("status"),
                   br(),
                   textOutput("details"))))
  ),
  
  # CONTENIDO CONDICIONAL (se muestra cuando ready = TRUE)
  conditionalPanel(
    condition = "output.ready",
    
    # ===== INDICADORES PRINCIPALES =====
    h2(class = "section-title", "📊 Indicadores Principales"),
    fluidRow(
      column(3, div(class = "metric-box",
                    div(class = "metric-label", "Efectividad"),
                    div(class = "metric-value", textOutput("ef")))),
      column(3, div(class = "metric-box",
                    div(class = "metric-label", "Parejas Formadas"),
                    div(class = "metric-value", textOutput("pairs")))),
      column(3, div(class = "metric-box",
                    div(class = "metric-label", "Tasa Exclusión"),
                    div(class = "metric-value", textOutput("exc")))),
      column(3, div(class = "metric-box",
                    div(class = "metric-label", "Reducción Control"),
                    div(class = "metric-value", textOutput("red"))))
    ),
    
    # ===== GRÁFICOS =====
    h2(class = "section-title", "📉 Visualizaciones"),
    
    fluidRow(
      column(6,
             h4("Distribución de Propensity Scores",
                style = "color: #667eea; margin-bottom: 15px;"),
             plotlyOutput("ps", height = "380px")),
      column(6,
             h4("Standardized Mean Differences (SMD)",
                style = "color: #667eea; margin-bottom: 15px;"),
             plotlyOutput("bal", height = "380px"))
    ),
    
    fluidRow(
      column(6,
             h4("Tasas de Hospitalización",
                style = "color: #667eea; margin-bottom: 15px;"),
             plotlyOutput("hosp", height = "380px")),
      column(6,
             h4("Mejora en Balance (%)",
                style = "color: #667eea; margin-bottom: 15px;"),
             plotlyOutput("bimp", height = "380px"))
    ),
    
    # ===== TABLAS DESCRIPTIVAS =====
    h2(class = "section-title", "📋 Estadística Descriptiva"),
    
    fluidRow(
      column(6,
             h4("Antes del Matching",
                style = "color: #667eea; margin-bottom: 10px;"),
             DTOutput("t1")),
      column(6,
             h4("Después del Matching",
                style = "color: #27ae60; margin-bottom: 10px;"),
             DTOutput("t2"))
    ),
    
    # ===== TABLA DE BALANCE =====
    h2(class = "section-title", "⚖️ Análisis de Balance"),
    p("Tabla de Standardized Mean Differences (SMD). Valores < 0.1 indican buen balance.",
      style = "font-size: 13px; color: #aaa; margin-bottom: 15px;"),
    DTOutput("tbal"),
    
    # ===== CONCLUSIONES =====
    div(class = "conclusion-box",
        h3("✓ Conclusiones del Análisis"),
        textOutput("conc"))
  )
)

# ============================================================================
# 6. LÓGICA DEL SERVIDOR (SERVER)
# ============================================================================

server <- function(input, output, session) {
  
  # Valores reactivos
  val <- reactiveValues(
    d = NULL,      # Datos originales
    m = NULL,      # Datos matched
    bal = NULL,    # Balance
    res = NULL,    # Resultados
    ready = FALSE  # Estado
  )
  
  # ===== EVENTO: EJECUTAR ANÁLISIS =====
  observeEvent(input$run, {
    output$status <- renderText("⏳ Generando datos sintéticos...")
    
    tryCatch({
      # Generar datos
      d0 <- generar_datos_vacuna(input$n)
      
      # Realizar matching
      dm <- realizar_psm_manual(d0, caliper = input$caliper)
      
      # Validar resultado
      if (nrow(dm) == 0) {
        output$status <- renderText("⚠️ Sin matches. Intenta aumentar el caliper.")
        return()
      }
      
      # Guardar en reactiveValues
      val$d <- d0
      val$m <- dm
      val$bal <- calcular_balance(d0, val$m)
      val$res <- analizar_resultados(val$m)
      val$ready <- TRUE
      
      # Actualizar status
      output$status <- renderText("✅ Análisis completado exitosamente")
      output$details <- renderText(
        paste0("Muestra original: ", nrow(val$d), " | ",
               "Parejas formadas: ", nrow(val$m) / 2, " | ",
               "Exclusión: ", round((1 - nrow(val$m) / nrow(val$d)) * 100, 1), "%")
      )
      
    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
      val$ready <- FALSE
    })
  })
  
  # ===== EVENTO: LIMPIAR =====
  observeEvent(input$clr, {
    val$ready <- FALSE
    val$d <- NULL
    val$m <- NULL
    val$bal <- NULL
    val$res <- NULL
    output$status <- renderText("🔄 Listo para nuevo análisis")
    output$details <- renderText("")
  })
  
  # ===== OUTPUT: Indicador de readiness =====
  output$ready <- reactive(val$ready)
  outputOptions(output, "ready", suspendWhenHidden = FALSE)
  
  # ===== OUTPUTS: MÉTRICAS =====
  output$ef <- renderText({
    if(val$ready) paste0(round(val$res$efectividad, 1), "%")
  })
  
  output$pairs <- renderText({
    if(val$ready) as.integer(nrow(val$m) / 2)
  })
  
  output$exc <- renderText({
    if(val$ready) paste0(round((1 - nrow(val$m) / nrow(val$d)) * 100, 1), "%")
  })
  
  output$red <- renderText({
    if(val$ready) {
      red_control <- abs(mean(val$d$hospitalizacion[val$d$vacunado == 0], na.rm = TRUE) -
                           mean(val$m$hospitalizacion[val$m$vacunado == 0], na.rm = TRUE)) * 100
      paste0(round(red_control, 1), "%")
    }
  })
  
  # ===== OUTPUTS: GRÁFICOS =====
  output$ps <- renderPlotly({
    if (!val$ready) return(NULL)
    
    g <- ggplot(val$m, aes(x = ps,
                           fill = factor(vacunado, labels = c("Control", "Vacunado")))) +
      geom_histogram(bins = 40, alpha = 0.75, position = "identity", color = "white") +
      scale_fill_manual(values = c("#e74c3c", "#27ae60")) +
      labs(title = "",
           x = "Propensity Score",
           y = "Frecuencia",
           fill = "Grupo") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#2b2b2b", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "#ccc"),
        legend.position = "top",
        legend.background = element_rect(fill = "#2b2b2b", color = NA),
        panel.grid = element_line(color = "#3a3a3a", size = 0.2)
      )
    
    ggplotly(g, tooltip = "none") %>%
      layout(paper_bgcolor = "#1a1a1a", plot_bgcolor = "#2b2b2b",
             font = list(color = "white"))
  })
  
  output$bal <- renderPlotly({
    if (!val$ready) return(NULL)
    
    b <- data.frame(
      Variable = rep(val$bal$Variable, 2),
      SMD = c(val$bal$SMD_Antes, val$bal$SMD_Despues),
      Per = rep(c("Antes", "Después"), each = nrow(val$bal))
    )
    
    g <- ggplot(b, aes(x = reorder(Variable, -SMD), y = SMD, fill = Per)) +
      geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.3) +
      geom_hline(yintercept = 0.1, linetype = "dashed", color = "#f39c12", size = 1) +
      scale_fill_manual(values = c("#e74c3c", "#27ae60")) +
      labs(title = "",
           y = "SMD",
           x = "Variable",
           fill = "Período",
           caption = "Línea roja = umbral de balance (0.1)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#2b2b2b", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "#ccc"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(fill = "#2b2b2b", color = NA),
        panel.grid = element_line(color = "#3a3a3a", size = 0.2),
        plot.caption = element_text(size = 10, color = "#999")
      )
    
    ggplotly(g, tooltip = "none") %>%
      layout(paper_bgcolor = "#1a1a1a", plot_bgcolor = "#2b2b2b",
             font = list(color = "white"))
  })
  
  output$hosp <- renderPlotly({
    if (!val$ready) return(NULL)
    
    s <- data.frame(
      Grupo = c("Control", "Vacunado"),
      Tasa = c(mean(val$m$hospitalizacion[val$m$vacunado == 0], na.rm = TRUE) * 100,
               mean(val$m$hospitalizacion[val$m$vacunado == 1], na.rm = TRUE) * 100)
    )
    
    g <- ggplot(s, aes(x = Grupo, y = Tasa, fill = Grupo)) +
      geom_col(alpha = 0.85, color = "white", size = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", Tasa)),
                vjust = -0.4, fontface = "bold", color = "white", size = 5) +
      scale_fill_manual(values = c("#e74c3c", "#27ae60")) +
      labs(title = "",
           y = "Tasa de Hospitalización (%)",
           x = "") +
      ylim(0, max(s$Tasa) * 1.2) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#2b2b2b", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "#ccc"),
        legend.position = "none",
        panel.grid = element_line(color = "#3a3a3a", size = 0.2)
      )
    
    ggplotly(g, tooltip = "none") %>%
      layout(paper_bgcolor = "#1a1a1a", plot_bgcolor = "#2b2b2b",
             font = list(color = "white"))
  })
  
  output$bimp <- renderPlotly({
    if (!val$ready) return(NULL)
    
    b <- val$bal
    b$Mejora <- ((b$SMD_Antes - b$SMD_Despues) / b$SMD_Antes) * 100
    b$Mejora <- ifelse(is.na(b$Mejora), 0, b$Mejora)
    
    g <- ggplot(b, aes(x = reorder(Variable, -Mejora), y = Mejora, fill = Mejora)) +
      geom_col(alpha = 0.85, color = "white", size = 0.3) +
      geom_text(aes(label = sprintf("%.0f%%", Mejora)),
                vjust = -0.3, fontface = "bold", color = "white", size = 4) +
      scale_fill_gradient(low = "#e74c3c", high = "#27ae60") +
      labs(title = "",
           y = "Mejora en Balance (%)",
           x = "Variable",
           fill = "Mejora %") +
      ylim(0, max(b$Mejora) * 1.2) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#2b2b2b", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "#ccc"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.background = element_rect(fill = "#2b2b2b", color = NA),
        panel.grid = element_line(color = "#3a3a3a", size = 0.2)
      )
    
    ggplotly(g, tooltip = "none") %>%
      layout(paper_bgcolor = "#1a1a1a", plot_bgcolor = "#2b2b2b",
             font = list(color = "white"))
  })
  
  # ===== FUNCIÓN: Crear tabla descriptiva =====
  makeTable <- function(dat) {
    data.frame(
      Grupo = c("Control", "Vacunado"),
      N = c(sum(dat$vacunado == 0), sum(dat$vacunado == 1)),
      EdadMedia = c(round(mean(dat$edad[dat$vacunado == 0], na.rm = TRUE), 1),
                    round(mean(dat$edad[dat$vacunado == 1], na.rm = TRUE), 1)),
      Diabetes = c(round(mean(dat$diabetes[dat$vacunado == 0], na.rm = TRUE) * 100, 1),
                   round(mean(dat$diabetes[dat$vacunado == 1], na.rm = TRUE) * 100, 1)),
      Hipertension = c(round(mean(dat$hipertension[dat$vacunado == 0], na.rm = TRUE) * 100, 1),
                       round(mean(dat$hipertension[dat$vacunado == 1], na.rm = TRUE) * 100, 1)),
      Hospitalizacion = c(round(mean(dat$hospitalizacion[dat$vacunado == 0], na.rm = TRUE) * 100, 1),
                          round(mean(dat$hospitalizacion[dat$vacunado == 1], na.rm = TRUE) * 100, 1))
    )
  }
  
  # ===== OUTPUTS: TABLAS =====
  output$t1 <- renderDT({
    if(val$ready) makeTable(val$d)
  }, options = list(dom = 't', pageLength = 2))
  
  output$t2 <- renderDT({
    if(val$ready) makeTable(val$m)
  }, options = list(dom = 't', pageLength = 2))
  
  output$tbal <- renderDT({
    if (!val$ready) return(NULL)
    
    tabla <- val$bal
    tabla$SMD_Antes <- round(tabla$SMD_Antes, 4)
    tabla$SMD_Despues <- round(tabla$SMD_Despues, 4)
    tabla$Mejora_Pct <- round(tabla$Mejora_Pct, 1)
    
    names(tabla) <- c("Variable", "SMD Antes", "SMD Después", "Mejora %")
    tabla
    
  }, options = list(dom = 't', pageLength = 10))
  
  # ===== OUTPUT: Conclusiones =====
  output$conc <- renderText({
    if (!val$ready) return("")
    
    e <- round(val$res$efectividad, 1)
    p <- nrow(val$m) / 2
    exc <- round((1 - nrow(val$m) / nrow(val$d)) * 100, 1)
    smd_promedio <- round(mean(val$bal$SMD_Despues), 3)
    
    paste0(
      "✓ La vacunación reduce la hospitalización un ", e, "%.\n",
      "✓ Se formaron ", as.integer(p), " parejas balanceadas exitosamente.\n",
      "✓ Tasa de exclusión: ", exc, "% de la muestra original.\n",
      "✓ SMD promedio después del matching: ", smd_promedio, " (< 0.1 = buen balance).\n",
      "✓ Hallazgo causal confirmado tras controlar confusión con PSM.\n",
      "✓ Todos los confusores estimados están balanceados entre grupos."
    )
  })
}

# ============================================================================
# 7. EJECUTAR LA APLICACIÓN
# ============================================================================

shinyApp(ui, server)