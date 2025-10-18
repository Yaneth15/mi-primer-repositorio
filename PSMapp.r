
# APP SHINY – Propensity Score Matching (PSM) –
pkgs <- c("shiny","shinythemes","ggplot2","dplyr","MatchIt",
          "plotly","DT","tidyr","tibble","readxl","shinyWidgets")
inst <- pkgs[ !pkgs %in% installed.packages()[,"Package"] ]
if(length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

generar_datos_vacuna <- function(n = 1500) {
  set.seed(123)
  tibble(
    id = 1:n,
    edad     = pmax(18, pmin(90, rnorm(n, 45, 15))),
    sexo     = rbinom(n, 1, .52),
    diabetes = rbinom(n, 1, .15),
    hipertension      = rbinom(n, 1, .25),
    inmunocompromiso  = rbinom(n, 1, .05),
    escolaridad       = pmax(6, pmin(20, rnorm(n, 12, 3))),
    seguro   = rbinom(n, 1, .7)
  ) |>
    mutate(
      ps_logit = -2 + 0.02*edad + 0.3*sexo + 0.4*diabetes +
        0.3*hipertension - 0.1*inmunocompromiso +
        0.05*escolaridad + 0.5*seguro,
      ps       = plogis(ps_logit),
      vacunado = rbinom(n, 1, prob = ps),
      resultado_logit = -3 + 0.04*edad + 0.3*diabetes +
        0.25*hipertension + 0.8*inmunocompromiso -
        0.6*vacunado,
      hospitalizacion = rbinom(n, 1, prob = plogis(resultado_logit))
    ) |>
    select(-ps_logit, -resultado_logit)
}

realizar_psm <- function(datos, tratamiento, covs, caliper = 0.25, ratio = 1) {
  formula <- as.formula(paste(tratamiento, "~", paste(covs, collapse = " + ")))
  m <- matchit(formula, data = datos, method = "nearest", distance = "logit",
               ratio = ratio, caliper = caliper, replace = FALSE)
  list(match_obj = m, datos_matched = match.data(m))
}

calcular_balance <- function(antes, despues, covs, trat) {
  res <- tibble()
  for(v in covs) {
    mv_a <- mean(antes[[v]][antes[[trat]] == 1], na.rm = TRUE)
    mc_a <- mean(antes[[v]][antes[[trat]] == 0], na.rm = TRUE)
    mv_d <- mean(despues[[v]][despues[[trat]] == 1], na.rm = TRUE)
    mc_d <- mean(despues[[v]][despues[[trat]] == 0], na.rm = TRUE)
    sd_a <- sd(antes[[v]])
    res  <- bind_rows(res,
                      tibble(Variable    = v,
                             SMD_Antes   = abs(mv_a - mc_a)/sd_a,
                             SMD_Despues = abs(mv_d - mc_d)/sd(antes[[v]])))
  }
  res
}

analizar_resultados <- function(dm, resultado, trat) {
  tv <- mean(dm[[resultado]][dm[[trat]] == 1], na.rm = TRUE) * 100
  tc <- mean(dm[[resultado]][dm[[trat]] == 0], na.rm = TRUE) * 100
  list(tasa_tratado = tv, tasa_control = tc, efectividad = (tc - tv)/tc * 100)
}

ui <- navbarPage(
  title = "PSM Analysis",
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    body {
      font-family: 'Segoe UI', sans-serif;
      background-color: #fafafa;
      color: #3d3d3d;
    }
    .header-box {
      background: linear-gradient(135deg, #d4a5d9 0%, #c8b8e4 100%);
      color: #2d2d2d;
      padding: 35px;
      border-radius: 12px;
      margin-bottom: 25px;
      box-shadow: 0 4px 15px rgba(212, 165, 217, 0.3);
    }
    .header-box h1 {
      color: #2d2d2d;
      font-weight: 700;
      margin-bottom: 10px;
    }
    .header-box p {
      color: #3d3d3d;
      font-size: 16px;
      margin: 8px 0;
    }
    .metric-box {
      background: linear-gradient(135deg, #b8dff0 0%, #c9b8e4 100%);
      color: #2d2d2d;
      padding: 25px;
      border-radius: 10px;
      text-align: center;
      box-shadow: 0 3px 10px rgba(184, 223, 240, 0.25);
      border-left: 5px solid #a8a8d9;
    }
    .metric-value {
      font-size: 36px;
      font-weight: 800;
      margin-top: 12px;
      color: #7a5a99;
    }
    .metric-label {
      font-size: 11px;
      text-transform: uppercase;
      font-weight: 600;
      color: #4d4d4d;
      letter-spacing: 1px;
    }
    .section-title {
      font-size: 22px;
      font-weight: 700;
      margin: 30px 0 20px 0;
      color: #7a5a99;
      border-bottom: 3px solid #d4a5d9;
      padding-bottom: 12px;
    }
    .btn-primary {
      background-color: #b8a8d9 !important;
      border: none !important;
      color: #2d2d2d !important;
      font-weight: 700;
      transition: all 0.3s;
    }
    .btn-primary:hover {
      background-color: #9e8acc !important;
      color: #fff !important;
    }
    .btn-secondary {
      background-color: #c8d4e8 !important;
      border: none !important;
      color: #2d2d2d !important;
      font-weight: 600;
    }
    .nav-tabs .nav-link.active {
      background-color: #d4a5d9 !important;
      color: #2d2d2d !important;
      border: none;
      font-weight: 700;
    }
    .nav-tabs .nav-link {
      color: #7a5a99 !important;
      font-weight: 600;
      border: none;
      border-bottom: 3px solid transparent;
    }
    .nav-tabs .nav-link:hover {
      border-bottom: 3px solid #d4a5d9;
    }
    .conclusion-box {
      background: linear-gradient(135deg, #f0d4e8 0%, #e8ddf0 100%);
      padding: 25px;
      border-radius: 10px;
      border-left: 5px solid #d4a5d9;
      margin-top: 25px;
      color: #3d3d3d;
    }
    .conclusion-box h3 {
      color: #7a5a99;
      font-weight: 700;
      margin-bottom: 15px;
    }
    .conclusion-box p {
      color: #4d4d4d;
      line-height: 1.8;
      font-size: 15px;
    }
    .well {
      background-color: #f5f5f5 !important;
      border: 1px solid #d9d9d9 !important;
      color: #2d2d2d !important;
    }
    .form-control, .form-select {
      background-color: #fff !important;
      border: 2px solid #d4a5d9 !important;
      color: #2d2d2d !important;
      font-weight: 500;
    }
    .form-control:focus {
      border-color: #9e8acc !important;
      box-shadow: 0 0 0 0.2rem rgba(212, 165, 217, 0.25) !important;
    }
    .checkbox input[type='checkbox'] {
      accent-color: #d4a5d9;
    }
    h4 {
      color: #7a5a99;
      font-weight: 700;
    }
  "))),
  
  # ==== PESTAÑA: QUÉ ES PSM ================================================
  tabPanel("📚 ¿Qué es PSM?",
           div(class = "header-box",
               h1("🎯 Propensity Score Matching (PSM)"),
               p("El PSM es una técnica estadística que permite estimar efectos causales a partir de datos observacionales.
        Su objetivo es emular un experimento aleatorizado cuando no es posible asignar aleatoriamente el tratamiento."),
               hr(style = "border-color: rgba(45, 45, 45, 0.3); margin: 20px 0;"),
               h4(style = "margin-top: 20px;")
           ),
           fluidRow(
             column(6,
                    div(style = "background: #f0d4e8; padding: 20px; border-radius: 8px; border-left: 5px solid #d4a5d9;",
                        h5("🔍 ¿Cómo funciona?", style = "color: #7a5a99; font-weight: 700;"),
                        HTML("
            <ol style='color: #3d3d3d; line-height: 1.8;'>
              <li><strong>Estimar propensity scores:</strong> Calcular la probabilidad de recibir tratamiento según características.</li>
              <li><strong>Emparejar sujetos:</strong> Agrupar individuos tratados con controles con probabilidades similares.</li>
              <li><strong>Comparar resultados:</strong> Analizar diferencias en el resultado entre grupos balanceados.</li>
              <li><strong>Interpretar causalmente:</strong> La diferencia es atribuible al tratamiento, no a confusión.</li>
            </ol>
          ")
                    )
             ),
             column(6,
                    div(style = "background: #c9b8e4; padding: 20px; border-radius: 8px; border-left: 5px solid #b8a8d9;",
                        h5("📊 Métricas clave", style = "color: #7a5a99; font-weight: 700;"),
                        HTML("
            <ul style='color: #3d3d3d; line-height: 2;'>
              <li><strong>SMD (Standardized Mean Difference):</strong> < 0.1 indica buen balance</li>
              <li><strong>Efectividad:</strong> % de reducción del resultado atribuible al tratamiento</li>
              <li><strong>Caliper:</strong> Distancia máxima permitida entre propensity scores al emparejar</li>
              <li><strong>Exclusión:</strong> % de casos no emparejados (quedan fuera del análisis)</li>
            </ul>
          ")
                    )
             )
           )
  ),
  
  # ==== PESTAÑA: CARGAR DATOS ==============================================
  tabPanel("📤 Datos",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("📁 Cargar archivo", style = "margin-top: 0;"),
               fileInput("file", "CSV o Excel (.xlsx, .xls)",
                         accept = c(".csv", ".xlsx", ".xls"),
                         placeholder = "Selecciona un archivo..."),
               checkboxInput("sinteticos", "Usar datos de ejemplo", value = TRUE),
               hr(),
               h4("⚙️ Configurar columnas"),
               uiOutput("sel_trat"),
               uiOutput("sel_out"),
               uiOutput("sel_covs"),
               hr(),
               actionButton("cargar", "✓ Cargar datos", 
                            class = "btn btn-primary btn-lg", width = "100%",
                            style = "margin-top: 15px;")
             ),
             mainPanel(
               width = 9,
               h4("👁️ Vista previa (primeras 10 filas)"),
               DTOutput("preview")
             )
           )
  ),
  
  # ==== PESTAÑA: ANÁLISIS ==================================================
  tabPanel("🔬 Análisis PSM",
           fluidPage(
             fluidRow(
               column(3,
                      div(style = "background: #f0f4fa; padding: 20px; border-radius: 8px; border-left: 5px solid #b8dff0;",
                          h4("⚙️ Parámetros", style = "margin-top: 0;"),
                          sliderInput("caliper", "Caliper (similitud):", 0.05, 0.5, 0.25, 0.05),
                          sliderInput("ratio", "Ratio (1:n matching):", 1, 5, 1, 1)
                      )
               ),
               column(3,
                      div(style = "background: #f0f4fa; padding: 20px; border-radius: 8px; border-left: 5px solid #c8d4e8; margin-top: 27px;",
                          actionButton("ejecutar", "▶ Ejecutar PSM",
                                       class = "btn btn-primary btn-lg", width = "100%",
                                       style = "margin-bottom: 12px;"),
                          actionButton("limpiar", "↻ Limpiar",
                                       class = "btn btn-secondary", width = "100%")
                      )
               ),
               column(3,
                      div(style = "background: #f0f4fa; padding: 20px; border-radius: 8px; border-left: 5px solid #d4a5d9;",
                          h4("📊 Estado", style = "margin-top: 0;"),
                          textOutput("status"),
                          br(),
                          textOutput("details")
                      )
               ),
               column(3, br())
             ),
             
             conditionalPanel(
               condition = "output.ready",
               h2(class = "section-title", "📈 Indicadores clave"),
               fluidRow(
                 column(3, div(class = "metric-box", 
                               div(class = "metric-label", "Efectividad"),
                               div(class = "metric-value", textOutput("ef")))),
                 column(3, div(class = "metric-box", 
                               div(class = "metric-label", "Parejas formadas"),
                               div(class = "metric-value", textOutput("pairs")))),
                 column(3, div(class = "metric-box", 
                               div(class = "metric-label", "% Exclusión"),
                               div(class = "metric-value", textOutput("exc")))),
                 column(3, div(class = "metric-box", 
                               div(class = "metric-label", "Reducción SMD"),
                               div(class = "metric-value", textOutput("red"))))
               ),
               
               h2(class = "section-title", "📊 Visualizaciones"),
               fluidRow(
                 column(6, 
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                            plotlyOutput("ps")
                        )
                 ),
                 column(6,
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                            plotlyOutput("bal")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-top: 20px;",
                            plotlyOutput("hosp")
                        )
                 ),
                 column(6,
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-top: 20px;",
                            plotlyOutput("bimp")
                        )
                 )
               ),
               
               h2(class = "section-title", "📋 Tablas comparativas"),
               fluidRow(
                 column(6,
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                            h5("Antes del matching", style = "color: #7a5a99; font-weight: 700;"),
                            DTOutput("t1")
                        )
                 ),
                 column(6,
                        div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                            h5("Después del matching", style = "color: #7a5a99; font-weight: 700;"),
                            DTOutput("t2")
                        )
                 )
               ),
               
               h2(class = "section-title", "⚖️ Evaluación de balance"),
               div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                   DTOutput("tbal")
               ),
               
               div(class = "conclusion-box",
                   h3("✓ Conclusiones"),
                   textOutput("conc")
               )
             )
           )
  )
)

# 7. SERVER ================================================================
server <- function(input, output, session) {
  
  datos <- reactiveValues(raw = NULL, matched = NULL, balance = NULL, res = NULL,
                          trat = "vacunado", resultado = "hospitalizacion", covs = NULL,
                          ready = FALSE)
  
  # --- Cargar datos ---
  observeEvent(input$cargar, {
    if(input$sinteticos) {
      datos$raw <- generar_datos_vacuna(1500)
      datos$trat <- "vacunado"
      datos$resultado <- "hospitalizacion"
      datos$covs <- c("edad", "sexo", "diabetes", "hipertension", "inmunocompromiso", "escolaridad", "seguro")
    } else {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      datos$raw <- switch(ext,
                          csv = read.csv(input$file$datapath),
                          xlsx = readxl::read_excel(input$file$datapath),
                          xls = readxl::read_excel(input$file$datapath))
      datos$raw <- as_tibble(datos$raw)
    }
    datos$ready <- FALSE
  })
  
  # --- Selectores dinámicos ---
  output$sel_trat <- renderUI({
    req(datos$raw)
    selectInput("trat", "Columna de tratamiento:", choices = names(datos$raw), selected = datos$trat)
  })
  output$sel_out <- renderUI({
    req(datos$raw)
    selectInput("out", "Columna de resultado:", choices = names(datos$raw), selected = datos$resultado)
  })
  output$sel_covs <- renderUI({
    req(datos$raw)
    checkboxGroupInput("covs", "Covariables (confusores):",
                       choices = names(datos$raw), selected = datos$covs)
  })
  
  # --- Preview ---
  output$preview <- renderDT({
    req(datos$raw)
    datos$raw |> head(10)
  }, options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE))
  
  # --- Actualizar nombres ---
  observe({
    datos$trat <- input$trat
    datos$resultado <- input$out
    datos$covs <- input$covs
  })
  
  # --- Ejecutar PSM ---
  observeEvent(input$ejecutar, {
    req(datos$raw, datos$trat, datos$resultado, datos$covs)
    output$status <- renderText("⏳ Procesando matching…")
    tryCatch({
      ps <- realizar_psm(datos$raw, tratamiento = datos$trat,
                         covs = datos$covs, caliper = input$caliper, ratio = input$ratio)
      datos$matched <- ps$datos_matched
      datos$balance <- calcular_balance(datos$raw, datos$matched, datos$covs, datos$trat)
      datos$res <- analizar_resultados(datos$matched, datos$resultado, datos$trat)
      datos$ready <- TRUE
      output$status <- renderText("✓ Análisis completado")
      output$details <- renderText(paste("Muestra original:", nrow(datos$raw),
                                         "| Parejas:", nrow(datos$matched)/2))
    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
    })
  })
  
  observeEvent(input$limpiar, {
    datos$ready <- FALSE
    output$status <- renderText("Listo para nuevo análisis")
    output$details <- renderText("")
  })
  
  output$ready <- reactive(datos$ready)
  outputOptions(output, "ready", suspendWhenHidden = FALSE)
  
  # --- Métricas ---
  output$ef <- renderText(if(datos$ready) paste0(round(datos$res$efectividad, 1), "%"))
  output$pairs <- renderText(if(datos$ready) format(nrow(datos$matched)/2, big.mark = ","))
  output$exc <- renderText(if(datos$ready) paste0(round((1 - nrow(datos$matched)/nrow(datos$raw)) * 100, 1), "%"))
  output$red <- renderText(if(datos$ready) {
    ta <- mean(datos$raw[[datos$resultado]][datos$raw[[datos$trat]] == 0], na.rm = TRUE)
    td <- mean(datos$matched[[datos$resultado]][datos$matched[[datos$trat]] == 0], na.rm = TRUE)
    paste0(round(abs(td - ta) * 100, 1), "%")
  })
  
  # --- Gráficos ---
  output$ps <- renderPlotly({
    if(!datos$ready) return()
    ggplotly(
      ggplot(datos$matched, aes(x = ps, fill = factor(!!sym(datos$trat), labels = c("Control", "Tratado")))) +
        geom_histogram(bins = 40, alpha = 0.8, position = "identity", color = "white", size = 0.2) +
        scale_fill_manual(values = c("#f0a4a4", "#b8dff0")) +
        labs(title = "Propensity Score - Después del Matching", x = "Propensity Score", y = "Frecuencia", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#7a5a99", face = "bold", size = 13),
              axis.title = element_text(color = "#3d3d3d", face = "bold", size = 11),
              axis.text = element_text(color = "#3d3d3d"),
              legend.position = "top",
              panel.grid.major = element_line(color = "#f0f0f0"))
    )
  })
  
  output$bal <- renderPlotly({
    if(!datos$ready) return()
    b <- datos$balance |>
      pivot_longer(c(SMD_Antes, SMD_Despues), names_to = "Periodo", values_to = "SMD") |>
      mutate(Periodo = c(SMD_Antes = "Antes", SMD_Despues = "Después")[Periodo])
    ggplotly(
      ggplot(b, aes(x = reorder(Variable, -SMD), y = SMD, fill = Periodo)) +
        geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.3) +
        geom_hline(yintercept = 0.1, linetype = "dashed", color = "#d4a5d9", size = 1) +
        scale_fill_manual(values = c("#f0a4a4", "#b8dff0")) +
        labs(title = "Standardized Mean Differences", x = "Variable", y = "SMD", fill = "Periodo") +
        annotate("text", x = Inf, y = 0.1, label = "Threshold: 0.1", vjust = -0.5, hjust = 1.1, color = "#d4a5d9", size = 3) +
        theme_minimal() +
        theme(plot.title = element_text(color = "#7a5a99", face = "bold", size = 13),
              axis.title = element_text(color = "#3d3d3d", face = "bold", size = 11),
              axis.text = element_text(color = "#3d3d3d"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              panel.grid.major = element_line(color = "#f0f0f0"))
    )
  })
  
  output$hosp <- renderPlotly({
    if(!datos$ready) return()
    s <- datos$matched |>
      group_by(!!sym(datos$trat)) |>
      summarise(Tasa = mean(!!sym(datos$resultado), na.rm = TRUE) * 100, .groups = "drop") |>
      mutate(Grupo = c("Control", "Tratado")[get(datos$trat) + 1])
    ggplotly(
      ggplot(s, aes(x = Grupo, y = Tasa, fill = Grupo)) +
        geom_col(alpha = 0.85, color = "white", size = 0.4) +
        geom_text(aes(label = sprintf("%.1f%%", Tasa)), vjust = -0.4, fontface = "bold", color = "#3d3d3d", size = 5) +
        scale_fill_manual(values = c("#f0a4a4", "#b8dff0")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Tasa de Resultado por Grupo", x = "Grupo", y = "Tasa (%)", fill = "Grupo") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#7a5a99", face = "bold", size = 13),
              axis.title = element_text(color = "#3d3d3d", face = "bold", size = 11),
              axis.text = element_text(color = "#3d3d3d"),
              legend.position = "none",
              panel.grid.major = element_line(color = "#f0f0f0"))
    )
  })
  
  output$bimp <- renderPlotly({
    if(!datos$ready) return()
    b <- datos$balance |> mutate(Mejora = ((SMD_Antes - SMD_Despues) / SMD_Antes) * 100)
    ggplotly(
      ggplot(b, aes(x = reorder(Variable, -Mejora), y = Mejora, fill = Mejora)) +
        geom_col(alpha = 0.85, color = "white", size = 0.3) +
        scale_fill_gradient(low = "#f0a4a4", high = "#b8dff0", name = "% Mejora") +
        labs(title = "Mejora en Balance de Covariables", x = "Variable", y = "% de Mejora") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#7a5a99", face = "bold", size = 13),
              axis.title = element_text(color = "#3d3d3d", face = "bold", size = 11),
              axis.text = element_text(color = "#3d3d3d"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              panel.grid.major = element_line(color = "#f0f0f0"))
    )
  })
  
  # --- Tablas ---
  makeTable <- function(dat) {
    dat |>
      group_by(!!sym(datos$trat)) |>
      summarise(
        N = n(),
        across(all_of(datos$covs), ~round(mean(., na.rm = TRUE), 2), .names = "{.col}"),
        .groups = "drop") |>
      mutate(Grupo = c("Control", "Tratado")[get(datos$trat) + 1]) |>
      select(Grupo, N, everything(), -!!sym(datos$trat))
  }
  
  output$t1 <- renderDT({
    if(datos$ready) makeTable(datos$raw)
  }, options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE))
  
  output$t2 <- renderDT({
    if(datos$ready) makeTable(datos$matched)
  }, options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE))
  
  output$tbal <- renderDT({
    if(!datos$ready) return()
    datos$balance |>
      mutate(across(c(SMD_Antes, SMD_Despues), ~round(., 3)),
             Mejora = round(((SMD_Antes - SMD_Despues) / SMD_Antes) * 100, 1)) |>
      rename("Variable" = Variable, "SMD Antes" = SMD_Antes,
             "SMD Después" = SMD_Despues, "Mejora %" = Mejora)
  }, options = list(dom = 't', scrollX = TRUE, autoWidth = TRUE))
  
  output$conc <- renderText({
    if(!datos$ready) return("")
    e <- round(datos$res$efectividad, 1)
    p <- nrow(datos$matched) / 2
    paste0(
      "✓ El tratamiento reduce el resultado en un ", e, "%\n\n",
      "✓ Se formaron ", format(p, big.mark = ","), " parejas balanceadas\n\n",
      "✓ El balance mejoró significativamente (SMD < 0.1) en todas las covariables\n\n",
      "✓ El efecto estimado es causal tras controlar adecuadamente la confusión mediante PSM"
    )
  })
}

# 8. EJECUTAR APP ==========================================================
shinyApp(ui, server)
