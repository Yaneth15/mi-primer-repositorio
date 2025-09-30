
#  APLICACIÓN “Kolmógorov–Smirnov”

library(shiny)
library(shinythemes)  
library(boot)         

ui <- navbarPage(
  title = strong("Kolmógorov–Smirnov (K–S)"),
  theme = shinytheme("superhero"),
  id    = "nav",
  
  # ------------------------- 1. Introducción -----------------------
  tabPanel("Introducción",
           fluidRow(
             column(
               8,
               h2("¿Qué es la prueba de Kolmógorov–Smirnov?"),
               p("Es una prueba no paramétrica que compara la función de distribución ",
                 "empírica (FDE) de una muestra con una distribución teórica continua ",
                 "o con la FDE de otra muestra."),
               br(),
               h4("Hipótesis"),
               tags$ul(
                 tags$li(strong("Una muestra:"), "H₀: los datos provienen de la distribución teórica especificada."),
                 tags$li(strong("Dos muestras:"), "H₀: ambas muestras provienen de la misma distribución.")
               ),
               br(),
               h4("Estadístico D"),
               p("D = supₓ | Fₙ(x) – F(x) |   (una muestra)"),
               p("D = supₓ | F₁(x) – F₂(x) |   (dos muestras)")
             ),
             column(
               4,
               h4("Glosario rápido"),
               tableOutput("intro_table")
             )
           )),
  
  # ------------------------- 2. Simulación -------------------------
  tabPanel("Simulación",
           sidebarLayout(
             sidebarPanel(
               h4("Parámetros"),
               radioButtons("tipo", "Tipo de prueba:",
                            choices = c("Una muestra vs. teórica" = "1teor",
                                        "Dos muestras"            = "2muestras")),
               conditionalPanel(
                 "input.tipo == '1teor'",
                 selectInput("teorica", "Distribución teórica:",
                             choices = c("Normal"      = "norm",
                                         "Exponencial" = "exp",
                                         "Uniforme"    = "unif")),
                 conditionalPanel(
                   "input.teorica == 'norm'",
                   numericInput("mean", "Media μ:", 0, step = .1),
                   numericInput("sd",   "Desv. σ:", 1, min = .01, step = .1)
                 ),
                 conditionalPanel(
                   "input.teorica == 'exp'",
                   numericInput("rate", "Rate λ:", 1, min = .01, step = .1)
                 ),
                 conditionalPanel(
                   "input.teorica == 'unif'",
                   numericInput("min", "Mínimo:", 0, step = .1),
                   numericInput("max", "Máximo:", 1, step = .1)
                 )
               ),
               numericInput("n1", "Tamaño muestra 1:", 30, min = 5, max = 5000, step = 5),
               conditionalPanel(
                 "input.tipo == '2muestras'",
                 numericInput("n2", "Tamaño muestra 2:", 30, min = 5, max = 5000, step = 5)
               ),
               actionButton("sim", "Generar y probar", icon = icon("play"),
                            class = "btn-success"),
               br(), br(),
               h4("Resultado K–S"),
               verbatimTextOutput("res_ks")
             ),
             mainPanel(
               plotOutput("graf_ks", height = "400px")
             )
           )),
  
  # --------------------- 3. Tamaño muestral ------------------------
  tabPanel("Tamaño muestral",
           fluidRow(
             column(
               6,
               h4("¿Cuántos datos necesito?"),
               p("Para detectar una desviación mínima Dₘᵢₙ con potencia 1–β ",
                 "respecto a una distribución teórica (simulación Monte-Carlo)."),
               numericInput("dmin", "Dₘᵢₙ mínimo a detectar:", .15, min = .01, step = .01),
               numericInput("pot",  "Potencia (1–β) deseada:", .80, min = .50, max = .99, step = .05),
               numericInput("alfa", "Nivel α:", .05, min = .01, max = .20, step = .01),
               actionButton("calc_n", "Calcular n", icon = icon("calculator")),
               br(), br(),
               verbatimTextOutput("out_n")
             ),
             column(
               6,
               h4("Gráfico de curvas"),
               plotOutput("curvas_n", height = "350px")
             )
           )),
  
  # ----------------------- 4. Redacción ----------------------------
  tabPanel("Redacción",
           fluidRow(
             column(
               4,
               h4("Personaliza tu párrafo"),
               textInput("tema", "Tema del estudio:",
                         placeholder = "Ej: altura de estudiantes"),
               numericInput("n_redac", "n de tu muestra:", 100, min = 5),
               selectInput("dist_redac", "Distribución teórica:",
                           choices = c("Normal", "Exponencial", "Uniforme")),
               numericInput("D_redac", "Estadístico D obtenido:", .08, step = .01),
               numericInput("p_redac", "p-valor:", .21, step = .01),
               actionButton("gen_redac", "Generar texto", icon = icon("file-alt")),
               br(), br(),
               downloadButton("desc_redac", "Descargar .txt")
             ),
             column(
               8,
               h4("Texto generado"),
               verbatimTextOutput("parrafo")
             )
           )),
  
  # ----------------------- 5. Recursos -----------------------------
  tabPanel("Recursos",
           fluidRow(
             column(
               6,
               h4("Referencias"),
               tags$ul(
                 tags$li("Conover, W. J. (1999). Practical Nonparametric Statistics."),
                 tags$li("Higgins, J. (2004). Introduction to Modern Nonparametric Statistics."),
                 tags$li("Rizzo, M. (2019). Statistical Computing with R.")
               )
             ),
             column(
               6,
               h4("Paquetes de R útiles"),
               tags$ul(
                 tags$li(strong("stats"), "– ks.test()"),
                 tags$li(strong("dgof"),  "– ks.test() con simulaciones"),
                 tags$li(strong("Matching"), "– ks.boot() bootstrap")
               )
             )
           ))
)

# -------------------------------------------------------------------
#  SERVER
# -------------------------------------------------------------------
server <- function(input, output, session){
  
  # --------------------- tabla intro ---------------------------
  output$intro_table <- renderTable({
    data.frame(
      Término = c("Estadístico D", "p-valor", "Empírica Fₙ", "Teórica F"),
      Definición = c("Máxima discrepancia entre funciones",
                     "Probabilidad bajo H₀ de obtener un D igual o mayor",
                     "Proporción acumulada de la muestra",
                     "Función de distribución acumulada teórica")
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  # --------------------- simulación ----------------------------
  vals <- reactiveValues(x1 = NULL, x2 = NULL, res = NULL)
  
  observeEvent(input$sim, {
    n1 <- input$n1
    if(input$tipo == "1teor"){
      if(input$teorica == "norm") vals$x1 <- rnorm(n1, input$mean, input$sd)
      if(input$teorica == "exp")  vals$x1 <- rexp(n1, input$rate)
      if(input$teorica == "unif") vals$x1 <- runif(n1, input$min, input$max)
      tt <- ks.test(vals$x1, switch(input$teorica,
                                    norm = "pnorm", exp = "pexp", unif = "punif"),
                    m = switch(input$teorica,
                               norm = input$mean, exp = input$rate, unif = input$min),
                    s = switch(input$teorica,
                               norm = input$sd,  exp = NULL,        unif = input$max))
      vals$res <- tt
    } else {
      n2 <- input$n2
      vals$x1 <- rnorm(n1, 0, 1)   # muestra 1
      vals$x2 <- rnorm(n2, 0, 1)   # muestra 2 (puedes cambiarla)
      vals$res <- ks.test(vals$x1, vals$x2)
    }
  })
  
  output$res_ks <- renderPrint({
    req(vals$res)
    rr <- vals$res
    cat("Estadístico D =", round(rr$statistic, 4), "\n")
    cat("p-valor       =", signif(rr$p.value, 4), "\n")
    cat("Conclusión (α = 0.05): ",
        ifelse(rr$p.value < 0.05,
               "Rechazar H₀ – diferencias significativas.",
               "No rechazar H₀ – no hay evidencia de diferencias."), "\n")
  })
  
  output$graf_ks <- renderPlot({
    req(vals$res, vals$x1)
    if(input$tipo == "1teor"){
      x <- sort(vals$x1)
      n <- length(x)
      Femp <- (1:n)/n
      xx <- seq(min(x), max(x), length = 500)
      Fteo <- switch(input$teorica,
                     norm = pnorm(xx, input$mean, input$sd),
                     exp  = pexp(xx, input$rate),
                     unif = punif(xx, input$min, input$max))
      plot(xx, Fteo, type = "l", lwd = 2, col = "steelblue",
           ylab = "Probabilidad acumulada", xlab = "x",
           main = "FDE empírica vs. teórica")
      lines(x, Femp, col = "tomato", lwd = 2)
      legend("bottomright", legend = c("Teórica", "Empírica"),
             col = c("steelblue", "tomato"), lwd = 2)
    } else {
      plot(ecdf(vals$x1), col = "steelblue", lwd = 2,
           main = "FDE de dos muestras", xlab = "x", ylab = "Probabilidad")
      plot(ecdf(vals$x2), col = "tomato", lwd = 2, add = TRUE)
      legend("bottomright", legend = c("Muestra 1", "Muestra 2"),
             col = c("steelblue", "tomato"), lwd = 2)
    }
  })
  
  # --------------------- tamaño muestral -----------------------
  n_out <- reactiveVal()
  
  observeEvent(input$calc_n, {
    dmin <- input$dmin
    pot  <- input$pot
    alfa <- input$alfa
    
    # simulación rápida: normal vs normal con shift dmin
    sim_power <- function(n){
      mean(replicate(500, {
        x <- rnorm(n)
        ks.test(x, "pnorm", dmin, 1)$p.value < alfa
      }))
    }
    vec_n <- seq(20, 1000, by = 20)
    pow   <- sapply(vec_n, sim_power)
    n_req <- min(vec_n[pow >= pot])
    n_out(n_req)
  })
  
  output$out_n <- renderPrint({
    req(n_out())
    cat("Tamaño muestral estimado para detectar D ≥", input$dmin,
        "con potencia", input$pot, "≈", n_out(), "observaciones.\n")
  })
  
  output$curvas_n <- renderPlot({
    req(n_out())
    dmin <- input$dmin
    alfa <- input$alfa
    sim_power <- function(n){
      mean(replicate(300, {
        x <- rnorm(n)
        ks.test(x, "pnorm", dmin, 1)$p.value < alfa
      }))
    }
    vec_n <- seq(20, 500, by = 20)
    pow   <- sapply(vec_n, sim_power)
    plot(vec_n, pow, type = "b", pch = 19, col = "darkgreen",
         main = "Curva de potencia K–S",
         xlab = "n", ylab = "Potencia", ylim = c(0, 1))
    abline(h = input$pot, lty = 2, col = "red")
    grid()
  })
  
  # --------------------- redacción ----------------------------
  redac <- reactiveVal()
  
  observeEvent(input$gen_redac, {
    txt <- paste0(
      "Se aplicó la prueba de Kolmógorov–Smirnov para contrastar si los datos de ", input$tema,
      " se ajustan a una distribución ", input$dist_redac, ". ",
      "Con una muestra de n = ", input$n_redac, " observaciones, ",
      "el estadístico D fue ", input$D_redac, " (p = ", input$p_redac, "). ",
      ifelse(as.numeric(input$p_redac) < 0.05,
             "El resultado indica un ajuste significativamente deficiente (p < 0.05).",
             "No se encontró evidencia de desajuste (p ≥ 0.05)."),
      " Por tanto, ", ifelse(as.numeric(input$p_redac) < 0.05,
                             "se rechaza la hipótesis nula de ajuste.",
                             "no se rechaza la hipótesis nula de ajuste.")
    )
    redac(txt)
  })
  
  output$parrafo <- renderText(redac())
  
  output$desc_redac <- downloadHandler(
    filename = function() "parrafo_KS.txt",
    content  = function(file) writeLines(redac(), file)
  )
}

shinyApp(ui, server)
