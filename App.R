library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(bslib)
library(promises)
library(future)
plan(multisession)

datosCadizResultados2023 <- read.csv("equiporesultados2023.csv", header = TRUE, sep = ",")
datosCadizResultados2022 <- read.csv("equiporesultados2022.csv", header = TRUE, sep = ",")
datosCadizResultados2021 <- read.csv("equiporesultados2021.csv", header = TRUE, sep = ",")
datosCadizResultados2020 <- read.csv("equiporesultados2020.csv", header = TRUE, sep = ",")
datosCadizTiros2023 <- read.csv("equipotiros2023.csv", header = TRUE, sep = ",")
datosCadizTiros2022 <- read.csv("equipotiros2022.csv", header = TRUE, sep = ",")
datosCadizTiros2021 <- read.csv("equipotiros2021.csv", header = TRUE, sep = ",")
datosCadizTiros2020 <- read.csv("equipotiros2020.csv", header = TRUE, sep = ",")
datosCadizTirosEnContra2023 <- read.csv("equipotirosencontra2023.csv", header = TRUE, sep = ",")
datosCadizTirosEnContra2022 <- read.csv("equipotirosencontra2022.csv", header = TRUE, sep = ",")
datosCadizTirosEnContra2021 <- read.csv("equipotirosencontra2021.csv", header = TRUE, sep = ",")
datosCadizTirosEnContra2020 <- read.csv("equipotirosencontra2020.csv", header = TRUE, sep = ",")
datosCadizTopGoleadores2023 <- read.csv("equipotopgoleadores2023.csv",header= TRUE, sep= ",")
datosCadizTopGoleadores2022 <- read.csv("equipotopgoleadores2022.csv",header= TRUE, sep= ",")
datosCadizTopGoleadores2021 <- read.csv("equipotopgoleadores2021.csv",header= TRUE, sep= ",")
datosCadizTopGoleadores2020 <- read.csv("equipotopgoleadores2020.csv",header= TRUE, sep= ",")
datosCadizGolesAFavor2023 <- read.csv("equipogolesafavor2023.csv",header= TRUE, sep= ",")
datosCadizGolesAFavor2022 <- read.csv("equipogolesafavor2022.csv",header= TRUE, sep= ",")
datosCadizGolesAFavor2021 <- read.csv("equipogolesafavor2021.csv",header= TRUE, sep= ",")
datosCadizGolesAFavor2020 <- read.csv("equipogolesafavor2020.csv",header= TRUE, sep= ",")
datosCadizGolesEnContra2023 <- read.csv("equipogolesencontra2023.csv",header= TRUE, sep= ",")
datosCadizGolesEnContra2022 <- read.csv("equipogolesencontra2022.csv",header= TRUE, sep= ",")
datosCadizGolesEnContra2021 <- read.csv("equipogolesencontra2021.csv",header= TRUE, sep= ",")
datosCadizGolesEnContra2020 <- read.csv("equipogolesencontra2020.csv",header= TRUE, sep= ",")

equipo <- "cadiz"  

if (file.exists("equipo_actual.txt")) {
  equipo <- readLines("equipo_actual.txt", warn = FALSE)[1]
}


tema_equipo_colores <- if (tolower(equipo) == "cadiz") {
  list(
    tema = bs_theme(
      bootswatch = "flatly",
      bg = "#ffff00",         
      fg = "#0033a0",         
      primary = "#0033a0"     
    ),
    bg = "#ffff00",
    texto = "#0033a0",
    borde = "#0033a0",
    boton_bg = "#0033a0",
    boton_fg = "#ffff00"
  )
} else {
  list(
    tema = bs_theme(
      bootswatch = "flatly",
      bg = "#f4f4f4",
      fg = "black",
      primary = "black"
    ),
    bg = "#f4f4f4",
    texto = "black",
    borde = "black",
    boton_bg = "black",
    boton_fg = "#f4f4f4"
  )
}



ui <- navbarPage(
  id = "navbar",
  title = div(paste("Análisis del", toupper(equipo)),
              style = paste0("color:#0033a0;; font-weight: bold; font-size: 25px;")),
  
  theme = tema_equipo_colores$tema,
  tags$style(HTML(paste0("
  body, .container-fluid {
    background-color: ", tema_equipo_colores$bg, " !important;
  }
"))),
  
  
  header = tags$head(
    tags$style(HTML(paste0("
      .navbar-nav > li > a {
        color: ", tema_equipo_colores$texto, " !important;
      }

      .navbar-nav > .active > a,
      .navbar-nav > .active > a:focus,
      .navbar-nav > .active > a:hover {
        color: ", tema_equipo_colores$bg, " !important;
        background-color: ", tema_equipo_colores$texto, " !important;
      }

      .navbar-nav > li > a:hover {
        background-color: ", tema_equipo_colores$borde, " !important;
        color: ", tema_equipo_colores$bg, " !important;
      }
    ")))
  ),
  
  tabPanel("Inicio",
           fluidPage(
             titlePanel(h1(paste("Bienvenido al Análisis del", toupper(equipo)), align = "center",
                           style = paste0("color:", tema_equipo_colores$texto))),
             
             
             br(),
             HTML(paste0(
               "<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ", tema_equipo_colores$borde, "; max-width:800px; margin:auto;'>",
               
               "<h5 style='color:", tema_equipo_colores$texto, "; font-weight:bold;'>¿Qué encontrarás aquí?</h5>",
               "<p style='text-align:justify; font-size:15px; color:", tema_equipo_colores$texto, ";'>En esta página podrás explorar y aprender sobre las distintas estadísticas del equipo y de los jugadores durante la temporada,
      incluyendo análisis de Resultados, Tiros, Goles, entre otros. El famoso 'Big Data'. <br> Encontrarás análisis de los datos de la temporada 2023-2024, pero podrás seleccionar temporadas anteriores
      e incluso combinarlas para apreciar la evolución del equipo. También podrás usar los datos de todas estas temporadas
      para calcular el futuro esperado en la pestaña Predicción. Recuerda que el fútbol es inesperado y no es una ciencia exacta, pero
      gracias al Big Data podrás saber cuál es el futuro más probable.</p>",
               
               "<h5 style='color:", tema_equipo_colores$texto, "; font-weight:bold;'>¿Qué es el Big Data?</h5>",
               "<p style='text-align:justify; font-size:15px; color:", tema_equipo_colores$texto, ";'>El big data en el fútbol se refiere a la recopilación, procesamiento y análisis de grandes volúmenes de 
      datos relacionados con todos los aspectos del juego.</p>",
               
               "<h5 style='color:", tema_equipo_colores$texto, "; font-weight:bold;'>¿Por qué el Big Data es importante para mi equipo?</h5>",
               "<p style='text-align:justify; font-size:15px; color:", tema_equipo_colores$texto, ";'>El análisis de estos datos nos aporta mucha información interesante que puede servirnos para ayudar a mejorar el rendimiento, tomar decisiones
      sobre alineaciones y fichajes, decidir qué estilo de juego funciona mejor con nuestra plantilla, etc.
      En esta Web tú también podrás aprender a visualizar y analizar estos datos.</p>",
               
               "<p style='text-align:center; margin-top:20px;'>",
               actionButton("go_analisis", "Empieza el análisis", 
                            style = paste0("color:white; background-color:", tema_equipo_colores$boton_bg, "; padding:10px 15px; border-radius:5px;")),
               "</p>",
               
               "<p style='text-align:center; margin-top:20px;'>",
               actionButton("cambiar_equipo_btn", "Cambiar Equipo", 
                            style = paste0("color:white; background-color:", tema_equipo_colores$boton_bg, 
                                           "; padding:8px 12px; border-radius:5px; border:none;")),
               "</p>",
               
               "</div>"
             )),
             br(),
             if (equipo == "cadiz"){
               tags$img(src = "escudo.png", height = "150px", style = "display: block; margin: auto;")
             },
             br(), br()
           )
  ),

  
  
  tabPanel("Análisis",
           sidebarLayout(
             sidebarPanel(
               selectInput("tipo_analisis", "Selecciona el elemento a analizar:",
                           choices = c("Resultados", "Tiros", "Tiros en Contra", "Tiros Jugadores", "Goles a favor","Goles en contra")),
               uiOutput("selector_grafico"),
               br(),
               
               
               div(
                 style = "border: 1px dashed #0033a0; padding: 10px; background-color: #f0f8ff; border-radius: 5px;",
                 selectInput("temporadas_seleccionadas", 
                             HTML("¿Quieres usar datos de temporadas anteriores?<br>
                                    <span style='font-size: 13px;'>Selecciona una o varias temporadas para analizar:</span>"),
                             choices = c("2023","2022", "2021", "2020"),
                             selected = "2023",
                             multiple = TRUE),
                
                 
                 
                 helpText(
                   tags$span(
                    "Para eliminar una temporada seleccionada, haz clic en ella y pulsa la tecla delete o suprimir de tu teclado.",
                     style = "font-size: 13px; color: #0033a0;"
                   )
                 )
               ),
               
               
               
               br(),
               helpText(
                 tags$span(
                   "¿Algo que no entiendas?",
                   style = "color: #0033a0; text-align: center; display: block;"
                 )
               ),
               
               
               
               actionButton("go_glosario", "Ve al glosario", 
                            style = "color:white; background-color:#0033a0; padding:10px 15px; border-radius:5px; display: block; margin: 0 auto;"),
               
               br(),
               
               if (equipo == "cadiz"){
                 tags$img(src = "escudo.png", height = "150px", style = "display: block; margin: auto;")}),
             
             mainPanel(
               helpText(
                 tags$span(
                   "💡 Puedes interactuar con el gráfico: Descárgalo como PNG, muévete por el gráfico, haz zoom o vuelve al zoom original",
                   style = "color: blue; text-align: right; display: block;"
                 )
               ),
               plotlyOutput("grafico", height = "450px"),
               br(),
               uiOutput("detalle_grafico"),
               br()
             )
           )
  ),
  
  tabPanel("Predicción",
           sidebarLayout(
             sidebarPanel(
               selectInput("tipo_prediccion", "Selecciona el elemento a predecir:",
                           choices = c("Resultados", "Tiros", "Tiros en Contra", "Goles a favor", "Goles en contra")),
               uiOutput("selector_prediccion"),
               br(),
               
               div(
                 style = "border: 1px dashed #0033a0; padding: 10px; background-color: #f0f8ff; border-radius: 5px;",
                 selectInput("temporadas_prediccion", 
                             HTML("Selecciona temporadas para entrenar el modelo:<br>
                           <span style='font-size: 13px;'>Los datos de estas temporadas se usarán para hacer predicciones:</span>"),
                             choices = c("2023","2022", "2021", "2020"),
                             selected = c("2023", "2022"),
                             multiple = TRUE),
                 
                 helpText(
                   tags$span(
                     "Para eliminar una temporada seleccionada, haz clic en ella y pulsa la tecla delete o suprimir de tu teclado.",
                     style = "font-size: 13px; color: #0033a0;"
                   )
                 )
               ),
               
               br(),
               
               
               div(
                 style = "border: 1px solid #28a745; padding: 10px; background-color: #f8fff8; border-radius: 5px;",
                 h5("Información de Predicción", style = "color: #28a745; font-weight: bold;"),
                 
                 tags$p("Se ha utilizado el método de ", 
                        tags$strong("Regresión Lineal."), 
                        style = "margin-top: 5px; color: #155724;")
               ),
               
               br(),
               helpText(
                 tags$span(
                   "¿Algo que no entiendas?",
                   style = "color: #0033a0; text-align: center; display: block;"
                 )
               ),
               
               actionButton("go_glosario", "Ve al glosario", 
                            style = "color:white; background-color:#0033a0; padding:10px 15px; border-radius:5px; display: block; margin: 0 auto;"),
               
               br(),
               
               if (equipo == "cadiz"){
                 tags$img(src = "escudo.png", height = "150px", style = "display: block; margin: auto;")}),
             
             mainPanel(
               helpText(
                 tags$span(
                   "🔮 Predicciones basadas en datos históricos: Descárgalo como PNG, muévete por el gráfico, haz zoom o vuelve al zoom original",
                   style = "color: green; text-align: right; display: block;"
                 )
               ),
               plotlyOutput("grafico_prediccion", height = "450px"),
               br(),
               
              
               
               br(),
               uiOutput("detalle_prediccion"),
               br()
             )
           )
  ),
  
  tabPanel("Glosario",
           fluidPage(
             titlePanel(
               div("📚 Glosario de Términos - Análisis de Datos en Fútbol",
                   style = paste0("color: ", tema_equipo_colores$texto, "; font-weight: bold;"))
             ),
             br(),
             
             
             HTML(paste0("<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ",
                         tema_equipo_colores$borde, "; max-width:850px; margin:auto;'>")),
             h4(style = paste0("color:", tema_equipo_colores$texto), "📈 Métricas de Rendimiento"),
             tags$ul(
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "xG (Goles Esperados): Probabilidad de que un disparo termine en gol según características como ángulo, distancia, tipo de pase previo, etc.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "xGA (Goles Esperados en Contra): xG que el equipo ha concedido. Ayuda a evaluar el rendimiento defensivo más allá de los goles reales encajados.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "xG sin penaltis: Goles esperados excluyendo los penaltis. Útil para medir la calidad ofensiva sin jugadas 'fáciles'.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Disparos cada 90 minutos: Número promedio de disparos realizados por un jugador por cada 90 minutos jugados.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Goles/Disparo: Eficiencia de un jugador: qué porcentaje de sus disparos se convierten en gol.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Goles/Disparo a Puerta: Ratio de goles marcados sobre los tiros que fueron a portería.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Posesión (%): Porcentaje de tiempo que el equipo controla el balón en un partido."))
             ),
             HTML("</div><br>"),
             
             
             HTML(paste0("<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ",
                         tema_equipo_colores$borde, "; max-width:850px; margin:auto;'>")),
             h4(style = paste0("color:", tema_equipo_colores$texto), "📊 Técnicas y Conceptos de Análisis"),
             tags$ul(
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Regresión Lineal: Técnica estadística que intenta predecir una variable (por ejemplo, goles) a partir de otra (por ejemplo, disparos).")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Tendencia: Dirección general del comportamiento de los datos en un gráfico (creciente, decreciente...).")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Correlación: Grado en que dos variables están relacionadas. Ej: Más disparos suele correlacionar con más goles.")),
                ),
             HTML("</div><br>"),
             
            
             HTML(paste0("<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ",
                         tema_equipo_colores$borde, "; max-width:850px; margin:auto;'>")),
             h4(style = paste0("color:", tema_equipo_colores$texto), "🧠 Conceptos de Big Data Deportiva"),
             tags$ul(
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Machine Learning (Aprendizaje Automático): Algoritmos que aprenden de los datos para hacer predicciones, como anticipar el rendimiento de un jugador.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Modelo predictivo: Modelo matemático que utiliza datos pasados para predecir un resultado futuro.")),
               ),
             HTML("</div><br>"),
             
             
             HTML(paste0("<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ",
                         tema_equipo_colores$borde, "; max-width:850px; margin:auto;'>")),
             h4(style = paste0("color:", tema_equipo_colores$texto), "⚽️ Términos del Juego Relacionados con Datos"),
             tags$ul(
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Disparo a Puerta: Tiro que va dentro de los tres palos y que necesita intervención del portero o acaba en gol.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Zona de peligro (Danger Zone): Área cercana al área pequeña desde donde los tiros tienen mayor probabilidad de gol."))
             ),
             HTML("</div><br>"),
             
             
             HTML(paste0("<div style='background-color:#ffffff; padding:15px; border-radius:6px; border-left:6px solid ",
                         tema_equipo_colores$borde, "; max-width:850px; margin:auto;'>")),
             h4(style = paste0("color:", tema_equipo_colores$texto), "📈 Predicción"),
             tags$ul(
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Regresión lineal: Es como trazar una línea recta que intenta predecir una cosa a partir de otra. Por ejemplo: si cuanto más dispara un jugador, más goles marca, la regresión lineal muestra esa relación y permite estimar cuántos goles marcará si dispara X veces.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Modelo predictivo: Usa datos del pasado para anticipar lo que podría pasar en el futuro. Por ejemplo, predecir cuántos goles marcará el equipo según su rendimiento anterior.")),
               tags$li(strong(style = paste0("color:", tema_equipo_colores$texto),
                              "Entrenar el modelo: Entrenamos el modelo eligiendo qué datos tener en cuenta; cuantas más temporadas elijamos, más fiable será la predicción."))
             ),
             HTML("</div><br><br>"),
            
             if (equipo == "cadiz"){
               tags$img(src = "escudo.png", height = "150px", style = "display: block; margin: auto;")})
  )

)



server <- function(input, output, session) {
  
  observeEvent(input$cambiar_equipo_btn, {
    showModal(modalDialog(
      title = "Cambiar Equipo",
      size = "m",
      div(style = "text-align: center; padding: 20px;",
          h4("¿Estás seguro de que quieres cambiar de equipo?", style = "color: #333; margin-bottom: 20px;"),
          br(),
          
          selectInput("selector_equipo", "Selecciona el equipo:",
                      choices = list("Athletic Club" = "2b390eca", "Cadiz CF" = "ee7c297c", "Barcelona" = "206d90db", "Real Madrid" = "53a2f082",
                                     "Atlético de Madrid " = "db3b9613", "Real Betis" = "fc536746", "Sevilla" = "ad2be733", "Valencia" = "dcc91a7b"),
                      selected = "2b390eca",
                      width = "100%"),
          br()
      ),
      footer = tagList(
        actionButton("confirmar_cambio", "Confirmar", 
                     style = "background-color: #28a745; color: white; border: none; padding: 8px 16px; border-radius: 4px; margin-right: 10px;"),
        modalButton("Cancelar")
      ),
      easyClose = TRUE
    ))
  })
  
  
  observeEvent(input$confirmar_cambio, {
    codigo_seleccionado <- input$selector_equipo
    años <- c(2023)
    
    
    removeModal()
    
   
    showModal(modalDialog(
      title = "Cambiando equipo...",
      div(style = "text-align: center; padding: 20px;",
          tags$div(class = "spinner-border", role = "status", style = "color: #007bff;",
                   tags$span(class = "sr-only", "Cargando...")),
          br(), br(),
          h5("Por favor espera mientras se descargan los datos del nuevo equipo."),
          p("Este proceso puede tardar unos minutos...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    
    future({
      tryCatch({

        system(paste("Rscript descarga_datos_annio.R", codigo_seleccionado, paste(años, collapse = " ")))
        return("success")
      }, error = function(e) {
        return(paste("error:", e$message))
      })
    }) %...>% {
      resultado <- .
      
      
      removeModal()
      
      if (resultado == "success") {
        showModal(modalDialog(
          title = "¡Cambio completado!",
          div(style = "text-align: center; padding: 20px;",
              tags$div(style = "color: #28a745; font-size: 48px; margin-bottom: 20px;", "✓"),
              h4("El equipo se ha cambiado exitosamente."),
              p("Por favor, cierre y vuelva a abrir la aplicación para que se muestren los datos actualizados.")
          ),
          easyClose = FALSE,
          footer = NULL  
        ))
      } else {
        
        showModal(modalDialog(
          title = "Error",
          div(style = "text-align: center; padding: 20px;",
              tags$div(style = "color: #dc3545; font-size: 48px; margin-bottom: 20px;", "✗"),
              h4("Ha ocurrido un error al cambiar el equipo."),
              p(gsub("error: ", "", resultado))
          ),
          footer = modalButton("Cerrar"),
          easyClose = TRUE
        ))
      }
    }
  })
  
  

  
  output$selector_grafico <- renderUI({
    opciones <- switch(input$tipo_analisis,
                       "Resultados" = c("Resultados en Casa vs Fuera",
                                        "Posesión según Resultado",
                                        "Relación entre Posesión y Goles Marcados",
                                        "Relación entre Posesión y Goles en Contra",
                                        "Comparación entre xG y Goles Marcados"),
                       
                       "Tiros" = c("Relación entre Disparos y Goles",
                                   "Comparación de xG con y sin penaltis"),
                       
                       "Tiros en Contra" = c("Relación entre Disparos Recibidos y Goles en Contra",
                                             "Comparación xG concedido vs Goles recibidos",
                                             "Distancia media tiros en contra"),
    
                       "Tiros Jugadores" =c("Goles por disparo vs Goles por disparo a puerta",
                                            "Relacion entre Disparos cada 90min y Goles",
                                            "Comparacion de Goles vs xG",
                                            "Efectividad por edad"),
    
                        "Goles a favor" =c("Goles en Casa vs Fuera",
                                           "Goles con cada parte del cuerpo",
                                           "Distribución goles por minuto",
                                           "Distancia de los Goles"),
                  
                       "Goles en contra" = c("Goles en contra Casa vs Fuera",
                                             "Goles en contra con cada parte del cuerpo",
                                             "Distribución goles en contra por minuto",
                                             "Distancia de los Goles en contra"))
    
    selectInput("grafico_seleccionado", "Selecciona un gráfico de análisis:", choices = opciones)
    
    
  })
  
  
  observeEvent(input$go_analisis, {
    updateNavbarPage(session, inputId = "navbar", selected = "Análisis")
  })
  
  observeEvent(input$go_glosario, {
    updateNavbarPage(session, inputId = "navbar", selected = "Glosario")
  })
  
  output$grafico <- renderPlotly({
    req(input$grafico_seleccionado)
    
    sufijo_categoria <- switch(input$grafico_seleccionado,
                               "Resultados en Casa vs Fuera" = "Resultados",
                               "Posesión según Resultado" = "Resultados",
                               "Relación entre Posesión y Goles Marcados" = "Resultados",
                               "Relación entre Posesión y Goles en Contra" = "Resultados",
                               "Comparación entre xG y Goles Marcados" = "Resultados",
                               "Relación entre Disparos y Goles" = "Tiros",
                               "Comparación de xG con y sin penaltis" = "Tiros",
                               "Relación entre Disparos Recibidos y Goles en Contra" = "TirosEnContra",
                               "Comparación xG concedido vs Goles recibidos" = "TirosEnContra",
                               "Distancia media tiros en contra" = "TirosEnContra",
                               "Goles por disparo vs Goles por disparo a puerta" = "TopGoleadores",
                               "Relacion entre Disparos cada 90min y Goles" = "TopGoleadores",
                               "Comparacion de Goles vs xG" = "TopGoleadores",
                               "Efectividad por edad" = "TopGoleadores",
                               "Goles en Casa vs Fuera" = "GolesAFavor",
                               "Goles con cada parte del cuerpo" = "GolesAFavor",
                               "Distribución goles por minuto" = "GolesAFavor",
                               "Distancia de los Goles" = "GolesAFavor",
                               "Goles en contra Casa vs Fuera" = "GolesEnContra",
                               "Goles en contra con cada parte del cuerpo" = "GolesEnContra",
                               "Distribución goles en contra por minuto" = "GolesEnContra",
                               "Distancia de los Goles en contra" = "GolesEnContra",
                               "Resultados" 
    )
    
    datosResultados <- {
      req(input$temporadas_seleccionadas)
      
      lista_temporadas <- input$temporadas_seleccionadas
      
      datos_combinados <- purrr::map_dfr(lista_temporadas, function(anio) {
        nombre_dataset <- paste0("datosCadiz", sufijo_categoria, anio)
        
        if (exists(nombre_dataset)) {
          datos <- get(nombre_dataset)
          datos$Temporada <- anio
          
         
          if ("Asistencia" %in% colnames(datos)) {
            datos <- datos %>% dplyr::mutate(Asistencia = as.numeric(Asistencia))
          }
          
          if ("GF" %in% colnames(datos)) {
            datos <- datos %>% dplyr::mutate(GF = as.numeric(GF))
          }
          
          if ("GC" %in% colnames(datos)) {
            datos <- datos %>% dplyr::mutate(GC = as.numeric(GC))
          }
          
          return(datos)
        } else {
          showNotification(paste("Dataset no encontrado:", nombre_dataset), type = "error")
          return(NULL)
        }
      })
      
      if (sufijo_categoria == "TopGoleadores") {
        if ("Goles" %in% colnames(datos_combinados)) {
          datos_combinados <- datos_combinados %>%
            arrange(desc(Goles)) %>%
            head(12)
        } else {
          datos_combinados <- head(datos_combinados, 8)
        }
      }
      
      if (sufijo_categoria == "Resultados"){
        datos_combinados <- datos_combinados %>%
          mutate(Resultado = case_when(
            Resultado == "W" ~ "Victoria",
            Resultado == "D" ~ "Empate",
            Resultado == "L" ~ "Derrota",
            TRUE ~ Resultado
          ) 
          )%>%
          mutate(
            Local.Visitante = case_when(
              Local.Visitante == "Away" ~ "Visitante",
              Local.Visitante == "Home" ~ "Local",
              TRUE ~ Local.Visitante
            )
          )
      }
      
      if (sufijo_categoria == "GolesAFavor") {
        datos_combinados <- datos_combinados %>%
          mutate(
            Local.Visitante = case_when(
              Local.Visitante == "Away" ~ "Visitante",
              Local.Visitante == "Home" ~ "Local",
              TRUE ~ Local.Visitante
            )
          ) %>%
          mutate(
            Body.Part = case_when(
              Body.Part == "Body.Part" ~ "Parte del cuerpo",
              Body.Part == "Head" ~ "Cabeza",
              Body.Part == "Left Foot" ~ "Pie Izquierdo",
              Body.Part == "Right Foot" ~ "Pie Derecho",
              TRUE ~ Body.Part
            )
          )
      }
      
      if (sufijo_categoria == "GolesEnContra") {
        datos_combinados <- datos_combinados %>%
          mutate(
            Local.Visitante = case_when(
              Local.Visitante == "Away" ~ "Visitante",
              Local.Visitante == "Home" ~ "Local",
              TRUE ~ Local.Visitante
            )
          ) %>%
          mutate(
            Body.Part = case_when(
              Body.Part == "Body.Part" ~ "Parte del cuerpo",
              Body.Part == "Head" ~ "Cabeza",
              Body.Part == "Left Foot" ~ "Pie Izquierdo",
              Body.Part == "Right Foot" ~ "Pie Derecho",
              TRUE ~ Body.Part
            )
          )
      }
      
      
      
      if (nrow(datos_combinados) == 0) {
        showNotification("No hay datos disponibles para el gráfico seleccionado", type = "warning")
        return(NULL)
      }
      
      datos_combinados
    }
    
    gg <- switch(input$grafico_seleccionado,
                 
                 
                 
                 "Resultados en Casa vs Fuera" = ggplot(datosResultados, aes(x=Local.Visitante, fill=Resultado)) +
                   geom_bar(position = "dodge") +
                   facet_wrap(~Temporada) +
                   labs(title = "Resultados en Casa vs Fuera", x= "Condición", y= "Cantidad de Partidos") +
                   mi_tema_cadiz(equipo), 
                 
                 "Posesión según Resultado" = ggplot(datosResultados, aes(x=Posesión, fill=Resultado)) +
                   geom_histogram(binwidth = 5, position= "dodge", color="black") +
                   facet_wrap(~Temporada) +
                   labs(title= "Posesión según Resultado", x="Posesión(%)", y= "Cantidad de Partidos") +
                   mi_tema_cadiz(equipo),
                 
                 "Relación entre Posesión y Goles Marcados" = ggplot(datosResultados, aes(x=Posesión, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   facet_wrap(~Temporada) +
                   labs(title= "Relación entre Posesión y Goles Marcados", x="Posesión(%)", y= "Goles") +
                   mi_tema_cadiz(equipo),
                 
                 "Relación entre Posesión y Goles en Contra" = ggplot(datosResultados, aes(x=Posesión, y=GC, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   facet_wrap(~Temporada) +
                   labs(title= "Relación entre Posesión y Goles en Contra", x="Posesión(%)", y= "Goles") +
                   mi_tema_cadiz(equipo),
                 
                 "Comparación entre xG y Goles Marcados" = ggplot(datosResultados, aes(x=xG, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   facet_wrap(~Temporada) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="#ffff00") +
                   labs(title = "Comparación entre xG y Goles Marcados", x="xG (Goles Esperados)", y = "Goles Marcados") +
                   mi_tema_cadiz(equipo),
                 
                 "Relación entre Disparos y Goles" = ggplot(datosResultados, aes(x=Disparos, y=GF)) + 
                   geom_point(color="#ffff00", size=3, alpha=0.7) +
                   geom_smooth(method="lm", color="#ffff00", se=FALSE) +
                   facet_wrap(~Temporada) +
                   scale_x_continuous(limits = c(0, 25)) +  
                   scale_y_continuous(limits = c(0, 10)) +  
                   labs(title="Relación entre Disparos y Goles", x="Disparos", y="Goles") +
                   mi_tema_cadiz(equipo),
                 
                 "Comparación de xG con y sin penaltis" = ggplot(datosResultados, aes(x=xG, y=xG...nopenalty)) + 
                   geom_point(color="#ffff00", size=3, alpha=0.7) +
                   facet_wrap(~Temporada) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="#ffff00") +
                   scale_x_continuous(limits = c(0, 3)) + 
                   scale_y_continuous(limits = c(0, 3)) +
                   labs(title="Comparación de xG con y sin penaltis", x="xG (Goles Esperados)", y="xG sin penaltis") +
                   mi_tema_cadiz(equipo),
                 
                 "Relación entre Disparos Recibidos y Goles en Contra" = ggplot(datosResultados, aes(x=Disparos, y=GF)) + 
                   geom_jitter(color="#ffff00", size=3, alpha=0.7, width=0.3, height=0.3) +
                   geom_smooth(method="lm", color="#ffff00", se=FALSE) +
                   facet_wrap(~Temporada) +
                   scale_x_continuous(limits=c(0, 25)) +  
                   scale_y_continuous(limits=c(0, 5)) +
                   labs(title="Relación entre Disparos Recibidos y Goles en Contra", x="Disparos Recibidos", y="Goles en Contra") +
                   mi_tema_cadiz(equipo),
                 
                 "Comparación xG concedido vs Goles recibidos" = ggplot(datosResultados, aes(x=xG, y=GF)) +
                   geom_jitter(color="#ffff00", size=3, alpha=0.7, width=0.3, height=0.3) +
                   geom_smooth(method="lm", color="#ffff00", se=FALSE) +
                   facet_wrap(~Temporada) +
                   scale_x_continuous(limits=c(0, 4)) +  
                   scale_y_continuous(limits=c(0, 5)) +
                   labs(title="xG en Contra vs Goles Recibidos", x="xG (Goles Esperados) Concedido", y="Goles Concedidos") +
                   mi_tema_cadiz(equipo),
                 
                 "Distancia media tiros en contra" = ggplot(datosResultados, aes(x=Distancia)) +
                   geom_histogram(binwidth=1, fill="#ffff00", color="black") +
                   facet_wrap(~Temporada) +
                   labs(title="Distancia de los Tiros en Contra", x="Distancia (metros)", y="Número de Tiros") +
                   mi_tema_cadiz(equipo),
                 
                 "Goles por disparo vs Goles por disparo a puerta" = ggplot(
                   datosResultados %>%
                     filter(!Nombre %in% c("Squad Total", "Opponent Total")),
                   aes(x = Goles.Disparo, y = Goles.DisparoPuerta, color = Nombre, label = Nombre)
                 ) +
                   geom_point(size = 3) +
                   facet_wrap(~Temporada) +
                   geom_smooth(method = "lm", se = FALSE, color = "#ffff00") +
                   geom_text(hjust = 0.5, vjust = -1, size = 3) +
                   labs(
                     title = "Goles por disparo vs Goles por disparo a puerta",
                     x = "Goles/Disparo", y = "Goles/DisparoPuerta"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Relacion entre Disparos cada 90min y Goles" = ggplot(
                   datosResultados %>%
                     filter(!Nombre %in% c("Squad Total", "Opponent Total")),
                   aes(x = Disparos.cada.90min, y = Goles, color = Nombre, label = Nombre)
                 ) +
                   geom_point(size = 2) +
                   facet_wrap(~Temporada) +
                   geom_smooth(method = "lm", se = FALSE, color = "#ffff00") +
                   geom_text(hjust = 0.5, vjust = -1, size = 3) +
                   labs(
                     title = "Relación entre Disparos cada 90min y Goles",
                     x = "Disparos cada 90min", y = "Goles"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Comparacion de Goles vs xG" = ggplot(
                   datosResultados %>%
                     filter(!Nombre %in% c("Squad Total", "Opponent Total")),
                   aes(x = xG, y = Goles, color = Nombre, label = Nombre)
                 ) +
                   facet_wrap(~Temporada) +
                   geom_point(size = 3) +
                   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ffff00") +
                   geom_text(hjust = 0.5, vjust = 1, size = 3) +
                   labs(
                     title = "Comparación de Goles vs xG",
                     x = "xG (Goles Esperados)", y = "Goles"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Efectividad por edad" = ggplot(
                   datosResultados %>%
                     filter(!Nombre %in% c("Squad Total", "Opponent Total")),
                   aes(x = Edad, y = Goles.DisparoPuerta, color = Nombre, label = Nombre)
                 ) +
                   geom_point(size = 3) +
                   facet_wrap(~Temporada) +
                   geom_smooth(method = "lm", se = FALSE, color = "#ffff00") +
                   geom_text(hjust = 0.5, vjust = -1, size = 3) +
                   labs(
                     title = "Efectividad por edad",
                     x = "Edad", y = "Goles por disparo a puerta"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Goles en Casa vs Fuera" = ggplot(datosResultados, aes(x=Local.Visitante))+
                   geom_bar(aes(fill = Local.Visitante)) +
                   facet_wrap(~Temporada) +
                   labs(title = "Goles en Casa vs Fuera", x="Local.Visitante", y="Goles")+
                   mi_tema_cadiz(equipo),
                 
                 "Goles con cada parte del cuerpo" = ggplot(
                   datosResultados %>%
                     mutate(`Parte del cuerpo` = trimws(tolower(Body.Part))) %>%
                     filter(!`Parte del cuerpo` %in% c("trace", "unknown", "na", "")),
                   aes(x = `Parte del cuerpo`, fill = `Parte del cuerpo`)
                 ) +
                   geom_bar() +
                   facet_wrap(~Temporada) +
                   labs(
                     title = "Goles con cada parte del cuerpo",
                     x = "Parte del cuerpo",
                     y = "Goles",
                     fill = "Parte del cuerpo"  
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Distribución goles por minuto" = ggplot(
                   datosResultados %>%
                     mutate(Minuto = as.numeric(Minute)),
                   aes(x = Minuto)
                 ) +
                   geom_histogram(binwidth = 5, fill = "#ffff00", color = "black") +
                   facet_wrap(~Temporada) +
                   labs(
                     title = "Distribución goles por minuto",
                     x = "Minuto",
                     y = "Cantidad de Goles"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Distancia de los Goles" = ggplot(datosResultados, aes(x=Distancia))+
                   geom_histogram(binwidth= 5,fill="#ffff00", color="black") +
                   facet_wrap(~Temporada) +
                   labs(title = "Distancia de los Goles", x="Distancia(metros)",y="Cantidad Goles")+
                   mi_tema_cadiz(equipo),
                 
                 "Goles en contra Casa vs Fuera" = ggplot(datosResultados, aes(x=Local.Visitante, fill = Local.Visitante))+
                   geom_bar() +
                   facet_wrap(~Temporada) +
                   labs(title = "Goles en contra en Casa vs Fuera", x="Condición", y="Goles en contra")+
                   mi_tema_cadiz(equipo),
                 
                 "Goles en contra con cada parte del cuerpo" = ggplot(
                   datosResultados %>%
                     mutate(`Parte del cuerpo` = trimws(tolower(Body.Part))) %>%
                     filter(!`Parte del cuerpo` %in% c("trace", "unknown", "na", "")),
                   aes(x = `Parte del cuerpo`, fill = `Parte del cuerpo`)
                 ) +
                   geom_bar() +
                   facet_wrap(~Temporada) +
                   labs(
                     title = "Goles en contra según parte del cuerpo rival",
                     x = "Parte del cuerpo",
                     y = "Goles en contra",
                     fill = "Parte del cuerpo"  
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Distribución goles en contra por minuto" = ggplot(
                   datosResultados %>%
                     mutate(Minuto = as.numeric(Minute)),
                   aes(x = Minuto)
                 ) +
                   geom_histogram(binwidth = 5, fill = "#ffff00", color = "black") +
                   facet_wrap(~Temporada) +
                   labs(
                     title = "Distribución de goles en contra por minuto",
                     x = "Minuto",
                     y = "Goles en contra"
                   ) +
                   mi_tema_cadiz(equipo),
                 
                 "Distancia de los Goles en contra" = ggplot(datosResultados, aes(x=Distancia))+
                   geom_histogram(binwidth = 5, fill="#ffff00", color="black") +
                   facet_wrap(~Temporada) +
                   labs(title = "Distancia de los Goles en contra", x="Distancia (metros)", y="Goles en contra")+
                   mi_tema_cadiz(equipo)
                 
    )
    gg <- gg +
      labs(y = "Cantidad")
    
    ggplotly(gg, tooltip = c("x", "y")) %>%  
      style(hoverlabel = list(namelength = -1)) %>% 
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "zoom2d","select2d", "lasso2d", 
          "autoScale2d",  "hoverCompareCartesian", "hoverClosestCartesian"
        ),
        modeBarButtonsToAdd = list("toImage")
      )
  })
  
  
  
  output$detalle_grafico <- renderUI({
    req(input$grafico_seleccionado)
    
    explicacion <- switch(input$grafico_seleccionado,
                          "Resultados en Casa vs Fuera" = "Este gráfico compara los resultados obtenidos cuando el equipo juega como local frente a cuando juega como visitante.",
                          "Posesión según Resultado" = "Se analiza el porcentaje de posesión del balón en cada partido y su relación con el resultado final.",
                          "Relación entre Posesión y Goles Marcados" = "Explora si tener más el balón está vinculado a marcar más goles.",
                          "Relación entre Posesión y Goles en Contra" = "Evalúa si ceder el balón al rival incrementa el riesgo de encajar goles.",
                          "Comparación entre xG y Goles Marcados" = "Compara los goles esperados (xG) con los goles realmente marcados en cada partido.",
                          "Relación entre Disparos y Goles" = "Mide la efectividad del equipo a la hora de transformar tiros en goles.",
                          "Comparación de xG con y sin penaltis" = "Muestra cuánto influyen los penaltis en los goles esperados.",
                          "Relación entre Disparos Recibidos y Goles en Contra" = "Compara la cantidad de disparos que recibe el equipo con los goles que termina encajando.",
                          "Comparación xG concedido vs Goles recibidos" = "Compara los goles que el rival debería haber marcado (xG en contra) con los que realmente ha marcado.",
                          "Distancia media tiros en contra" = "Analiza desde qué distancia promedio recibe tiros el equipo en cada partido.",
                          "Goles por disparo vs Goles por disparo a puerta" = "Mide la efectividad de los jugadores al convertir sus disparos, en general y a portería.",
                          "Relacion entre Disparos cada 90min y Goles" = "Relaciona la frecuencia de disparos de los jugadores con su capacidad goleadora.",
                          "Comparacion de Goles vs xG" = "Compara goles marcados y esperados por jugador para ver quién rinde por encima o por debajo.",
                          "Efectividad por edad" = "Analiza si hay relación entre la edad de los jugadores y su eficiencia goleadora.",
                          "Goles en Casa vs Fuera" = "Compara los goles que el equipo marca como local y como visitante.",
                          "Goles con cada parte del cuerpo" = "Muestra con qué parte del cuerpo se marcan más goles: cabeza, pie dominante o no dominante.",
                          "Distribución goles por minuto" = "Muestra en qué minutos del partido el equipo suele marcar sus goles.",
                          "Distancia de los Goles" = "Analiza desde qué distancia se marcan los goles, indicando si son fruto de jugadas cercanas o tiros lejanos.",
                          "Goles en contra Casa vs Fuera" = "Compara los goles que el equipo encaja cuando juega como local frente a cuando juega como visitante.",
                          "Goles en contra con cada parte del cuerpo" = "Muestra con qué parte del cuerpo los rivales marcan más goles contra el equipo: cabeza, pie derecho o pie izquierdo.",
                          "Distribución goles en contra por minuto" = "Analiza en qué momentos del partido el equipo es más vulnerable y suele encajar goles.",
                          "Distancia de los Goles en contra" = "Estudia desde qué distancia los rivales logran marcar goles, lo que indica la efectividad defensiva en diferentes zonas.",
)
    
    interpretacion <- switch(input$grafico_seleccionado,
                             "Resultados en Casa vs Fuera" = "📍 Si las victorias se concentran como local, puede indicar fortaleza en casa. Un rendimiento parejo refleja regularidad.",
                             "Posesión según Resultado" = "📊 Si las victorias se asocian con mayor posesión, sugiere que controlar el balón es clave para el equipo.",
                             "Relación entre Posesión y Goles Marcados" = "⚽ Una tendencia positiva revela que dominar el balón genera más ocasiones y goles.",
                             "Relación entre Posesión y Goles en Contra" = "🛡️ Si se reciben más goles al tener menos posesión, puede sugerir fragilidad sin balón.",
                             "Comparación entre xG y Goles Marcados" = "🎯 Si se marcan más goles que los esperados, el equipo está siendo muy efectivo. Si no, falta acierto.",
                             "Relación entre Disparos y Goles" = "📈 Si hay una relación fuerte, crear ocasiones genera rédito. Si no, puede haber fallos en la definición.",
                             "Comparación de xG con y sin penaltis" = "⚖️ Un alto xG por penaltis sugiere dependencia de jugadas a balón parado.",
                             "Relación entre Disparos Recibidos y Goles en Contra" = "🧤 Pocos tiros pero muchos goles indican debilidad defensiva o fallos del portero.",
                             "Comparación xG concedido vs Goles recibidos" = "🔍 Si se encajan más goles que los xG en contra, algo falla en la defensa o la portería.",
                             "Distancia media tiros en contra" = "📏 Si los tiros rivales son cercanos, la defensa está permitiendo ocasiones muy peligrosas.",
                             "Goles por disparo vs Goles por disparo a puerta" = "🧠 Este gráfico muestra qué jugadores aprovechan mejor sus oportunidades.",
                             "Relacion entre Disparos cada 90min y Goles" = "🔥 Jugadores con pocos disparos pero muchos goles son especialmente eficientes.",
                             "Comparacion de Goles vs xG" = "📌 Si un jugador marca más de lo esperado, está en gran forma. Si no, podría estar rindiendo por debajo.",
                             "Efectividad por edad" = "📅 Puede mostrar si los jóvenes están listos para rendir o si la experiencia pesa en la efectividad.",
                             "Goles en Casa vs Fuera" = "🧭 Si el rendimiento varía mucho, puede que el entorno influya en el equipo.",
                             "Goles con cada parte del cuerpo" = "🦶⚽ Una alta proporción con la cabeza podría mostrar dominio aéreo; muchos con la izquierda o derecha indican buena técnica.",
                             "Distribución goles por minuto" = "⏱️ Si los goles llegan tarde, el equipo puede destacar en momentos decisivos. Si llegan al principio, puede ser clave el arranque.",
                             "Distancia de los Goles" = "🚀 Si predominan los goles lejanos, el equipo puede tener jugadores con buen disparo o recurrir al tiro exterior.",
                             "Goles en contra Casa vs Fuera" = "🏠 Si se encajan más goles fuera de casa, puede indicar problemas de adaptación. Un patrón equilibrado sugiere consistencia defensiva.",
                             "Goles en contra con cada parte del cuerpo" = "🎯 Si los rivales marcan mucho de cabeza, puede indicar debilidad en jugadas aéreas. Muchos goles con los pies sugieren problemas en la presión defensiva.",
                             "Distribución goles en contra por minuto" = "⌛ Si se encajan muchos goles al final, puede indicar fatiga o falta de concentración. Goles tempranos pueden sugerir arranques lentos.",
                             "Distancia de los Goles en contra" = "🚨 Si los rivales marcan desde cerca, la defensa está permitiendo ocasiones muy claras. Goles lejanos pueden indicar falta de presión.",
                             
    )
    
    if (is.null(explicacion) || is.null(interpretacion)) {
      return(HTML("<em>No hay información disponible para este gráfico.</em>"))
    }
    
    HTML(paste0(
      
      "<div style='background-color:#f8f9fa; padding:15px; border-radius:8px; border-left:5px solid #0033a0; margin-top:10px;'>",
      
      "<h4 style='color:#0033a0; font-weight:bold; margin-bottom:8px;'>📊 ¿Qué muestra este gráfico?</h4>",
      "<p style='text-align:justify; font-size:15px;'>", explicacion, "</p>",
      
      "<h4 style='color:#0033a0; font-weight:bold; margin-top:15px;'>🔎 ¿Qué conclusiones puedo sacar?</h4>",
      "<p style='text-align:justify; font-size:15px;'>", interpretacion, "</p>",
      resumen_visual <- switch(input$grafico_seleccionado,
                               
                               "Resultados en Casa vs Fuera" = "
    <strong>🟢 Bueno:</strong> Rendimiento equilibrado o mejor fuera de casa.<br>
    <strong>🟡 Normal:</strong> Mejor rendimiento en casa, pero no exagerado.<br>
    <strong>🔴 Preocupante:</strong> Gran diferencia de rendimiento entre casa y fuera.",
                               
                               "Posesión según Resultado" = "
    <strong>🟢 Bueno:</strong> Altos porcentajes de posesión en victorias.<br>
    <strong>🟡 Normal:</strong> Sin patrón claro.<br>
    <strong>🔴 Preocupante:</strong> Alta posesión asociada a derrotas o baja posesión en victorias.",
                               
                               "Relación entre Posesión y Goles Marcados" = "
    <strong>🟢 Bueno:</strong> Más posesión, más goles.<br>
    <strong>🟡 Normal:</strong> Relación neutra o débil.<br>
    <strong>🔴 Preocupante:</strong> Mucha posesión, pocos goles.",
                               
                               "Relación entre Posesión y Goles en Contra" = "
    <strong>🟢 Bueno:</strong> Más posesión, menos goles en contra.<br>
    <strong>🟡 Normal:</strong> Sin patrón definido.<br>
    <strong>🔴 Preocupante:</strong> Más posesión y aún así encajar goles.",
                               
                               "Comparación entre xG y Goles Marcados" = "
    <strong>🟢 Bueno:</strong> Goles marcados por encima del xG.<br>
    <strong>🟡 Normal:</strong> Goles cercanos al xG.<br>
    <strong>🔴 Preocupante:</strong> Goles marcados por debajo del xG.",
                               
                               "Relación entre Disparos y Goles" = "
    <strong>🟢 Bueno:</strong> Alta efectividad con pocos disparos.<br>
    <strong>🟡 Normal:</strong> Relación equilibrada.<br>
    <strong>🔴 Preocupante:</strong> Muchos disparos, pocos goles.",
                               
                               "Comparación de xG con y sin penaltis" = "
    <strong>🟢 Bueno:</strong> Buena producción de xG sin penaltis.<br>
    <strong>🟡 Normal:</strong> Dependencia moderada del penalti.<br>
    <strong>🔴 Preocupante:</strong> Gran parte del xG depende de penaltis.",
                               
                               "Relación entre Disparos Recibidos y Goles en Contra" = "
    <strong>🟢 Bueno:</strong> Muchos disparos en contra y pocos goles encajados.<br>
    <strong>🟡 Normal:</strong> Relación directa y proporcional.<br>
    <strong>🔴 Preocupante:</strong> Se encajan muchos goles con pocos disparos recibidos.",
                               
                               "Comparación xG concedido vs Goles recibidos" = "
    <strong>🟢 Bueno:</strong> Se encajan menos goles de los esperados.<br>
    <strong>🟡 Normal:</strong> Goles recibidos similares al xG concedido.<br>
    <strong>🔴 Preocupante:</strong> Se encajan más goles de los esperados.",
                               
                               "Distancia media tiros en contra" = "
    <strong>🟢 Bueno:</strong> Mayoría de tiros recibidos desde lejos.<br>
    <strong>🟡 Normal:</strong> Distancia media aceptable.<br>
    <strong>🔴 Preocupante:</strong> Muchos tiros desde dentro del área.",
                               
                               "Goles por disparo vs Goles por disparo a puerta" = "
    <strong>🟢 Bueno:</strong> Alta eficiencia en ambos ratios.<br>
    <strong>🟡 Normal:</strong> Relación lógica y progresiva.<br>
    <strong>🔴 Preocupante:</strong> Muy baja conversión incluso cuando se tira a puerta.",
                               
                               "Relacion entre Disparos cada 90min y Goles" = "
    <strong>🟢 Bueno:</strong> Pocos disparos, muchos goles (máxima eficiencia).<br>
    <strong>🟡 Normal:</strong> Relación proporcional.<br>
    <strong>🔴 Preocupante:</strong> Muchos disparos cada 90’ y pocos goles.",
                               
                               "Comparacion de Goles vs xG" = "
    <strong>🟢 Bueno:</strong> Jugadores con más goles que xG.<br>
    <strong>🟡 Normal:</strong> Goles similares al xG.<br>
    <strong>🔴 Preocupante:</strong> Jugadores que no alcanzan su xG.",
                               
                               "Efectividad por edad" = "
    <strong>🟢 Bueno:</strong> Jugadores jóvenes y veteranos efectivos.<br>
    <strong>🟡 Normal:</strong> Efectividad constante en todas las edades.<br>
    <strong>🔴 Preocupante:</strong> Baja efectividad en jugadores clave por edad.",
                               
                               "Goles en Casa vs Fuera" = "
    <strong>🟢 Bueno:</strong> Producción ofensiva estable en ambas condiciones.<br>
    <strong>🟡 Normal:</strong> Algo más de goles en casa.<br>
    <strong>🔴 Preocupante:</strong> Muchos más goles en casa que fuera o viceversa.",
                               
                               "Goles con cada parte del cuerpo" = "
    <strong>🟢 Bueno:</strong> Diversidad de formas de anotar (pie, cabeza...).<br>
    <strong>🟡 Normal:</strong> Predominio claro de una parte.<br>
    <strong>🔴 Preocupante:</strong> Dependencia extrema de una sola forma de rematar.",
                               
                               "Distribución goles por minuto" = "
    <strong>🟢 Bueno:</strong> Más goles en momentos clave (inicio y final).<br>
    <strong>🟡 Normal:</strong> Goles repartidos sin patrón.<br>
    <strong>🔴 Preocupante:</strong> Ausencia de goles en tramos clave.",
                               
                               "Distancia de los Goles" = "
    <strong>🟢 Bueno:</strong> Goles desde varias distancias, incluidos lejanos.<br>
    <strong>🟡 Normal:</strong> Mayoría desde zonas comunes (dentro del área).<br>
    <strong>🔴 Preocupante:</strong> Todos los goles desde posiciones muy cercanas.",
                               
      
                               "Goles en contra Casa vs Fuera" = "
    <strong>🟢 Bueno:</strong> Pocos goles en contra tanto en casa como fuera, o mejor defensivamente fuera.<br>
    <strong>🟡 Normal:</strong> Ligera diferencia a favor de la defensa en casa.<br>
    <strong>🔴 Preocupante:</strong> Muchos más goles en contra fuera de casa o defensivamente frágil en ambos sitios.",
                               
                               "Goles en contra con cada parte del cuerpo" = "
    <strong>🟢 Bueno:</strong> Los rivales no dominan claramente ninguna forma de ataque.<br>
    <strong>🟡 Normal:</strong> Predominio lógico de goles con los pies.<br>
    <strong>🔴 Preocupante:</strong> Excesivos goles de cabeza (debilidad aérea) o desde situaciones específicas.",
                               
                               "Distribución goles en contra por minuto" = "
    <strong>🟢 Bueno:</strong> Pocos goles en contra en momentos clave o repartidos uniformemente.<br>
    <strong>🟡 Normal:</strong> Sin patrón claro de vulnerabilidad temporal.<br>
    <strong>🔴 Preocupante:</strong> Concentración de goles en contra en tramos específicos (fatiga, desconcentración).",
                               
                               "Distancia de los Goles en contra" = "
    <strong>🟢 Bueno:</strong> Los rivales solo marcan desde lejos (buena defensa cercana).<br>
    <strong>🟡 Normal:</strong> Mayoría de goles desde el área (situación típica).<br>
    <strong>🔴 Preocupante:</strong> Muchos goles desde muy cerca (defensa muy permisiva).",
                               
                               NULL
      ),
      
      "</div>"
    ))
  })
  
  output$selector_prediccion <- renderUI({
    opciones <- switch(input$tipo_prediccion,
                       "Resultados" = c("Predicción de Resultados según Posesión",
                                        "Predicción de Goles por xG"
                       ),
                       
                       "Tiros" = c("Predicción de Goles por Disparos",
                                   "Predicción de xG por Disparos",
                                   "Predicción de Eficiencia de Tiro"
                       ),
                       
                       "Tiros en Contra" = c("Predicción de Goles Encajados según Tiros Recibidos",
                                             "Predicción de xGA por Tiros Recibidos"
                       ),
                       "Goles a favor" = c("Distancia de los goles a favor",
                                           "Distribución goles por minuto"),
                       "Goles en contra" = c("Distancia de los goles en contra",
                                             "Distribución goles en contra por minuto")
    )
    
    selectInput("prediccion_seleccionada", "Selecciona una predicción:", choices = opciones)
  })
  
  output$grafico_prediccion <- renderPlotly({
    req(input$prediccion_seleccionada, input$temporadas_prediccion)
    
    sufijo_categoria <- switch(input$prediccion_seleccionada,
                               "Predicción de Resultados según Posesión" = "Resultados",
                               "Predicción de Goles por xG" = "Resultados", 
                               
                               "Predicción de Goles por Disparos" = "Tiros",
                               "Predicción de xG por Disparos" = "Tiros",
                               "Predicción de Eficiencia de Tiro" = "Tiros",
                               
                               "Predicción de Goles Encajados según Tiros Recibidos" = "TirosEnContra",
                               "Predicción de xGA por Tiros Recibidos" = "TirosEnContra",
                               
                               "Distancia de los goles a favor" = "GolesAFavor",
                               "Distribución goles por minuto" = "GolesAFavor",
                               
                               "Distancia de los goles en contra" = "GolesEnContra",
                               "Distribución goles en contra por minuto" = "GolesEnContra"
                               

    )
    
    datos_combinados <- {
      lista_temporadas <- input$temporadas_prediccion
      
      datos_totales <- purrr::map_dfr(lista_temporadas, function(anio) {
        nombre_dataset <- paste0("datosCadiz", sufijo_categoria, anio)
        
        if (exists(nombre_dataset)) {
          datos <- get(nombre_dataset)
          datos$Temporada <- anio
          
         
          numeric_cols <- c("Asistencia", "GF", "GC", "Puntos", "Victorias", "xG", "xGA", 
                            "Disparos", "Tarjetas", "Faltas", "Corners", "Interceptaciones", 
                            "PasesClaves", "Ocasiones", "PosesionFinal", "Ataques", "Posesión")
          
          for(col in numeric_cols) {
            if(col %in% colnames(datos)) {
              datos[[col]] <- as.numeric(datos[[col]])
            }
          }
          
          
          if("GF" %in% colnames(datos) && "Disparos" %in% colnames(datos) && !("EficienciaTiro" %in% colnames(datos))) {
            datos$EficienciaTiro <- ifelse(datos$Disparos > 0, datos$GF / datos$Disparos * 100, 0)
          }
          
          if("GC" %in% colnames(datos) && !("PorteriaCero" %in% colnames(datos))) {
            datos$PorteriaCero <- ifelse(datos$GC == 0, 1, 0)
          }
          
          if("Posesión" %in% colnames(datos) && !("PosesionRival" %in% colnames(datos))) {
            datos$PosesionRival <- 100 - datos$Posesión
          }
          
          return(datos)
        } else {
          showNotification(paste("Dataset no encontrado:", nombre_dataset), type = "error")
          return(NULL)
        }
      })
      
      if (is.null(datos_totales) || nrow(datos_totales) == 0) {
        showNotification("No hay datos disponibles para la predicción seleccionada", type = "warning")
        return(NULL)
      }
      
      datos_totales
    }
    
    if (is.null(datos_combinados)) {
      return(NULL)
    }
    
    gg <- switch(input$prediccion_seleccionada,
                 
                 
                 "Predicción de Resultados según Posesión" = {
                   modelo <- lm(GF ~ Posesión, data = datos_combinados)
                   
                   pred_data <- data.frame(Posesión = seq(min(datos_combinados$Posesión, na.rm = TRUE), 
                                                          max(datos_combinados$Posesión, na.rm = TRUE), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_combinados, aes(x = Posesión, y = GF)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Posesión, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de Goles según Posesión", 
                          x = "Posesión (%)", y = "Goles Predichos") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Predicción de Goles por xG" = {
                   modelo <- lm(GF ~ xG, data = datos_combinados)
                   
                   pred_data <- data.frame(xG = seq(min(datos_combinados$xG, na.rm = TRUE), 
                                                    max(datos_combinados$xG, na.rm = TRUE), 
                                                    length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_combinados, aes(x = xG, y = GF)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = xG, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
                     labs(title = "Predicción de Goles por xG", 
                          x = "xG (Goles Esperados)", y = "Goles Predichos") +
                     mi_tema_cadiz(equipo)
                 },
                 

                 "Predicción de Goles por Disparos" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Disparos), !is.na(GF), Disparos <= quantile(Disparos, 0.95))
                   
                   modelo <- lm(GF ~ Disparos, data = datos_filtrados)
                   
                   pred_data <- data.frame(Disparos = seq(min(datos_filtrados$Disparos), 
                                                          max(datos_filtrados$Disparos), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Disparos, y = GF)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Disparos, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de Goles por Disparos", 
                          x = "Disparos", y = "Goles Predichos") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Predicción de xG por Disparos" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Disparos), !is.na(GF), Disparos <= quantile(Disparos, 0.95))
                   
                   modelo <- lm(xG ~ Disparos, data = datos_filtrados)
                   
                   pred_data <- data.frame(Disparos = seq(min(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          max(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Disparos, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Disparos, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de xG por Disparos", 
                          x = "Disparos", y = "xG (Goles Esperados) Predicho") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Predicción de Eficiencia de Tiro" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Disparos), !is.na(GF), Disparos <= quantile(Disparos, 0.95))
                   
                   modelo <- lm(EficienciaTiro ~ Disparos, data = datos_filtrados)
                   
                   pred_data <- data.frame(Disparos = seq(min(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          max(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Disparos, y = EficienciaTiro)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Disparos, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de Eficiencia de Tiro", 
                          x = "Disparos", y = "Eficiencia de Tiro (%)") +
                     mi_tema_cadiz(equipo)
                 },
                 
                
                 "Predicción de Goles Encajados según Tiros Recibidos" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Disparos), !is.na(GC)) %>%
                     filter(Disparos <= quantile(Disparos, 0.95, na.rm = TRUE))
                   
                   modelo <- lm(GC ~ Disparos, data = datos_filtrados)
                   
                   pred_data <- data.frame(Disparos = seq(min(datos_filtrados$Disparos), 
                                                          max(datos_filtrados$Disparos), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Disparos, y = GC)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Disparos, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de Goles Encajados según Tiros Recibidos", 
                          x = "Tiros Recibidos", y = "Goles Encajados Predichos") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Predicción de xGA por Tiros Recibidos" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Disparos), !is.na(GF), Disparos <= quantile(Disparos, 0.95))
                   
                   modelo <- lm(xG ~ Disparos, data = datos_filtrados)
                   
                   pred_data <- data.frame(Disparos = seq(min(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          max(datos_filtrados$Disparos, na.rm = TRUE), 
                                                          length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Disparos, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Disparos, y = Prediccion), 
                               color = "#ffff00", size = 2) +
                     labs(title = "Predicción de xGA por Tiros Recibidos", 
                          x = "Tiros Recibidos", y = "xGA Predicho") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Distancia de los goles a favor" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Distancia), !is.na(xG)) %>%
                     mutate(Distancia = as.numeric(Distancia)) %>%
                     filter(Distancia <= quantile(Distancia, 0.99, na.rm = TRUE))  # eliminar outliers extremos si los hay
                   
                   modelo <- lm(xG ~ Distancia, data = datos_filtrados)
                   
                   pred_data <- data.frame(Distancia = seq(min(datos_filtrados$Distancia, na.rm = TRUE),
                                                           max(datos_filtrados$Distancia, na.rm = TRUE),
                                                           length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Distancia, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Distancia, y = Prediccion),
                               color = "#ffff00", size = 2) +
                     labs(title = "Relación entre Distancia del Gol y xG",
                          x = "Distancia (m)", y = "xG (Goles Esperados)") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Distancia de los goles en contra" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Distancia), !is.na(xG)) %>%
                     mutate(Distancia = as.numeric(Distancia)) %>%
                     filter(Distancia <= quantile(Distancia, 0.99, na.rm = TRUE))  # eliminar outliers extremos si los hay
                   
                   modelo <- lm(xG ~ Distancia, data = datos_filtrados)
                   
                   pred_data <- data.frame(Distancia = seq(min(datos_filtrados$Distancia, na.rm = TRUE),
                                                           max(datos_filtrados$Distancia, na.rm = TRUE),
                                                           length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Distancia, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Distancia, y = Prediccion),
                               color = "#ffff00", size = 2) +
                     labs(title = "Relación entre Distancia de los Goles en Contra y xG",
                          x = "Distancia (m)", y = "xG (Goles Esperados)") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Distribución goles por minuto" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Minute), !is.na(xG)) %>%
                     mutate(Minute = as.numeric(gsub("\\+.*", "", Minute))) %>%
                     filter(Minute >= 0 & Minute <= 120)
                   
                   modelo <- lm(xG ~ Minute, data = datos_filtrados)
                   
                   pred_data <- data.frame(Minute = seq(min(datos_filtrados$Minute, na.rm = TRUE),
                                                        max(datos_filtrados$Minute, na.rm = TRUE),
                                                        length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Minute, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Minute, y = Prediccion),
                               color = "#ffff00", size = 2) +
                     labs(title = "Relación entre Minuto del Gol y xG",
                          x = "Minuto del gol", y = "xG (Goles Esperados)") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 "Distribución goles en contra por minuto" = {
                   datos_filtrados <- datos_combinados %>%
                     filter(!is.na(Minute), !is.na(xG)) %>%
                     mutate(Minute = as.numeric(gsub("\\+.*", "", Minute))) %>%
                     filter(Minute >= 0 & Minute <= 120)
                   
                   modelo <- lm(xG ~ Minute, data = datos_filtrados)
                   
                   pred_data <- data.frame(Minute = seq(min(datos_filtrados$Minute, na.rm = TRUE),
                                                        max(datos_filtrados$Minute, na.rm = TRUE),
                                                        length.out = 100))
                   pred_data$Prediccion <- predict(modelo, pred_data, type = "response")
                   
                   ggplot(datos_filtrados, aes(x = Minute, y = xG)) +
                     geom_point(aes(color = Temporada), size = 3, alpha = 0.7) +
                     geom_line(data = pred_data, aes(x = Minute, y = Prediccion),
                               color = "#ffff00", size = 2) +
                     labs(title = "Relación entre Minuto del Gol en Contra y xG",
                          x = "Minuto del gol encajado", y = "xG (Goles Esperados)") +
                     mi_tema_cadiz(equipo)
                 },
                 
                 
                 
    )
    
    ggplotly(gg) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "zoom2d","select2d", "lasso2d", 
          "autoScale2d", "hoverCompareCartesian", "hoverClosestCartesian"
        ),
        modeBarButtonsToAdd = list("toImage")
      )
  })
  
  output$detalle_prediccion <- renderUI({
    req(input$prediccion_seleccionada)
    
    div(
      style = "border: 1px solid #6c757d; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      h4("Interpretación de la Predicción", style = "color: #495057; font-weight: bold;"),
      
      switch(input$prediccion_seleccionada,
             # RESULTADOS
             "Predicción de Resultados según Posesión" = p("Este modelo predice los goles basándose en el porcentaje de posesión. Los puntos amarillos muestran la tendencia predicha, mientras que los puntos de colores representan los datos reales de cada temporada."),
             "Predicción de Goles por xG" = p("La línea amarilla muestra la relación predicha entre xG y goles reales. La línea roja discontinua representa la relación perfecta (1:1). Los puntos de colores pertenecen a las temporadas anteriores."),
             
             
             # TIROS
             "Predicción de Goles por Disparos" = p("Este modelo estima cuántos goles se pueden esperar según el número de disparos realizados, basándose en patrones históricos."),
             "Predicción de xG por Disparos" = p("Predice los goles esperados (xG) según el volumen de disparos. Útil para evaluar la calidad ofensiva independientemente de los goles reales."),
             "Predicción de Eficiencia de Tiro" = p("Calcula el porcentaje de conversión de disparos en goles. Una eficiencia alta indica mejor finalización o mejores ocasiones de gol.Es normal que a mayor cantidad de tiros menor sea la eficiencia, ya que no suelen convertirse en gol todos los tiros."),
             
             # TIROS EN CONTRA
             "Predicción de Goles Encajados según Tiros Recibidos" = p("Este modelo predice los goles encajados a partir de los tiros que recibe el equipo. Una línea ascendente indica que más disparos recibidos tienden a traducirse en más goles en contra."),
             "Predicción de xGA por Tiros Recibidos" = p("Predice los goles esperados en contra (xGA) según los disparos recibidos. Ayuda a evaluar la calidad defensiva del equipo."),
             
             # GOLES A FAVOR
             "Distancia de los goles a favor" = p("Este modelo analiza la relación entre la distancia media de los disparos y los goles marcados. Una distancia menor generalmente indica mejores ocasiones de gol y mayor probabilidad de conversión."),
             "Distribución goles por minuto" = p("Este modelo examina la relación entre el minuto en el que se marca un gol y su valor de xG. Permite identificar si el equipo genera mejores ocasiones en momentos específicos del partido."),
             
             # GOLES EN CONTRA
             "Distancia de los goles en contra" = p("Predice los goles encajados según la distancia media desde la que dispara el rival. Distancias menores del rival suelen traducirse en más goles en contra, indicando problemas defensivos en zona de peligro."),
             "Distribución goles en contra por minuto" = p("Este modelo analiza la relación entre el minuto en el que se encaja un gol y su xG. Puede revelar si el equipo tiende a conceder ocasiones peligrosas en fases concretas del encuentro.")
             
             )
    )
  })

}
mi_tema_cadiz <- function(equipo) {
  if (tolower(equipo) == "cadiz") {
    theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#0033a0"),
        plot.background = element_rect(fill = "#0033a0"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray80"),
        text = element_text(family = "Arial", size = 15, color = "#ffff00"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "#ffff00"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "white"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  } else {
    theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        text = element_text(family = "Arial", size = 15, color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "black"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "right",
        legend.title = element_text(face = "bold")
      )
  }
}

shinyApp(ui = ui, server = server)
