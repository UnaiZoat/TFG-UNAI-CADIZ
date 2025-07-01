library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(bslib)

datosCadizResultados <- read.csv("Cadizcf_resultados_partidos_limpios.csv", header = TRUE, sep = ",")
datosCadizTiros <- read.csv("Cadizcf-tirosLimpios.csv", header = TRUE, sep = ",")
datosTirosEnContra <- read.csv("Cadizcf-tirosencontraLimpios.csv", header = TRUE, sep = ",")
datosTirosJugador <- read.csv("TirosJugadorLimpios.csv",header= TRUE, sep= ",")
datosGolesafavor <- read.csv("golesafavorCadizcfLimpio.csv",header= TRUE, sep= ",")

datosCadizTiros <- datosCadizTiros[-nrow(datosCadizTiros),]
datosTirosEnContra <- datosTirosEnContra[-nrow(datosTirosEnContra),]
datosGolesafavor$Minute <- as.numeric(datosGolesafavor$Minute)

top_goleadores <- datosTirosJugador %>%
  arrange(desc(Goles)) %>%
  slice_head(n=7)

ui <- navbarPage(
  id = "navbar",
  title = div("Análisis del Cádiz CF", style="color: #0033a0; font-weight: bold; font-size: 25px;"),
  theme = bs_theme(
    bootswatch = "flatly",
    bg = "#ffff00",         
    fg = "#0033a0",         
    primary = "#0033a0"
  ),
  
  tabPanel("Inicio",
           fluidPage(
             titlePanel(h1("Bienvenido al Análisis del Cádiz CF", align = "center")),
             tags$img(src = "escudo.png", height = "150px", style = "display: block; margin: auto;"),
             br(),
             HTML(paste0(
               "<div style='background-color:#f3f3f3; padding:15px; border-radius:6px; border-left:6px solid #0033a0; max-width:800px; margin:auto;'>",
               
               "<h5 style='color:#0033a0; font-weight:bold;'>¿Qué encontrarás aquí?</h5>",
               "<p style='text-align:justify; font-size:15px;'>En esta página podrás explorar y aprender sobre las distintas estadísticas del equipo y de los jugadores durante la temporada, incluyendo análisis de Resultados, Tiros, Goles, entre otros. El famoso 'Big Data'</p>",
               
               "<h5 style='color:#0033a0; font-weight:bold;'>¿Qué es el Big Data?</h5>",
               "<p style='text-align:justify; font-size:15px;'>El big data en el fútbol se refiere a la recopilación, procesamiento y análisis de grandes volúmenes de 
               datos relacionados con todos los aspectos del juego.</p>",
               
               "<h5 style='color:#0033a0; font-weight:bold;'>¿Por qué el Big Data es importante para mi equipo?</h5>",
               "<p style='text-align:justify; font-size:15px;'>El análisis de estos datos nos aporta mucha información interesante que puede servirnos para ayudar a mejorar el rendimiento, tomar decisiones
               sobre alineaciones y fichajes, decidir que estilo de juego funciona mejor con nuestra plantilla, etc....
               En esta Web tu también podrás aprender a visualizar y analizar estos datos</p>",
               
               "<p style='text-align:center; margin-top:20px;'>",
               actionButton("go_analisis", "Empieza el análisis", 
             style="color:white; background-color:#0033a0; padding:10px 15px; border-radius:5px;"),
               "</p>",
               
               "</div>"
             )),
             
             br(), br()
           )
  ),
  
  
  
  tabPanel("Análisis",
           sidebarLayout(
             sidebarPanel(
               selectInput("tipo_analisis", "Selecciona el elemento a analizar:",
                           choices = c("Resultados", "Tiros", "Tiros en Contra", "Tiros Jugadores", "Goles a favor")),
               uiOutput("selector_grafico"),
               br(),
               tags$img(src = "escudo.png", height = "130px", style = "display: block; margin-left: auto; margin-right: auto;")
             ),
             
             mainPanel(
               helpText(
                 tags$span(
                   "💡 Puedes interactuar con el gráfico: Descargalo como PNG,muevete por el gráfico, haz zoom o vuelve al zoom original",
                   style = "color: blue; text-align: right; display: block;"
                 )
               ),
               plotlyOutput("grafico", height= "450px"),
               br(),
               uiOutput("detalle_grafico"),
               br()
             )
           )
  ),
  
  tabPanel("Actualiza datos",
           fluidPage(
             titlePanel(h2("Actualizalos datos", align = "center")),
             tags$hr(),
             
             HTML(paste0(
               "<div style='background-color:#f9f9f9; padding:15px; border-radius:6px; border-left:6px solid #0033a0; max-width:850px; margin:auto;'>",
               
               "<h5 style='color:#0033a0;'>📈 ¿Qué puedes hacer aquí?</h5>",
               "<ul style='font-size:15px; text-align:justify;'>",
               "<li><strong>Actualizar los datos</strong> de las categorías actuales con archivos nuevos (por ejemplo, nuevos partidos o estadísticas).</li>",
               "<li><strong>Analizar una temporada anterior</strong> subiendo estadísticas de otra temporada ya disputada</li>",
               "<li>Los archivos deben estar en formato <strong>.csv</strong> y tener una estructura similar a los datos actuales.</li>",
               "</ul>",
               
               "<p style='font-size:14px; color:#555;'>Puedes subir uno o varios archivos (Resultados, Tiros, Goles, etc.) y la app los cargará automáticamente.</p>",
               "</div>"
             )),
             
             br(),
             
             fileInput("cargar_csvs", 
                       label = "📤 Sube tus archivos CSV (uno o varios):", 
                       multiple = TRUE,
                       accept = c(".csv"),
                       width = "60%"),
             
             uiOutput("resultado_carga_csv"),
             
             tags$hr(),
             tags$img(src = "escudo.png", height = "100px", style = "display: block; margin: auto;")
           )
           
           
           ),
  tabPanel("Cambia equipo",
           fluidPage(
             titlePanel(h2("Actualizar o personalizar los datos", align = "center")),
             tags$hr(),
             
             HTML(paste0(
               "<div style='background-color:#f9f9f9; padding:15px; border-radius:6px; border-left:6px solid #0033a0; max-width:850px; margin:auto;'>",
               
               "<h5 style='color:#0033a0;'>📈 ¿Qué puedes hacer aquí?</h5>",
               "<ul style='font-size:15px; text-align:justify;'>",
               "<li><strong>Actualizar los datos</strong> de las categorías actuales con archivos nuevos pertenecientes a otro equipo de la liga española.</li>",
               "<li>Los archivos deben estar en formato <strong>.csv</strong> y tener una estructura similar a los datos actuales.</li>",
               "</ul>",
               
               "<p style='font-size:14px; color:#555;'>Puedes subir uno o varios archivos (Resultados, Tiros, Goles, etc.) y la app los cargará automáticamente.</p>",
               "</div>"
             )),
             
             br(),
             
             fileInput("cargar_csvs", 
                       label = "📤 Sube tus archivos CSV (uno o varios):", 
                       multiple = TRUE,
                       accept = c(".csv"),
                       width = "60%"),
             
             uiOutput("resultado_carga_csv"),
             
             tags$hr(),
             tags$img(src = "escudo.png", height = "100px", style = "display: block; margin: auto;")
           )
           
           
  )
)

server <- function(input, output, session) {
  
  output$selector_grafico <- renderUI({
    opciones <- switch(input$tipo_analisis,
                       "Resultados" = c("Resultados en Casa vs Fuera",
                                        "Posesión según Resultado",
                                        "Relación entre Posesión y Goles Marcados",
                                        "Relación entre Posesión y Goles en Contra",
                                        "Comparación entre xG y Goles Marcados"),
                       
                       "Tiros" = c("Relación entre Disparos y Goles",
                                   "Comparación de xG con y sin penaltis"),
                       
                       "Tiros en Contra" = c("Relación entre Disparos Recibidos y Goles en Contra"),
    
                       "Tiros Jugadores" =c("Goles por disparo vs Goles por disparo a puerta",
                                            "Relacion entre Disparos cada 90min y Goles",
                                            "Comparacion de Goles vs xG",
                                            "Efectividad por edad"),
    
                        "Goles a favor" =c("Goles en Casa vs Fuera",
                                           "Goles con cada parte del cuerpo",
                                           "Distribución goles por minuto",
                                           "Distancia de los Goles"))
    
    selectInput("grafico_seleccionado", "Selecciona un gráfico de análisis:", choices = opciones)
    
    
  })
  
  
  observeEvent(input$go_analisis, {
    updateNavbarPage(session, inputId = "navbar", selected = "Análisis")
  })
  
  output$grafico <- renderPlotly({
    req(input$grafico_seleccionado)
    
    
    
    gg <- switch(input$grafico_seleccionado,
                 
                 "Resultados en Casa vs Fuera" = ggplot(datosCadizResultados, aes(x=Local.Visitante, fill=Resultado)) +
                   geom_bar(position = "dodge") +
                   labs(title = "Resultados en Casa vs Fuera", x= "Condición", y= "Cantidad de Partidos") +
                   mi_tema_cadiz(),
                 
                 "Posesión según Resultado" = ggplot(datosCadizResultados, aes(x=Posesión, fill=Resultado)) +
                   geom_histogram(binwidth = 5, position= "dodge", color="black") +
                   labs(title= "Posesión según Resultado", x="Posesión(%)", y= "Cantidad de Partidos") +
                   mi_tema_cadiz(),
                 
                 "Relación entre Posesión y Goles Marcados" = ggplot(datosCadizResultados, aes(x=Posesión, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   labs(title= "Relación entre Posesión y Goles Marcados", x="Posesión(%)", y= "Goles") +
                   mi_tema_cadiz(),
                 
                 "Relación entre Posesión y Goles en Contra" = ggplot(datosCadizResultados, aes(x=Posesión, y=GC, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   labs(title= "Relación entre Posesión y Goles en Contra", x="Posesión(%)", y= "Goles") +
                   mi_tema_cadiz(),
                 
                 "Comparación entre xG y Goles Marcados" = ggplot(datosCadizResultados, aes(x=xG, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="#ffff00") +
                   labs(title = "Comparación entre xG y Goles Marcados", x="xG (Goles Esperados)", y = "Goles Marcados") +
                   mi_tema_cadiz(),

                 "Relación entre Disparos y Goles" = ggplot(datosCadizTiros, aes(x=Disparos, y=GF)) + 
                   geom_point(color="#ffff00", size=3, alpha=0.7) +
                   geom_smooth(method="lm", color="#ffff00", se=FALSE) +
                   labs(title="Relación entre Disparos y Goles", x="Disparos", y="Goles") +
                   mi_tema_cadiz(),
                 
                 "Comparación de xG con y sin penaltis" = ggplot(datosCadizTiros, aes(x=xG, y=xG...nopenalty)) + 
                   geom_point(color="#ffff00", size=3, alpha=0.7) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="#ffff00") +
                   labs(title="Comparación de xG con y sin penaltis", x="xG", y="xG sin penaltis") +
                   mi_tema_cadiz(),

                 "Relación entre Disparos Recibidos y Goles en Contra" = ggplot(datosTirosEnContra, aes(x=Disparos, y=GF)) + 
                   geom_point(color="#ffff00", size=3, alpha=0.7) +
                   geom_smooth(method="lm", color="#ffff00", se=FALSE) +
                   labs(title="Relación entre Disparos Recibidos y Goles en Contra", x="Disparos Recibidos", y="Goles en Contra") +
                   mi_tema_cadiz(),
                 
                 "Goles por disparo vs Goles por disparo a puerta"=ggplot(top_goleadores, aes(x=Goles.Disparo, y=Goles.DisparoPuerta, color=Nombre, label=Nombre))+
                   geom_point(size=3)+
                   geom_smooth(method = "lm",se=FALSE,color="#ffff00")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title = "Goles por disparo vs Goles por disparo a puerta",
                        x="Goles/Disparo", y="Goles/DisparoPuerta")+
                   mi_tema_cadiz(),
                 
                 "Relacion entre Disparos cada 90min y Goles"=ggplot(top_goleadores, aes(x=Disparos.cada.90min, y=Goles, color=Nombre,label=Nombre))+
                   geom_point(size=2)+
                   geom_smooth(method="lm",se=FALSE,color="#ffff00")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title="Relacion entre Disparos cada 90min y Goles",
                        x="Disparos cada 90min", y="Goles")+
                   mi_tema_cadiz(),
                 
                 "Comparacion de Goles vs xG"= ggplot(top_goleadores,aes(x=xG,y=Goles,color=Nombre,label=Nombre))+
                   geom_point(size=3)+
                   geom_abline(slope = 1,intercept = 0,linetype="dashed",color="#ffff00")+
                   geom_text(hjust=0.5,vjust=1,size=3)+
                   labs(title = "Comparacion de Goles vs xG",
                        x="xG",y="Goles")+
                   mi_tema_cadiz(),
                 
                 "Efectividad por edad"=ggplot(top_goleadores,aes(x=Edad, y=Goles.DisparoPuerta,color=Nombre,label=Nombre))+
                   geom_point(size=3)+
                   geom_smooth(method="lm",se=FALSE,color="#ffff00")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title = "Efectividad por edad",
                        x="Edad",y="Goles por disparo a puerta")+
                   mi_tema_cadiz(),
                 
                 "Goles en Casa vs Fuera"=ggplot(datosGolesafavor, aes(x=Sedes, fill = Sedes))+
                   geom_bar() +
                   labs(title = "Goles en Casa vs Fuera", x="Sede", y="Goles")+
                   mi_tema_cadiz(),
                 
                 "Goles con cada parte del cuerpo"=ggplot(datosGolesafavor, aes(x=Parte.del.cuerpo, fill = Parte.del.cuerpo))+
                   geom_bar() +
                   labs(title = "Goles con cada parte del cuerpo", x="Parte del cuerpo", y="Goles")+
                   mi_tema_cadiz(),
                 
                 "Distribución goles por minuto"=ggplot(datosGolesafavor,aes(x=Minute))+
                   geom_histogram(binwidth = 5, fill="#ffff00", color="black")+
                   labs(title = "Distribución goles por minuto", x="Minuto", y="Cantidad Goles")+
                   mi_tema_cadiz(),
                 
                 "Distancia de los Goles"=ggplot(datosGolesafavor, aes(x=Distance))+
                   geom_histogram(binwidth= 5,fill="#ffff00", color="black") +
                   labs(title = "Distancia de los Goles", x="Distancia(metros)",y="Cantidad Goles")+
                   mi_tema_cadiz()
                 
    )
    
    ggplotly(gg) %>%
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
                          "Resultados en Casa vs Fuera" = "Aquí podemos ver la diferencia de resultados (Victoria, Empate o Derrota) en los partidos jugados como local o visitante",
                          "Posesión según Resultado" = "Aquí podemos ver los distintos porcentajes de posesión en cada partido y si han contribuido a una victoria, un empate o derrota",
                          "Relación entre Posesión y Goles Marcados" = "Una relación entra la posesión y los goles marcados, para comprobar la efectividad de esta. Contiene además información sobre las victorias, empates y derrotas en cada caso",
                          "Relación entre Posesión y Goles en Contra" = "Similar al gráfico anterior pero teniendo en cuenta los goles en contra, lo que indica si cuando se ha cedido posesión se han concedido más goles o no",
                          "Comparación entre xG y Goles Marcados" = "Comparación entre los goles esperados y los goles realmente marcados, para ver si se ha cumplido la efectividad esperada o no",
                          "Relación entre Disparos y Goles" = "Una comparativa entre los disparos y los goles marcados, para ver si la selección de tiro ha sido buena",
                          "Comparación de xG con y sin penaltis" = "Una comparativa para ver si los penaltis han influido mucho en los goles esperados",
                          "Relación entre Disparos Recibidos y Goles en Contra" = "Un análisis de todos los disparos en contra recibidos para ver cuántos se han traducido en un gol para el rival, comprobando así la efectividad de portero y defensas",
                          "Goles por disparo vs Goles por disparo a puerta" = "Comprobación de la efectividad de los mayores goleadores, teniendo en cuenta sus disparos en general y sus disparos a puerta",
                          "Relacion entre Disparos cada 90min y Goles" = "Efectividad de los mayores goleadores teniendo en cuenta cuántos disparos realizan cada 90min y cuántos de esos se traducen en goles",
                          "Comparacion de Goles vs xG" = "Comparación de los goles esperados y los goles marcados de los máximos goleadores",
                          "Efectividad por edad" = "La edad de los máximos goleadores y cómo de efectivos son teniendo en cuenta los goles por disparos a puerta",
                          "Goles en Casa vs Fuera" = "Una comparativa entre los goles conseguidos de local vs los goles conseguidos de visitante",
                          "Goles con cada parte del cuerpo" = "Un análisis de los goles según la parte del cuerpo con la que se ha rematado",
                          "Distribución goles por minuto" = "Cantidad de goles que se han marcado en cada minuto de juego, para comprobar si la efectividad es mayor al inicio o al final del encuentro",
                          "Distancia de los Goles" = "Comparación entre la distancia de los distintos goles logrados, viendo así si se es más efectivo disparando cerca o lejos de la portería rival"
    )
    
    interpretacion <- switch(input$grafico_seleccionado,
                             "Resultados en Casa vs Fuera" = "Este gráfico muestra si el equipo obtiene mejores resultados jugando en casa o fuera. Si las victorias se concentran en casa, podría indicar que el equipo se hace fuerte en su estadio.",
                             "Posesión según Resultado" = "Permite observar si tener más el balón se relaciona con ganar partidos. Una posesión alta en victorias podría indicar un estilo de juego dominante.",
                             "Relación entre Posesión y Goles Marcados" = "Una relación positiva indicaría que el equipo marca más goles cuando domina la posesión, lo que sugiere que se beneficia de controlar el juego.",
                             "Relación entre Posesión y Goles en Contra" = "Una tendencia negativa podría sugerir que ceder la posesión al rival lleva a encajar más goles.",
                             "Comparación entre xG y Goles Marcados" = "Si los goles superan al xG, los delanteros están siendo muy efectivos. Si es al revés, falta efectividad.",
                             "Relación entre Disparos y Goles" = "Una correlación fuerte indica que generar muchos disparos se traduce en goles, mostrando un buen nivel ofensivo.",
                             "Comparación de xG con y sin penaltis" = "Una gran diferencia sugiere que los penaltis tienen un peso importante en la producción ofensiva del equipo.",
                             "Relación entre Disparos Recibidos y Goles en Contra" = "Evalúa la solidez defensiva: si se encajan muchos goles con pocos tiros recibidos, puede haber problemas de portero o defensa.",
                             "Goles por disparo vs Goles por disparo a puerta" = "Identifica a los delanteros más certeros. Cuanto más alto estén, más efectivos son en aprovechar sus oportunidades.",
                             "Relacion entre Disparos cada 90min y Goles" = "Evalúa la relación entre la frecuencia de disparos y los goles marcados, útil para detectar delanteros eficientes.",
                             "Comparacion de Goles vs xG" = "Permite detectar jugadores que superan las expectativas (goles > xG) o que están por debajo del rendimiento esperado.",
                             "Efectividad por edad" = "Muestra si hay relación entre edad y rendimiento, útil para tomar decisiones sobre jóvenes promesas o jugadores veteranos.",
                             "Goles en Casa vs Fuera" = "Ayuda a identificar si el equipo se comporta de forma similar como local y visitante, o si existe dependencia del entorno.",
                             "Goles con cada parte del cuerpo" = "Muestra si el equipo domina el juego aéreo, usa bien ambas piernas, o si hay carencias técnicas.",
                             "Distribución goles por minuto" = "Permite ver si el equipo tiende a marcar en ciertas fases del partido: inicio, medio o final.",
                             "Distancia de los Goles" = "Indica si se prefieren tiros lejanos o cercanos, lo que sugiere el estilo de juego (más directo o más elaborado)."
    )
    
    HTML(paste0(
      "<div style='background-color:#f3f3f3; padding:12px; border-radius:6px; border-left:4px solid #0033a0;'>",
      
      "<h5 style='color:#0033a0; font-weight:bold;'>Explicación del gráfico</h5>",
      "<p style='text-align:justify;'>", explicacion, "</p>",
      
      "<h5 style='color:#0033a0; font-weight:bold;'>¿Qué conclusiones puedo sacar?</h5>",
      "<p style='text-align:justify;'>", interpretacion, "</p>",
      
      "</div>"
    ))
  })

  observeEvent(input$cargar_csvs, {
    req(input$cargar_csvs)
    
    output$resultado_carga_csv <- renderUI({
      archivos <- input$cargar_csvs$name
      HTML(paste0(
        "<div style='color:green; font-size:16px;'>✅ Se han subido los siguientes archivos:</div>",
        "<ul>",
        paste0("<li>", archivos, "</li>", collapse = ""),
        "</ul>"
      ))
    })
    
    for (i in 1:nrow(input$cargar_csvs)) {
      archivo <- input$cargar_csvs$datapath[i]
      nombre <- tolower(input$cargar_csvs$name[i])
      
      df <- read.csv(archivo)
      
      if (grepl("resultados", nombre)) {
        resultados_data <<- df
      } else if (grepl("tirosjugadores", nombre)) {
        tiros_jugadores_data <<- df
      } else if (grepl("tiros", nombre)) {
        tiros_data <<- df
      } else if (grepl("goles", nombre)) {
        goles_data <<- df
      } else {
        warning(paste("Archivo no reconocido:", nombre))
      }
    }
  })
  
}

mi_tema_cadiz <- function(){
  theme_minimal() +
    theme(
      panel.background = element_rect(fill= "#0033a0"),
      plot.background = element_rect(fill = "#0033a0"),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray80"),
      text = element_text(family = "Arial", size = 15, color = "#ffff00"),
      plot.title = element_text(face = "bold",hjust=0.5,size=20,color= "#ffff00"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "white"),
      legend.position = "bottom",
      legend.title = element_blank()
      
    )
}

shinyApp(ui = ui, server = server)
