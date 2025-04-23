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

ui <- fluidPage(
  theme = bs_theme(
    bg = "#ffff00",         
    fg = "#0000ff",         
    primary = "#0000ff",    
    base_font = font_google("Roboto")  
  ),
  
  titlePanel(
    tags$h1("Análisis del Cádiz CF", 
            style = "color:#0000ff; font-size: 40px; font-family: 'Roboto'; text-shadow: 1px 1px #0000ff;text-align: center;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_analisis", "Selecciona el elemento a analizar:",
                  choices = c("Resultados", "Tiros", "Tiros en Contra", "Tiros Jugadores", "Goles a favor")),
      
      uiOutput("selector_grafico")
    ),
    
    mainPanel(
      plotOutput("grafico"),
      
      br(),
      uiOutput("explicacion_grafico"),
      br(),
      tags$img(src = "escudo.png", 
               height = "150px", 
               style = "display: center; margin-left: 100px; margin-right: 100px;")
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
  
  output$grafico <- renderPlot({
    req(input$grafico_seleccionado)  
    
    gg <- switch(input$grafico_seleccionado,
                 
                 "Resultados en Casa vs Fuera" = ggplot(datosCadizResultados, aes(x=Local.Visitante, fill=Resultado)) +
                   geom_bar(position = "dodge") +
                   labs(title = "Resultados en Casa vs Fuera", x= "Condición", y= "Cantidad de Partidos") +
                   theme_minimal(),
                 
                 "Posesión según Resultado" = ggplot(datosCadizResultados, aes(x=Posesión, fill=Resultado)) +
                   geom_histogram(binwidth = 5, position= "dodge", color="black") +
                   labs(title= "Posesión según Resultado", x="Posesión(%)", y= "Cantidad de Partidos") +
                   theme_minimal(),
                 
                 "Relación entre Posesión y Goles Marcados" = ggplot(datosCadizResultados, aes(x=Posesión, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   labs(title= "Relación entre Posesión y Goles Marcados", x="Posesión(%)", y= "Goles") +
                   theme_minimal(),
                 
                 "Relación entre Posesión y Goles en Contra" = ggplot(datosCadizResultados, aes(x=Posesión, y=GC, color=Resultado)) +
                   geom_point(size=3) +
                   geom_smooth(method = "lm", se=FALSE) +
                   labs(title= "Relación entre Posesión y Goles en Contra", x="Posesión(%)", y= "Goles") +
                   theme_minimal(),
                 
                 "Comparación entre xG y Goles Marcados" = ggplot(datosCadizResultados, aes(x=xG, y=GF, color=Resultado)) +
                   geom_point(size=3) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="black") +
                   labs(title = "Comparación entre xG y Goles Marcados", x="xG (Goles Esperados)", y = "Goles Marcados") +
                   theme_minimal(),

                 "Relación entre Disparos y Goles" = ggplot(datosCadizTiros, aes(x=Disparos, y=GF)) + 
                   geom_point(color="blue", size=3, alpha=0.7) +
                   geom_smooth(method="lm", color="red", se=FALSE) +
                   labs(title="Relación entre Disparos y Goles", x="Disparos", y="Goles") +
                   theme_minimal(),
                 
                 "Comparación de xG con y sin penaltis" = ggplot(datosCadizTiros, aes(x=xG, y=xG...nopenalty)) + 
                   geom_point(color="blue", size=3, alpha=0.7) +
                   geom_abline(slope = 1, intercept = 0, linetype="dashed", color="red") +
                   labs(title="Comparación de xG con y sin penaltis", x="xG", y="xG sin penaltis") +
                   theme_minimal(),

                 "Relación entre Disparos Recibidos y Goles en Contra" = ggplot(datosTirosEnContra, aes(x=Disparos, y=GF)) + 
                   geom_point(color="blue", size=3, alpha=0.7) +
                   geom_smooth(method="lm", color="red", se=FALSE) +
                   labs(title="Relación entre Disparos Recibidos y Goles en Contra", x="Disparos Recibidos", y="Goles en Contra") +
                   theme_minimal(),
                 
                 "Goles por disparo vs Goles por disparo a puerta"=ggplot(top_goleadores, aes(x=Goles.Disparo, y=Goles.DisparoPuerta, color=Nombre, label=Nombre))+
                   geom_point(size=3)+
                   geom_smooth(method = "lm",se=FALSE,color="red")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title = "Goles por disparo vs Goles por disparo a puerta",
                        x="Goles/Disparo", y="Goles/DisparoPuerta")+
                   theme_minimal(),
                 
                 "Relacion entre Disparos cada 90min y Goles"=ggplot(top_goleadores, aes(x=Disparos.cada.90min, y=Goles, color=Nombre,label=Nombre))+
                   geom_point(size=2)+
                   geom_smooth(method="lm",se=FALSE,color="blue")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title="Relacion entre Disparos cada 90min y Goles",
                        x="Disparos cada 90min", y="Goles")+
                   theme_minimal(),
                 
                 "Comparacion de Goles vs xG"= ggplot(top_goleadores,aes(x=xG,y=Goles,color=Nombre,label=Nombre))+
                   geom_point(size=3)+
                   geom_abline(slope = 1,intercept = 0,linetype="dashed",color="red")+
                   geom_text(hjust=0.5,vjust=1,size=3)+
                   labs(title = "Comparacion de Goles vs xG",
                        x="xG",y="Goles")+
                   theme_minimal(),
                 
                 "Efectividad por edad"=ggplot(top_goleadores,aes(x=Edad, y=Goles.DisparoPuerta,color=Nombre,label=Nombre))+
                   geom_point(size=3)+
                   geom_smooth(method="lm",se=FALSE,color="red")+
                   geom_text(hjust=0.5,vjust=-1,size=3)+
                   labs(title = "Efectividad por edad",
                        x="Edad",y="Goles por disparo a puerta")+
                   theme_minimal(),
                 
                 "Goles en Casa vs Fuera"=ggplot(datosGolesafavor, aes(x=Sedes, fill = Sedes))+
                   geom_bar() +
                   labs(title = "Goles en Casa vs Fuera", x="Sede", y="Goles")+
                   theme_minimal(),
                 
                 "Goles con cada parte del cuerpo"=ggplot(datosGolesafavor, aes(x=Parte.del.cuerpo, fill = Parte.del.cuerpo))+
                   geom_bar() +
                   labs(title = "Goles con cada parte del cuerpo", x="Parte del cuerpo", y="Goles")+
                   theme_minimal(),
                 
                 "Distribución goles por minuto"=ggplot(datosGolesafavor,aes(x=Minute))+
                   geom_histogram(binwidth = 5, fill="blue", color="black")+
                   labs(title = "Distribución goles por minuto", x="Minuto", y="Cantidad Goles")+
                   theme_minimal(),
                 
                 "Distancia de los Goles"=ggplot(datosGolesafavor, aes(x=Distance))+
                   geom_histogram(binwidth= 5,fill="blue", color="black") +
                   labs(title = "Distancia de los Goles", x="Distancia(metros)",y="Cantidad Goles")+
                   theme_minimal()
                 
    )
    
    gg 
  })
  output$explicacion_grafico <- renderUI({
    
    texto <- switch(input$grafico_seleccionado,
           
           "Resultados en Casa vs Fuera" = "Aquí podemos ver la diferencia de resultados (Victoria,Empate o Derrota) en los partidos jugados como local o visitante",
           
           "Posesión según Resultado" = "Aquí podemos ver los distintos porcentajes de posesión en cada partido y si han contribuido a una victoria, un empate o derrota",
           
           "Relación entre Posesión y Goles Marcados" = "Una relación entra la posesión y los goles marcados, para comprobar la efectividad de esta. Contiene además información sobre las victorias, empates y derrotas en cada caso",
           
           "Relación entre Posesión y Goles en Contra" = "Similar al gráfico anterior pero teniendo en cuenta los goles en contra, lo que indica si cuando se ha cedido posesión se han concedido más goles o no",
           
           "Comparación entre xG y Goles Marcados" = "Comparación entre los goles esperados y los goles realmente marcados, para ver si se ha cumplido la efectividad esperada o no",
           
           "Relación entre Disparos y Goles" = "Una comparativa entre los disparos y los goles marcados, para ver si la selección de tiro ha sido buena",
           
           "Comparación de xG con y sin penaltis" = "Una comparativa para ver si los penaltis han influido mucho en los goles esperados",
           
           "Relación entre Disparos Recibidos y Goles en Contra" = "Un análisis de todos los disparos en contra recibidos para ver cuántos se han traducido en un gol para el rival, comprobando así la efectividad de portero y defensas",
           
           "Goles por disparo vs Goles por disparo a puerta" = "Comprobación de la efectivad de los mayores goleadores, teniendo en cuenta sus disparos en general y sus disparos a puerta",
           
           "Relacion entre Disparos cada 90min y Goles" = "Efectividad de los mayores goleadores teniendo en cuenta cuantos disparos realizan cada 90min y cuantos de esos se traducen en goles",
           
           "Comparacion de Goles vs xG" = "Comparación de los goles esperados y los goles marcados de los máximos goleadores",
           
           "Efectividad por edad" = "La edad de los máximos goleadores y cómo de efectivos son teniendo en cuenta los goles por disparos a puerta",
           
           "Goles en Casa vs Fuera" = "Una comparativa entre los goles conseguidos de local vs los goles conseguidos de visitante",
           
           "Goles con cada parte del cuerpo" = "Un análisis de los goles según la parte del cuerpo con la que se ha rematado",
           
           "Distribución goles por minuto" = "Cantidad de goles que se han marcado en cada minuto de juego, para comprobar si la efectividad es mayor al inicio o al final del encuentro",
           
           "Distancia de los Goles" = "Comparación entre la distancia de los distintos goles logrados, viendo así si se es más efectivo disparando cerca o lejos de la porteria rival"
           
           )
    
    HTML(paste0("<div style='text-align: justified; font-style:bold; color: #0000ff;'>", texto, "</div>"))
  })
}

shinyApp(ui = ui, server = server)
