library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

datosCadizResultados <- read.csv("Cadizcf_resultados_partidos_limpios.csv", header = TRUE, sep = ",")
datosCadizTiros <- read.csv("Cadizcf-tirosLimpios.csv", header = TRUE, sep = ",")
datosTirosEnContra <- read.csv("Cadizcf-tirosencontraLimpios.csv", header = TRUE, sep = ",")

datosCadizTiros <- datosCadizTiros[-nrow(datosCadizTiros),]
datosTirosEnContra <- datosTirosEnContra[-nrow(datosTirosEnContra),]

ui <- fluidPage(
  titlePanel("Análisis del Cádiz CF"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_analisis", "Selecciona el análisis:",
                  choices = c("Resultados", "Tiros", "Tiros en Contra")),
      
      uiOutput("selector_grafico")
    ),
    
    mainPanel(
      plotOutput("grafico")
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
                       
                       "Tiros en Contra" = c("Relación entre Disparos Recibidos y Goles en Contra"))
    
    selectInput("grafico_seleccionado", "Selecciona un gráfico:", choices = opciones)
  })
  
  output$grafico <- renderPlot({
    req(input$grafico_seleccionado)  # Asegurar que se haya seleccionado algo
    
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
                   theme_minimal()
    )
    
    gg 
  })
}

shinyApp(ui = ui, server = server)
