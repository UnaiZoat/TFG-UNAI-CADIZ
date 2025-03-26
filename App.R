library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

datosCadizResultados <- read.csv("Cadizcf_resultados_partidos_limpios.csv", header = TRUE, sep = ",")
datosCadizTiros <- read.csv("Cadizcf-tirosLimpios.csv", header = TRUE, sep = ",")
datosTirosEnContra <- read.csv("Cadizcf-tirosencontraLimpios.csv", header = TRUE, sep = ",")
datosTirosJugador <- read.csv("TirosJugadorLimpios.csv",header= TRUE, sep= ",")

datosCadizTiros <- datosCadizTiros[-nrow(datosCadizTiros),]
datosTirosEnContra <- datosTirosEnContra[-nrow(datosTirosEnContra),]

top_goleadores <- datosTirosJugador %>%
  arrange(desc(Goles)) %>%
  slice_head(n=7)

ui <- fluidPage(
  titlePanel("Análisis del Cádiz CF"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_analisis", "Selecciona el análisis:",
                  choices = c("Resultados", "Tiros", "Tiros en Contra", "Tiros Jugadores")),
      
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
                       
                       "Tiros en Contra" = c("Relación entre Disparos Recibidos y Goles en Contra"),
    
                       "Tiros Jugadores" =c("Goles por disparo vs Goles por disparo a puerta",
                                            "Relacion entre Disparos cada 90min y Goles",
                                            "Comparacion de Goles vs xG",
                                            "Efectividad por edad"))
    
    selectInput("grafico_seleccionado", "Selecciona un gráfico:", choices = opciones)
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
                   theme_minimal()
                 
    )
    
    gg 
  })
}

shinyApp(ui = ui, server = server)
