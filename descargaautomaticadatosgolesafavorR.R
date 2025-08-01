if (!require("rvest")) install.packages("rvest")
if (!require("writexl")) install.packages("writexl")
library(rvest)
library(writexl)

año <- 2023
temporada <- paste0(año, "-", año + 1)
url <- paste0("https://fbref.com/en/squads/ee7c297c/", temporada, "/goallogs/c12/Cadiz-Goal-Logs-La-Liga")


cambio_nombres <- c(
  "Date" = "Fecha",
  "Time" = "Hora",
  "Comp" = "Competición",
  "Round" = "Jornada",
  "Day" = "Día",
  "Venue" = "Local.Visitante",
  "Result" = "Resultado",
  "GF" = "GF",
  "GA" = "GC",
  "Opponent" = "Rival",
  "xG" = "xG",
  "npxG" = "xG - nopenalty",
  "G-xG" = "G-xG",
  "np:G-xG" = "np:G-xG",
  "xGA" = "xGA",
  "Poss" = "Posesión",
  "Attendance" = "Asistencia",
  "Captain" = "Capitán",
  "Formation" = "Formación",
  "Opp Formation" = "Formación Rival",
  "Referee" = "Árbitro",
  "Match Report" = "Informe",
  "Notes" = "Notas",
  "Gls" = "Goles Marcados",
  "Sh" = "Disparos",
  "SoT" = "Disparos a puerta",
  "SoT%" = "% DisparosPuerta",
  "G/Sh" = "Goles/Disparo",
  "G/SoT" = "Goles/DisparoPuerta",
  "Dist" = "Distancia",
  "Dist(yds)" = "Distancia"
)


tryCatch({
  tables <- read_html(url) %>%
    html_table(header = TRUE)
  
  
  if (length(tables) == 0) {
    stop("No se encontraron tablas en la página")
  }
  
  df <- tables[[1]]
  
  
  cat("Columnas originales encontradas:\n")
  print(colnames(df))
  cat("\n")
  
  
  nombres_actuales <- colnames(df)
  nombres_nuevos <- nombres_actuales
  
  
  for (i in seq_along(nombres_actuales)) {
    if (nombres_actuales[i] %in% names(cambio_nombres)) {
      nombres_nuevos[i] <- cambio_nombres[nombres_actuales[i]]
    }
  }
  
  colnames(df) <- nombres_nuevos
  
  
  cat("Columnas después del cambio:\n")
  print(colnames(df))
  cat("\n")
  
  
  columnas_no_encontradas <- names(cambio_nombres)[!names(cambio_nombres) %in% nombres_actuales]
  if (length(columnas_no_encontradas) > 0) {
    cat("Columnas del diccionario que no se encontraron en la tabla:\n")
    print(columnas_no_encontradas)
    cat("\n")
  }
  
  
  nombre_archivo <- paste0("equipogolesafavor", año, ".csv")
  write.csv(df, nombre_archivo, row.names = FALSE)
  
  message(paste("✅ Archivo generado:", nombre_archivo))
  message(paste("📊 Dimensiones:", nrow(df), "filas x", ncol(df), "columnas"))
  
}, error = function(e) {
  message("❌ Error al procesar la página:")
  message(e$message)
  message("Verifica la URL y la conexión a internet")
})