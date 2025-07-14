if (!require("rvest")) install.packages("rvest")
if (!require("writexl")) install.packages("writexl")
library(rvest)
library(writexl)

año <- 2020
temporada <- paste0(año, "-", año + 1)
url <- paste0("https://fbref.com/en/squads/ee7c297c/", temporada, "/Cadiz-Stats-La-Liga")

# Diccionario de cambio de nombres
cambio_nombres <- c(
  "Date" = "Fecha",
  "Time" = "Hora",
  "Comp" = "Competición",
  "Round" = "Jornada",
  "Day" = "Día",
  "Venue" = "Sedes",
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
  "Gls" = "Goles",
  "Sh" = "Disparos",
  "SoT" = "Disparos a puerta",
  "SoT%" = "% DisparosPuerta",
  "Sh/90" = "Disparos cada 90min",
  "SoT/90" = "DisparosPuerta cada 90min",
  "G/Sh" = "Goles/Disparo",
  "G/SoT" = "Goles/DisparoPuerta",
  "Dist" = "Distancia",
  "Dist(yds)" = "Distance",
  "FK" = "Tiros Libres",
  "PK" = "Penaltis",
  "PKatt" = "Penaltis Intentados",
  "npxG/Sh" = "npxG/Disparo",
  "Scorer" = "Anotador",
  "Body Part" = "Parte del cuerpo",
  "Minute" = "Minute",
  "Score" = "Marcador",
  "Player" = "Nombre",
  "Age" = "Edad",
  "Nation" = "País",
  "Pos" = "Posicion",
  "Gls" = "Goles"
)

# Leer la página web
tryCatch({
  # Leer la tabla sin usar header = TRUE automáticamente
  tables <- read_html(url) %>%
    html_table(header = FALSE, fill = TRUE)
  
  # Verificar que hay tablas
  if (length(tables) == 0) {
    stop("No se encontraron tablas en la página")
  }
  
  df_raw <- tables[[5]]
  
  # Mostrar las primeras filas para diagnóstico
  cat("Primeras filas de la tabla raw:\n")
  print(head(df_raw, 3))
  cat("\n")
  
  
  
  # Usar la primera fila que contiene "Date" como encabezados
  header_row <- header_row[1]
  
  # Extraer los nombres de las columnas
  column_names <- as.character(df_raw[header_row, ])
  
  # Limpiar nombres de columnas (eliminar espacios extra, etc.)
  column_names <- trimws(column_names)
  
  # Mostrar los nombres de columnas encontrados
  cat("Nombres de columnas encontrados:\n")
  print(column_names)
  cat("\n")
  
  # Crear el dataframe final saltando las filas de encabezado
  df <- df_raw[(header_row + 1):nrow(df_raw), ]
  
  # Asignar nombres de columnas
  colnames(df) <- column_names
  
  
  # Cambiar nombres de columnas usando el diccionario
  nombres_actuales <- colnames(df)
  nombres_nuevos <- nombres_actuales
  
  # Cambiar nombres solo para las columnas que existen en ambos lugares
  for (i in seq_along(nombres_actuales)) {
    if (nombres_actuales[i] %in% names(cambio_nombres)) {
      nombres_nuevos[i] <- cambio_nombres[nombres_actuales[i]]
    }
  }
  
  colnames(df) <- nombres_nuevos
  
  # Mostrar las columnas después del cambio
  cat("Columnas después del cambio:\n")
  print(colnames(df))
  cat("\n")
  
  # Mostrar columnas que no se encontraron
  columnas_no_encontradas <- names(cambio_nombres)[!names(cambio_nombres) %in% nombres_actuales]
  if (length(columnas_no_encontradas) > 0) {
    cat("Columnas del diccionario que no se encontraron en la tabla:\n")
    print(columnas_no_encontradas)
    cat("\n")
  }
  
  # Resetear índices de filas
  rownames(df) <- NULL
  
  # Guardar archivo
  nombre_archivo <- paste0("cadiztopgoleadores", año, ".csv")
  write.csv(df, nombre_archivo, row.names = FALSE)
  
  message(paste("✅ Archivo generado:", nombre_archivo))
  message(paste("📊 Dimensiones:", nrow(df), "filas x", ncol(df), "columnas"))
  
  # Mostrar una muestra de los datos
  cat("Muestra de los datos finales:\n")
  print(head(df, 3))
  
}, error = function(e) {
  message("❌ Error al procesar la página:")
  message(e$message)
  message("Verifica la URL y la conexión a internet")
})