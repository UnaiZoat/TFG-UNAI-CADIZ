if (!require("rvest")) install.packages("rvest")
if (!require("writexl")) install.packages("writexl")
library(rvest)
library(writexl)

if (!exists("año")) {
  año <- 2023  
}
if (!exists("codigo_equipo")) codigo_equipo <- "ee7c297c"

temporada <- paste0(año, "-", año + 1)
url <- paste0("https://fbref.com/en/squads/", codigo_equipo, "/", temporada,"/Cadiz-Stats#all_stats_standard")

cambio_nombres <- c(
  "Player" = "Nombre",
  "Nation" = "País", 
  "Pos" = "Posición",
  "Age" = "Edad",
  "90s" = "90s",
  "Gls" = "Goles",
  "Sh" = "Disparos",
  "SoT" = "Disparos a puerta",
  "SoT%" = "% DisparosPuerta",
  "Sh/90" = "Disparos cada 90min",
  "SoT/90" = "DisparosPuerta cada 90min",
  "G/Sh" = "Goles/Disparo",
  "G/SoT" = "Goles/DisparoPuerta",
  "Dist" = "Distancia",
  "FK" = "Tiros Libres",
  "PK" = "Penaltis",
  "PKatt" = "Penaltis Intentados",
  "xG" = "xG",
  "npxG" = "xG - nopenalty",
  "npxG/Sh" = "npxG/Disparo",
  "G-xG" = "G-xG",
  "np:G-xG" = "np:G-xG",
  "Matches" = "Matches"
)

tryCatch({
  tables <- read_html(url) %>%
    html_table(header = FALSE)  # Cambiar a FALSE para manejar encabezados manualmente
  
  if (length(tables) == 0) {
    stop("No se encontraron tablas en la página")
  }
  
  df <- tables[[5]]
  
  # Encontrar la fila que contiene los encabezados reales (Player, Nation, etc.)
  fila_encabezados <- which(df[,1] == "Player")[1]
  
  if (!is.na(fila_encabezados)) {
    # Usar esa fila como nombres de columnas
    colnames(df) <- as.character(df[fila_encabezados, ])
    
    # Eliminar todas las filas hasta los encabezados (inclusive)
    df <- df[(fila_encabezados + 1):nrow(df), ]
  }
  
  # Eliminar filas vacías en la primera columna
  df <- df[df[,1] != "", ]
  
  # Eliminar las filas de totales al final si existen
  df <- df[!grepl("^(Squad Total|Opponent Total)$", df[,1], ignore.case = TRUE), ]
  
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
  
  nombre_archivo <- paste0("equipotopgoleadores", año, ".csv")
  write.csv(df, nombre_archivo, row.names = FALSE)
  
  message(paste("✅ Archivo generado:", nombre_archivo))
  message(paste("📊 Dimensiones:", nrow(df), "filas x", ncol(df), "columnas"))
  
}, error = function(e) {
  message("❌ Error al procesar la página:")
  message(e$message)
  message("Verifica la URL y la conexión a internet")
})