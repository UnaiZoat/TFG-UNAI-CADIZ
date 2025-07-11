if (!require("rvest")) install.packages("rvest")
if (!require("writexl")) install.packages("writexl")

library(rvest)
library(writexl)

# ⚙️ Define el año de inicio de la temporada (ej. 2021 para la 2021-2022)
año <- 2020

# Crea el string de temporada
temporada <- paste0(año, "-", año + 1)

# Construye la URL
url <- paste0("https://fbref.com/en/squads/ee7c297c/", temporada, "/Cadiz-Stats")

# Lee la página y extrae las tablas
tables <- read_html(url) %>%
  html_table(header = TRUE)

# Selecciona la tabla de resultados
df <- tables[[2]]

# Exporta como CSV
nombre_archivo <- paste0("cadizresultados", año, ".csv")
write.csv(df, nombre_archivo, row.names = FALSE)

message(paste("Hecho:", nombre_archivo))

