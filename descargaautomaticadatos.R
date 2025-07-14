if (!require("rvest")) install.packages("rvest")
if (!require("writexl")) install.packages("writexl")

library(rvest)
library(writexl)

año <- 2020

temporada <- paste0(año, "-", año + 1)
url <- paste0("https://fbref.com/en/squads/ee7c297c/", temporada, "/Cadiz-Stats")

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
  "xGA" = "xGA",
  "Poss" = "Posesión",
  "Attendance" = "Asistencia",
  "Captain" = "Capitán",
  "Formation" = "Formación",
  "Opp Formation" = "Formación Rival",
  "Referee" = "Árbitro",
  "Match Report" = "Informe",
  "Notes" = "Notas"
)

tables <- read_html(url) %>%
  html_table(header = TRUE)

df <- tables[[2]]

colnames(df) <- ifelse(
  colnames(df) %in% names(cambio_nombres),
  cambio_nombres[colnames(df)],
  colnames(df) 
)

nombre_archivo <- paste0("cadizresultados", año, ".csv")
write.csv(df, nombre_archivo, row.names = FALSE)

message(paste("✅ Archivo generado:", nombre_archivo))