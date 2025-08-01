args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("❌ Debes proporcionar al menos un año como argumento. Ejemplo: Rscript descargar_datos.R 2023")
}


años <- as.integer(args)


scripts <- c("descargaautomaticadatosresultados.R",
             "descargaautomaticadatostiros.R",
             "descargaautomaticadatostirosencontra.R",
             "descargaautomaticadatosgolesafavor.R",
             "descargaautomaticadatostopgoleadores.R")


for (año in años) {
  cat(paste0("\n📅 Procesando datos para el año ", año, "\n"))
  for (script in scripts) {
    cat(paste("🔄 Ejecutando", script, "para el año", año, "\n"))
    source(script, local = TRUE)
  }
}