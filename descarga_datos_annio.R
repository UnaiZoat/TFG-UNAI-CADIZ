args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("❌ Debes proporcionar el código del equipo y al menos un año. Ejemplo: Rscript descargar_datos.R ee7c297c 2023")
}


codigo_equipo <- args[1]


años <- as.integer(args[-1])

scripts <- c("descargaautomaticadatosresultados.R",
             "descargaautomaticadatostiros.R",
             "descargaautomaticadatostirosencontra.R",
             "descargaautomaticadatosgolesafavor.R",
             "descargaautomaticadatostopgoleadores.R")


for (año in años) {
  cat(paste0("\n📅 Procesando datos para el año ", año, " y equipo ", codigo_equipo, "\n"))
  for (script in scripts) {
    cat(paste0("▶️ Ejecutando ", script, " para ", año, "\n"))
    source(script, local = TRUE)
    Sys.sleep(2)  
  }
}