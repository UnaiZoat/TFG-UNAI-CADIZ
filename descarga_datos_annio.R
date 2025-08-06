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
             "descargaautomaticadatostopgoleadores.R",
             "descargaautomaticadatosgolesencontra.R")

for (i in seq_along(años)) {
  año <- años[i]
  cat(paste0("\n📅 Procesando datos para el año ", año, " y equipo ", codigo_equipo, "\n"))
  
  
  if (i > 1) {
    cat("⏸️ Pausa entre años (15 segundos)...\n")
    Sys.sleep(15)
  }
  
  for (j in seq_along(scripts)) {
    script <- scripts[j]
    cat(paste0("▶️ Ejecutando ", script, " para ", año, "\n"))
    source(script, local = TRUE)
    
    
    if (j < length(scripts)) {
      tiempo_pausa <- sample(5:10, 1)
      cat(paste0("⏸️ Pausa de ", tiempo_pausa, " segundos...\n"))
      Sys.sleep(tiempo_pausa)
    }
  }
}

nombre_equipo <- if (codigo_equipo == "ee7c297c") "cadiz" else if (codigo_equipo == "2b390eca") "athletic" else
  if (codigo_equipo =="206d90db") "barcelona" else if (codigo_equipo == "53a2f082") "Real Madrid"  else "equipo"
writeLines(nombre_equipo, "equipo_actual.txt")