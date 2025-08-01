año <- 2021


scripts <- c("descargaautomaticadatosresultados.R",
             "descargaautomaticadatostiros.R",
             "descargaautomaticadatostirosencontra.R",
             "descargaautomaticadatosgolesafavor.R",
             "descargaautomaticadatostopgoleadores.R")


for (script in scripts) {
  cat(paste("🔄 Ejecutando", script, "para el año", año, "\n"))
  source(script, local = TRUE)
}