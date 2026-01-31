# =============================================================
# Script para leer y exportar el contenido de una app Shiny
# Ejecuta este script en RStudio o en tu consola de R en Windows
# =============================================================

ruta_app <- "C:/Users/Usuario/Documents/siri vilma axel/calc"

# Listar todos los archivos en la carpeta (recursivo)
archivos <- list.files(ruta_app, recursive = TRUE, full.names = TRUE)

# Filtrar solo archivos relevantes (R, CSV, RDS, txt, html, css, js, etc.)
extensiones <- c("R", "r", "Rmd", "rmd", "csv", "txt", "html", "css",
                 "js", "json", "yaml", "yml", "rds", "RDS", "xlsx", "xls")

cat("=== ARCHIVOS ENCONTRADOS EN:", ruta_app, "===\n\n")
cat("Total de archivos:", length(archivos), "\n\n")

for (archivo in archivos) {
  nombre <- basename(archivo)
  ruta_relativa <- sub(paste0(normalizePath(ruta_app, winslash = "/"), "/"), "",
                       normalizePath(archivo, winslash = "/"))
  ext <- tools::file_ext(archivo)

  cat("-------------------------------------------\n")
  cat("ARCHIVO:", ruta_relativa, "\n")
  cat("-------------------------------------------\n")

  # Solo leer archivos de texto (no binarios)
  if (tolower(ext) %in% c("r", "rmd", "csv", "txt", "html", "css",
                            "js", "json", "yaml", "yml")) {
    contenido <- tryCatch(
      readLines(archivo, warn = FALSE, encoding = "UTF-8"),
      error = function(e) paste("ERROR al leer:", e$message)
    )
    cat(paste(contenido, collapse = "\n"), "\n\n")
  } else {
    cat("[Archivo binario - no se muestra contenido]\n\n")
  }
}

cat("=== FIN DEL REPORTE ===\n")
