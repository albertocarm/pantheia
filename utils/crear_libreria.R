# =============================================================
# Script para crear la estructura de un paquete R (librería)
# que contiene tu app Shiny, lista para subir a GitHub.
#
# Ejecuta este script en RStudio o consola de R en Windows.
# =============================================================

# --- CONFIGURACION ---
nombre_paquete <- "calc"
ruta_base      <- "C:/Users/Usuario/Documents/siri vilma axel"
ruta_app_orig  <- file.path(ruta_base, "calc")
ruta_paquete   <- file.path(ruta_base, paste0(nombre_paquete, ".pkg"))

# --- CREAR ESTRUCTURA DEL PAQUETE ---
cat("Creando estructura del paquete en:", ruta_paquete, "\n\n")

dirs <- c(
  ruta_paquete,
  file.path(ruta_paquete, "R"),
  file.path(ruta_paquete, "inst", "app"),
  file.path(ruta_paquete, "man")
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  Creado:", d, "\n")
  }
}

# --- DESCRIPTION ---
writeLines(con = file.path(ruta_paquete, "DESCRIPTION"), c(
  paste0("Package: ", nombre_paquete),
  "Title: Aplicacion Shiny Calc",
  "Version: 0.1.0",
  "Authors@R: person('Usuario', role = c('aut', 'cre'), email = 'usuario@example.com')",
  "Description: Paquete que contiene la aplicacion Shiny calc para uso local.",
  "License: GPL-3",
  "Encoding: UTF-8",
  "Depends: R (>= 4.0.0)",
  "Imports: shiny",
  "RoxygenNote: 7.2.3"
))
cat("  Creado: DESCRIPTION\n")

# --- NAMESPACE ---
writeLines(con = file.path(ruta_paquete, "NAMESPACE"), c(
  "export(run_app)",
  "import(shiny)"
))
cat("  Creado: NAMESPACE\n")

# --- R/run_app.R (funcion para lanzar la app) ---
writeLines(con = file.path(ruta_paquete, "R", "run_app.R"), c(
  "#' Ejecutar la aplicacion Shiny",
  "#'",
  "#' @param ... Argumentos adicionales pasados a shiny::runApp()",
  "#' @export",
  "run_app <- function(...) {",
  "  app_dir <- system.file('app', package = 'calc')",
  "  if (app_dir == '') {",
  "    stop('No se encontro la app. Intenta reinstalar el paquete.')",
  "  }",
  "  shiny::runApp(app_dir, ...)",
  "}"
))
cat("  Creado: R/run_app.R\n")

# --- COPIAR ARCHIVOS DE LA APP ORIGINAL A inst/app/ ---
destino_app <- file.path(ruta_paquete, "inst", "app")

archivos_app <- list.files(ruta_app_orig, recursive = TRUE, full.names = TRUE)

cat("\nCopiando archivos de la app original a inst/app/...\n")
for (archivo in archivos_app) {
  ruta_rel <- sub(
    paste0(normalizePath(ruta_app_orig, winslash = "/"), "/"), "",
    normalizePath(archivo, winslash = "/")
  )
  destino <- file.path(destino_app, ruta_rel)
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  file.copy(archivo, destino, overwrite = TRUE)
  cat("  Copiado:", ruta_rel, "\n")
}

# --- .Rbuildignore ---
writeLines(con = file.path(ruta_paquete, ".Rbuildignore"), c(
  "^.*\\.Rproj$",
  "^\\.Rproj\\.user$"
))
cat("  Creado: .Rbuildignore\n")

# --- RESUMEN ---
cat("\n=== LISTO ===\n")
cat("Paquete creado en:", ruta_paquete, "\n\n")
cat("Estructura final:\n")
todos <- list.files(ruta_paquete, recursive = TRUE, all.files = TRUE)
for (f in todos) cat("  ", f, "\n")

cat("\n--- PROXIMOS PASOS ---\n")
cat("1. Abre RStudio y ve a: File > Open Project > ", ruta_paquete, "\n")
cat("2. Instala el paquete localmente:\n")
cat("     devtools::install('", ruta_paquete, "')\n", sep = "")
cat("3. Ejecuta la app:\n")
cat("     library(calc)\n")
cat("     run_app()\n")
cat("4. Para subir a GitHub, inicializa git en la carpeta del paquete:\n")
cat("     usethis::use_git()\n")
cat("     usethis::use_github()\n")
