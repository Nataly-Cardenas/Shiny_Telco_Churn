# Carga la aplicación sin iniciar el servidor (las rutas son relativas a la raíz del repositorio).
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old <- setwd(root)
withr::defer(setwd(old), teardown_env())
suppressPackageStartupMessages({ library(shiny); library(htmltools); library(plotly) })
DATA <- file.path(root, "data")
for (f in c("i18n.R", "icons.R", "analytics.R", "eda.R", "components.R", "plots.R", "modelos.R", "chatbot.R", "pages_contexto.R", "pages_eda.R",
            "entregable2.R", "entregable2_extra.R", "pages_modelos.R", "app_ui.R", "app_server.R"))
  source(file.path(root, "src", f), encoding = "UTF-8", local = environment())
# Una entrada falsa de Shiny para las funciones que leen valores con isolate(input[[id]]).
ART <- file.path(root, "artifacts")
fake_input <- list()
render <- function(x) htmltools::renderTags(x)$html
