# Señal perdida: dashboard interactivo del proyecto Telco Customer Churn de Laura Rivera y Natalý Cárdenas.
# Versión en Shiny (R) del dashboard Dash_Telco_Churn_ML: tres secciones (Contexto, EDA y Modelos), los 12 modelos finales
# con predicción en vivo y explicación SHAP, asistente e idiomas. Todo en R: los modelos se evalúan con src/modelos.R.
#
# Ejecutar:  shiny::runApp()   (desde esta carpeta)

library(shiny)
library(htmltools)
library(plotly)

DATA <- "data"
for (f in c("i18n.R", "icons.R", "analytics.R", "eda.R", "components.R", "plots.R", "modelos.R", "chatbot.R", "pages_contexto.R", "pages_eda.R",
            "entregable2.R", "entregable2_extra.R", "pages_modelos.R", "app_ui.R", "app_server.R"))
  source(file.path("src", f), encoding = "UTF-8", local = TRUE)

addResourcePath("artifacts", normalizePath(ART))                                      # metadata.json de cada modelo
addResourcePath("evidence", normalizePath(file.path(DATA, "interpretabilidad")))      # figuras de interpretabilidad del experimento

shinyApp(app_ui, app_server)
