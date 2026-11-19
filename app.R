# Señal perdida: dashboard interactivo del proyecto Telco Customer Churn de Laura Rivera y Natalý Cárdenas.
# Versión en Shiny (R) del dashboard Dash_Telco_Churn_ML (rama dash-original-con-ML, la publicada en Render): cada sección
# corresponde a un capítulo del Jupyter Book y «Modelos finales» evalúa en R los 12 modelos finales (src/modelos.R).
#
# Ejecutar:  shiny::runApp()   (desde esta carpeta)

library(shiny)
library(htmltools)
library(plotly)

DATA <- "data"
for (f in c("i18n.R", "icons.R", "analytics.R", "eda.R", "components.R", "plots.R", "modelos.R", "chatbot.R", "pages_eda.R",
            "pages_model.R", "pages_model_extra.R", "pages_modelos_ml.R", "pages_other.R", "app_ui.R", "app_server.R"))
  source(file.path("src", f), encoding = "UTF-8", local = TRUE)

addResourcePath("artifacts", normalizePath(ART))   # metadata.json de cada modelo (ficha del modelo)

shinyApp(app_ui, app_server)
