# Señal perdida: dashboard interactivo del proyecto Telco Customer Churn de Laura Rivera y Natalý Cárdenas.
# Versión en Shiny (R) del dashboard Dash_Telco_Churn. Cada sección corresponde a un capítulo del Jupyter Book.
#
# Ejecutar:  shiny::runApp()   (desde esta carpeta)

library(shiny)
library(htmltools)
library(plotly)

DATA <- "data"
for (f in c("i18n.R", "icons.R", "analytics.R", "eda.R", "components.R", "chatbot.R", "pages_eda.R", "pages_model.R", "pages_other.R", "app_ui.R", "app_server.R"))
  source(file.path("src", f), encoding = "UTF-8", local = TRUE)

shinyApp(app_ui, app_server)
