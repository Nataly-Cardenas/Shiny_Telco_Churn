# Lógica del servidor: cada salida equivale a un callback del dashboard de Dash.

app_server <- function(input, output, session) {
  lang <- reactive(if (identical(input$lang, "en")) "en" else "es")
  route <- reactive({ k <- input$route %||% "inicio"; if (k %in% names(PAGES)) k else "inicio" })
  tenure <- reactive(input$tenure %||% c(0, 72))
  d <- reactive(cohort(input$split %||% "train", input$contract %||% "Todos", input$internet %||% "Todos", tenure()))
  L <- function() set_lang(lang())   # cada salida fija el idioma antes de construir textos

  # ---------------------------------------------------------------- textos fijos e idioma
  observeEvent(lang(), {
    L()
    session$sendCustomMessage("chrome", chrome_texts())
    for (id in c("split", "contract", "internet")) {
      values <- switch(id, split = c("train", "test", "all"), contract = CONTRACTS, internet = INTERNETS)
      labels <- switch(id, split = split_labels(), list(Todos = T("Todos")))
      lab <- vapply(values, function(v) if (v %in% names(labels)) labels[[v]] else as.character(cat_name(v)), "")
      updateSelectizeInput(session, id, choices = setNames(values, lab), selected = isolate(input[[id]]))
    }
  })
  output$ticker <- renderUI({ L(); ticker() })

  observeEvent(input$reset, {
    updateSelectizeInput(session, "split", selected = "train"); updateSelectizeInput(session, "contract", selected = "Todos")
    updateSelectizeInput(session, "internet", selected = "Todos"); session$sendCustomMessage("slider-set", list(id = "tenure", value = c(0, 72)))
  })

  # ---------------------------------------------------------------- páginas
  output$content <- renderUI({
    L(); key <- route()
    tab <- c(eda = "eda-tabs", preprocesamiento = "prep-tabs", modelado = "model-tabs", modelos_ml = "modelos-tabs")[key]
    div(class = paste("page", key), PAGES[[key]](isolate(d()), if (is.na(tab)) 0 else isolate(as.integer(input[[tab]] %||% 0))))
  })
  tab_value <- function(id) as.integer(input[[id]] %||% 0)

  output$`eda-summary` <- renderUI({ L(); eda_kpis(d()) })
  output$`eda-tabs-body` <- renderUI({ L(); eda_tab(tab_value("eda-tabs"), d(), input) })
  output$`prep-tabs-body` <- renderUI({ L(); prep_tab(tab_value("prep-tabs")) })
  output$`model-tabs-body` <- renderUI({ L(); model_tab(tab_value("model-tabs"), input) })
  output$`modelos-tabs-body` <- renderUI({ L(); modelos_tab(tab_value("modelos-tabs"), input) })

  # ---------------------------------------------------------------- análisis exploratorio
  output$`uni-num-output` <- renderUI({ req(input$`uni-num`); L(); uni_num(input$`uni-num`, d()) })
  output$`uni-cat-output` <- renderUI({ req(input$`uni-cat`); L(); uni_cat(input$`uni-cat`, d()) })
  bi_num_r <- reactive({ req(input$`bi-num`); L(); bi_num(input$`bi-num`, d()) })
  output$`bi-num-output` <- renderUI({ L(); bi_num_r()[[1]] })
  output$`bi-pb-output` <- renderUI({ L(); bi_num_r()[[2]] })
  bi_cat_r <- reactive({ req(input$`bi-cat`); L(); bi_cat(input$`bi-cat`, d()) })
  output$`bi-cat-output` <- renderUI({ L(); bi_cat_r()[[1]] })
  output$`bi-holm-output` <- renderUI({ L(); bi_cat_r()[[2]] })
  output$`free-output` <- renderUI({ req(input$`free-x`, input$`free-y`, input$`free-color`); L(); free_output(input$`free-x`, input$`free-y`, input$`free-color`, d()) })
  output$`explorer-table` <- renderUI({ L(); x <- d(); if (!nrow(x)) empty() else table_ui(x[input$`explorer-cols` %||% "customerID"], page_size = 12) })
  output$export <- downloadHandler(filename = "telco_seleccion.csv", content = function(file) {
    x <- d()[input$`explorer-cols` %||% "customerID"]
    utils::write.csv(x, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })

  # ---------------------------------------------------------------- modelado
  output$`res-output` <- renderUI({ req(input$`res-family`); L(); res_output(input$`res-family`) })
  output$`models-output` <- renderUI({ req(input$`model-family`, input$`model-balance`, input$`model-metric`); L(); model_output(input$`model-family`, input$`model-balance`, input$`model-metric`) })
  output$`opt-output` <- renderUI({ req(input$`opt-family`); L(); opt_output(input$`opt-family`) })
  output$`explain-output` <- renderUI({ req(input$`explain-model`); L(); explain_output(input$`explain-model`) })
  output$`cal-map-output` <- renderUI({ req(input$`cal-family`); L(); cal_map(input$`cal-family`) })
  output$`eff-variants-output` <- renderUI({ req(input$`eff-family`); L(); eff_variants(input$`eff-family`) })
  output$`eff-scale-output` <- renderUI({ req(input$`eff-scale`); L(); eff_scale(input$`eff-scale`) })

  # ---------------------------------------------------------------- modelos finales
  output$`lideres-tabla` <- renderUI({ L(); tabla_lideres(isTRUE(input$`lideres-ablacion`)) })
  output$`diag-salida` <- renderUI({ req(input$`diag-modelo`, input$`diag-eval`); L(); diagnostico_salida(input$`diag-modelo`, input$`diag-eval`) })
  output$`shap-salida` <- renderUI({ req(input$`shap-modelo`); L(); shap_salida(input$`shap-modelo`) })

  # Predicción en vivo: se recalcula al pulsar Predecir, al cambiar de modelo o de umbral, o al cargar un cliente del hold-out.
  entrada <- reactiveVal(cliente_valores(EJEMPLO()))
  formulario <- function() setNames(lapply(ENTRADA, function(c) {
    v <- input[[paste0("in-", c)]]
    if (is.null(v) || (is.character(v) && !nzchar(v))) NULL else if (c == "SeniorCitizen") as.integer(v) else v
  }), ENTRADA)
  observeEvent(input$`pred-ejemplo`, {
    row <- as.integer(input$`pred-ejemplo`); val <- cliente_valores(row)
    if (identical(val, entrada())) return()
    for (c in ENTRADA) {
      id <- paste0("in-", c)
      if (c %in% c("tenure", "MonthlyCharges", "TotalCharges")) updateNumericInput(session, id, value = val[[c]] %||% NA)
      else updateSelectizeInput(session, id, selected = as.character(val[[c]]))
    }
    entrada(val)
  }, ignoreInit = TRUE)
  observeEvent(input$`pred-boton`, entrada(formulario()), ignoreInit = TRUE)
  observeEvent(input$`pred-modelo`, { f <- formulario(); if (!all(vapply(f, is.null, TRUE))) entrada(f) }, ignoreInit = TRUE)
  observeEvent(input$`pred-azar`, updateSelectizeInput(session, "pred-ejemplo", selected = as.character(sample(TEST_IDS, 1))), ignoreInit = TRUE)
  output$`pred-salida` <- renderUI({
    req(input$`pred-modelo`); L()
    prediccion_salida(input$`pred-modelo`, entrada(), input$`pred-umbral-modo` %||% "f1", input$`pred-umbral` %||% .5)
  })

  # ---------------------------------------------------------------- asistente
  log <- reactiveVal(list())
  observeEvent(input$chat_ask, {
    text <- trimws(input$chat_ask$text %||% "")
    if (!nzchar(text)) return()
    L(); reply <- answer(text, log(), lang())
    log(c(log(), list(list(role = "user", text = text), list(role = "assistant", text = reply))))
  })
  observeEvent(input$chat_new, log(list()))
  output$`chat-messages` <- renderUI({ L(); chat_messages(log()) })
  outputOptions(output, "chat-messages", suspendWhenHidden = FALSE)   # el panel se abre y cierra sin que Shiny lo note
  observe({ L(); session$sendCustomMessage("chat-texts", chat_texts(log())) })
}
