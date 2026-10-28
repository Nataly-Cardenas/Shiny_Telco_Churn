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
    tab <- c(eda = "eda-tabs", preprocesamiento = "prep-tabs", modelado = "model-tabs", simulador = "sim-tabs")[key]
    div(class = paste("page", key), PAGES[[key]](isolate(d()), if (is.na(tab)) 0 else isolate(as.integer(input[[tab]] %||% 0))))
  })
  tab_value <- function(id) as.integer(input[[id]] %||% 0)

  output$`eda-summary` <- renderUI({ L(); eda_kpis(d()) })
  output$`eda-tabs-body` <- renderUI({ L(); eda_tab(tab_value("eda-tabs"), d(), input) })
  output$`prep-tabs-body` <- renderUI({ L(); prep_tab(tab_value("prep-tabs")) })
  output$`model-tabs-body` <- renderUI({ L(); model_tab(tab_value("model-tabs"), input) })
  output$`sim-tabs-body` <- renderUI({ L(); sim_tab(tab_value("sim-tabs"), input) })

  # ---------------------------------------------------------------- análisis exploratorio
  output$`uni-num-output` <- renderUI({ req(input$`uni-num`); L(); uni_num(input$`uni-num`, d()) })
  output$`uni-cat-output` <- renderUI({ req(input$`uni-cat`); L(); uni_cat(input$`uni-cat`, d()) })
  bi_num_r <- reactive({ req(input$`bi-num`); L(); bi_num(input$`bi-num`, d()) })
  output$`bi-num-output` <- renderUI({ L(); bi_num_r()[[1]] })
  output$`bi-pb-output` <- renderUI({ L(); bi_num_r()[[2]] })
  bi_cat_r <- reactive({ req(input$`bi-cat`); L(); bi_cat(input$`bi-cat`, d()) })
  output$`bi-cat-output` <- renderUI({ L(); bi_cat_r()[[1]] })
  output$`bi-holm-output` <- renderUI({ L(); bi_cat_r()[[2]] })
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

  # ---------------------------------------------------------------- simulador
  output$`sim-output` <- renderUI({
    req(input$`sim-contract`, input$`sim-internet`, input$`sim-payment`); L()
    sim_output(input$`sim-contract`, input$`sim-internet`, input$`sim-payment`, input$`sim-tenure` %||% 4, input$`sim-monthly` %||% 85)
  })
  observeEvent(input$`real-random`, updateSelectizeInput(session, "real-id", selected = sample(TEST_IDS, 1)), ignoreInit = TRUE)
  output$`real-output` <- renderUI({ req(input$`real-id`); L(); real_output(input$`real-id`) })
  output$`threshold-output` <- renderUI({ req(input$`threshold-model`); L(); threshold_output(input$`threshold-model`, input$threshold %||% .5) })

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
