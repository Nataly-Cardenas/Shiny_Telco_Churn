# Simulador, 6. Conclusiones y Autoras.

PAY <- unique(RAW$PaymentMethod)
GAUGE_M <- list(l = 34, r = 34, t = 50, b = 10)
SIM_TABS <- c(N("Perfil de riesgo"), N("Clientes reales"), N("Umbral de alerta"))

simulador <- function(d, tab = 0) {
  tagList(page_head("simulador", T("Juega con perfiles, revisa clientes reales y decide dónde poner la alarma de retención.")),
          note(T("Herramienta creada para el dash"), T("Combina los datos de entrenamiento y las predicciones guardadas de los modelos finalistas para explorar casos; no corresponde a un capítulo del libro.")),
          tabs("sim-tabs", T(SIM_TABS), tab))
}

sim_tab <- function(i, input) {
  keep <- function(id, default) isolate(input[[id]]) %||% default
  body <- switch(i + 1,
    tagList(grid(card(cls = "controls-card", tags$h3(T("Arma un cliente")),
                      field(T("Contrato"), dropdown("sim-contract", c("Month-to-month", "One year", "Two year"), keep("sim-contract", "Month-to-month"))),
                      field(T("Internet"), dropdown("sim-internet", c("Fiber optic", "DSL", "No"), keep("sim-internet", "Fiber optic"))),
                      field(T("Método de pago"), dropdown("sim-payment", PAY, keep("sim-payment", "Electronic check"))),
                      field(T("Antigüedad (meses)"), slider("sim-tenure", 0, 72, 1, keep("sim-tenure", 4), c(`0` = "0", `24` = "24", `48` = "48", `72` = "72"))),
                      field(T("Cargo mensual ($)"), slider("sim-monthly", 18, 119, 1, keep("sim-monthly", 85), c(`20` = "20", `60` = "60", `100` = "100")))),
                 uiOutput("sim-output")),
            note(T("¿De dónde sale este riesgo?"), T(paste0("Es la tasa de abandono observada en clientes de entrenamiento con un perfil parecido, no una predicción del modelo. ",
                                                           "Sirve para explorar patrones; para la predicción del modelo usa la pestaña de clientes reales.")))),
    card(cls = "controls-card", div(class = "controls",
         field(T("Cliente del test histórico"), selectizeInput("real-id", NULL, choices = setNames(TEST_IDS, rows(TEST_IDS)$customerID), selected = keep("real-id", TEST_IDS[1]), width = "100%", options = list(dropdownParent = "body"))),
         actionButton("real-random", tagList(icon("shuffle"), T("Cliente al azar")), class = "btn ghost")),
         uiOutput("real-output")),
    tagList(card(cls = "controls-card", div(class = "controls",
                 field(T("Modelo finalista"), dropdown("threshold-model", FINALISTS, keep("threshold-model", FINALISTS[1]), setNames(as.list(T(unname(FINALIST_NAMES[FINALISTS]))), FINALISTS))),
                 field(T("Umbral de alerta"), slider("threshold", .05, .95, .01, keep("threshold", .5), c(`0.1` = "0.10", `0.3` = "0.30", `0.5` = "0.50", `0.7` = "0.70", `0.9` = "0.90"), tooltip = TRUE))),
                 uiOutput("threshold-output")),
            note(T("Decisión de negocio"), T("Bajar el umbral detecta más clientes en riesgo pero dispara más falsas alertas. Elegirlo bien requiere conocer el costo de cada contacto y de cada cliente perdido."))))
  tab_body(body)
}

gauge <- function(value, title, color) {
  plot_ly(type = "indicator", mode = "gauge+number", value = value * 100, number = list(suffix = " %", font = list(size = 34)), title = list(text = title, font = list(size = 13)),
          gauge = list(axis = list(range = c(0, 100), tickcolor = "#8792a8"), bar = list(color = color, thickness = .28), bgcolor = "rgba(0,0,0,0)", borderwidth = 0,
                       steps = list(list(range = c(0, 35), color = "rgba(59,181,122,.14)"), list(range = c(35, 60), color = "rgba(242,169,59,.14)"), list(range = c(60, 100), color = "rgba(232,86,108,.16)"))))
}

level <- function(p) if (p >= .6) list("alto", HIGH) else if (p >= .35) list("medio", MID) else list("bajo", LOW)

signal <- function(p) {
  lit <- max(1, round((1 - p) * 5))
  div(class = paste("signal", level(p)[[1]]), lapply(0:4, function(i) tags$b(class = if (i < lit) "lit" else "", style = sprintf("height:%dpx", 12 + i * 9))))
}

sim_output <- function(contract, internet, payment, tenure, monthly) {
  r_ <- similar_customers(contract, internet, payment, tenure, monthly); s <- r_$s; w <- r_$w
  if (!nrow(s)) return(card(note(T("Sin clientes parecidos"), T("Prueba con otra combinación de contrato e internet."))))
  r <- mean(s$target); lv <- level(r)
  window <- paste0(fmt(T("antigüedad ±{t} meses · cargo ±${m}"), t = w$tenure, m = w$monthly), if (w$payment) T(" · mismo método de pago") else "")
  risk <- c(alto = T("Riesgo alto"), medio = T("Riesgo medio"), bajo = T("Riesgo bajo"))[[lv[[1]]]]
  card(div(class = "sim-top",
           div(class = "sim-sig", signal(r), tags$strong(style = paste0("color:", lv[[2]]), risk), tags$small(fmt(T("{n} clientes parecidos"), n = f0(nrow(s))))),
           graph(gauge(r, T("Abandono observado en clientes similares"), lv[[2]]), 260, FALSE, GAUGE_M)),
       tags$p(class = "insight", T("Criterio de similitud: mismo contrato e internet, "), window, "."),
       div(class = "pc-pills", tags$span(class = "pill coral", fmt(T("{n} se fueron"), n = sum(s$target))), tags$span(class = "pill cyan", fmt(T("{n} se quedaron"), n = sum(1 - s$target)))))
}

real_output <- function(row) {
  row <- as.integer(row); c <- rows(row); sc <- test_scores(); sc <- sc[sc$row_id == row, ]
  facts <- list(c(T("Contrato"), cat_name(c$Contract)), c("Internet", cat_name(c$InternetService)), c(T("Pago"), cat_name(c$PaymentMethod)), c(T("Antigüedad"), paste(c$tenure, T("meses"))),
                c(T("Cargo mensual"), sprintf("$%.2f", c$MonthlyCharges)), c(T("Soporte técnico"), cat_name(c$TechSupport)), c(T("Seguridad en línea"), cat_name(c$OnlineSecurity)),
                c(T("Adulto mayor"), cat_name(as.integer(c$SeniorCitizen))))
  gauges <- lapply(FINALISTS, function(e) graph(gauge(sc[[e]], T(FINALIST_NAMES[[e]]), level(sc[[e]])[[2]]), 220, FALSE, GAUGE_M))
  verdict <- div(class = paste("truth", if (c$target == 1) "coral" else "cyan"), icon(if (c$target == 1) "x" else "check", "sm"), T("En la realidad este cliente "),
                 tags$b(if (c$target == 1) T("abandonó") else T("permaneció")))
  div(div(class = "real",
          div(class = "mini-phone", div(class = "pc-row", tags$small(T("Cliente")), tags$strong(c$customerID)),
              div(class = "facts", lapply(facts, function(f) div(tags$small(f[1]), tags$strong(f[2])))), verdict),
          div(class = "gauges", gauges)),
      tags$p(class = "insight", T("Probabilidades nativas guardadas de los modelos finalistas, sin recalibrar. El umbral por defecto es 0.5.")))
}

threshold_output <- function(experiment, threshold) {
  p <- predictions(experiment); tm <- threshold_metrics(p, threshold); cm <- tm$cm; m <- tm$m
  tn <- cm[1, 1]; fp <- cm[1, 2]; fn <- cm[2, 1]; tp <- cm[2, 2]; total <- sum(cm)
  # Redondeo como Python (al par más cercano) para que la cuadrícula sume igual que en el dash original.
  quota <- c(tp = tp, fn = fn, fp = fp, tn = tn) / total * 100; quota <- ifelse(abs(quota %% 1 - .5) < 1e-9, 2 * round(quota / 2), round(quota))
  names(quota) <- c("tp", "fn", "fp", "tn"); quota[["tn"]] <- quota[["tn"]] + 100 - sum(quota)
  legend <- list(c("tp", T("Abandonos detectados"), tp), c("fn", T("Abandonos que se escapan"), fn), c("fp", T("Falsas alertas"), fp), c("tn", T("Clientes estables bien leídos"), tn))
  names_ <- setNames(vapply(legend, `[`, "", 2), vapply(legend, `[`, "", 1))
  waffle <- unlist(lapply(c("tp", "fn", "fp", "tn"), function(k) rep(sprintf('<span class="w %s" title="%s"></span>', k, htmlEscape(names_[[k]], TRUE)), max(0, quota[[k]]))))
  rc <- roc_curve(p$y_true, p$score)
  roc <- plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = rc$fpr, y = rc$tpr, line = list(color = BASE), name = "ROC", fill = "tozeroy", fillcolor = "rgba(46,134,171,.10)") %>%
    add_trace(type = "scatter", mode = "lines+markers", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), marker = list(color = GREY), name = T("Azar")) %>%
    add_trace(type = "scatter", mode = "markers", x = fp / (fp + tn), y = tp / (tp + fn), marker = list(size = 14, color = HOT, line = list(color = "white", width = 2)), name = T("Umbral actual")) %>%
    layout(xaxis = list(title = T("Tasa de falsas alertas")), yaxis = list(title = T("Tasa de detección")))
  cards <- lapply(names(m), function(k) if (k == "Alertas") kpi(T("ALERTAS"), m[[k]], T("test histórico")) else kpi(toupper(k), round(m[[k]] * 100, 1), T("test histórico"), "", 1, " %"))
  div(do.call(kpis, cards),
      grid(card(tags$h3(class = "chart-title", T("100 clientes del test")), div(class = "waffle", HTML(paste(waffle, collapse = ""))),
                div(class = "legend", lapply(legend, function(l) div(class = "leg", tags$span(class = paste("w", l[1])), paste0(l[2], ": "), tags$b(f0(as.numeric(l[3]))))))),
           chart(T("Curva ROC y punto de operación"), roc, fmt(T("AUC = {a}. El punto resaltado se mueve con el umbral."), a = sprintf("%.3f", auc(p$y_true, p$score))), 360)))
}

# ================================================================ 6. CONCLUSIONES
REPRO <- list(c("file", N("Versiones exactas"), N("requirements.txt fija cada librería usada en el entrenamiento.")),
              c("shield", N("Verificación automática"), N("verificar_reproduccion.py compara versiones, huellas SHA-256 de los datos y los artefactos de las 1,680 unidades.")),
              c("layers", N("Resultados versionados"), N("Pipelines, predicciones, checkpoints y semillas de cada unidad quedan guardados; el libro se construye sin reentrenar.")),
              c("code", N("Código del experimento"), N("Un apéndice reproduce las 32 celdas exactas que entrenaron las 112 configuraciones.")))

conclusiones <- function(d) {
  finds <- list(c("layers", N("El perfil de cuenta delata el riesgo"), N("Contrato, antigüedad y servicio de internet concentran la señal, en el EDA y en SHAP.")),
                c("link", N("Empate en la cima"), N("Random Forest y XGBoost con SMOTE (F1 0.638) y la regresión logística (0.634) no se separan estadísticamente.")),
                c("scale", N("Balancear importa más que buscar"), N("SMOTE tiene efecto grande en 5 de 7 familias (δ de Cliff ≥ 0.68); cambiar de optimizador solo ayuda con presupuestos bajos.")),
                c("gauge", N("Probabilidades más honestas"), N("Platt mejoró Brier y ECE en 80 de 96 configuraciones y la isotónica en 84: isotónica para árboles y GaussianNB, Platt para SVM y KNN.")),
                c("hourglass", N("Buscar puede costar menos"), N("Successive Halving igualó el F1 de una búsqueda completa en Random Forest y SVM con 3.7× y 7.1× menos tiempo; en XGBoost no conviene.")),
                c("flame", N("El costo está en el núcleo del algoritmo"), N("LinearSVC entrena 36× y predice unas 900× más rápido que SVC RBF con 0.002 menos de F1; hist acelera 2.8× a XGBoost.")))
  limits <- c(N("Un único conjunto de datos: los folds no son bloques independientes."), N("El test ya se había usado: es evidencia suplementaria."),
              N("Los datos son una foto: no garantizan anticipar el abandono futuro."), N("Sin costos de intervención, el F1 no se traduce en dinero."))
  steps <- list(c(N("Validación temporal"), N("Evaluar en clientes futuros.")), c(N("Costos explícitos"), N("Elegir el umbral con el costo de cada error.")),
                c(N("Modelo propuesto"), N("Superar a las referencias en desempeño y tiempo.")))
  tagList(page_head("conclusiones", T("Qué aprendimos, qué sigue siendo incierto y qué falta para llevarlo a una campaña de retención.")),
          subhead(T("Hallazgos principales")),
          div(class = "findings", lapply(finds, function(f) div(class = "finding", tags$span(class = "drv-icon", icon(f[1])), tags$h3(T(f[2])), tags$p(T(f[3]))))),
          grid(card(eyebrow(T("LIMITACIONES")), tags$ul(lapply(limits, function(x) tags$li(T(x))))),
               card(eyebrow(T("PRÓXIMOS PASOS")), div(lapply(seq_along(steps), function(k) div(class = "next", tags$span(class = "obj-n", sprintf("0%d", k)), div(tags$h4(T(steps[[k]][1])), tags$p(T(steps[[k]][2])))))))),
          card(eyebrow(T("REPRODUCIBILIDAD")), tags$h3(T("Todo lo que se reporta se puede verificar")),
               div(class = "repro", lapply(REPRO, function(r) div(class = "station", tags$span(class = "st-icon", icon(r[1])), tags$h4(T(r[2])), tags$p(T(r[3]))))),
               tags$a(class = "chip-link", href = paste0(BOOK, "CODIGO_EXPERIMENTO.html"), target = "_blank", icon("code", "sm"), T("Ver el código del experimento en el Jupyter Book"), icon("arrow", "sm"))),
          card(cls = "glow", div(class = "paper-card",
               div(eyebrow(T("PAPER DEL PROYECTO")), tags$h2(T("Modelo de clasificación binaria para la predicción de abandono de clientes")),
                   tags$p(T("Metodología completa, tablas, figuras y referencias en formato Springer Nature."))),
               div(class = "cover-cta", tags$a(class = "btn primary", href = paste0(BOOK, "paper.html"), target = "_blank", icon("file"), T("Leer el paper")),
                   tags$a(class = "btn ghost", href = paste0(BOOK, "paper.pdf"), target = "_blank", icon("download"), "PDF")))))
}

# ================================================================ AUTORAS
autoras <- function(d) {
  people <- list(c("laura.jpg", "Laura S. Rivera Figueredo", "sriveral@uninorte.edu.co", "https://github.com/lauraformore", "https://www.linkedin.com/in/laura-s-rivera-figueredo-862451315/"),
                 c("nataly.jpg", "Natalý A. Cárdenas Izaquita", "nizaquita@uninorte.edu.co", "https://github.com/Nataly-Cardenas", "https://www.linkedin.com/in/n%C3%A1taly-c%C3%A1rdenas-iz%C3%A1quita"))
  res <- list(c("code", T("Código y notebooks"), T("Repositorio Telco_Churn_JBook"), REPO), c("sliders", T("Este dashboard"), T("Repositorio Shiny_Telco_Churn"), SHINY_REPO),
              c("table", T("Conjunto de datos"), "Kaggle · Telco Customer Churn (IBM)", "https://www.kaggle.com/datasets/blastchar/telco-customer-churn"), c("file", "Paper", T("Versión web y PDF"), paste0(BOOK, "paper.html")))
  tagList(page_head("autoras", T("El equipo detrás del proyecto y los recursos para profundizar.")),
          div(class = "authors", lapply(people, function(p) div(class = "author",
              div(class = "avatar", tags$img(src = paste0("img/", p[1]), alt = fmt(T("Foto de {n}"), n = p[2]))), tags$h2(p[2]),
              tags$p(class = "muted", T("Ciencia de Datos · Universidad del Norte")),
              div(class = "author-links", tags$a(class = "chip-link linkedin", href = p[5], target = "_blank", icon("linkedin", "sm"), "LinkedIn"),
                  tags$a(class = "chip-link", href = p[4], target = "_blank", icon("code", "sm"), "GitHub"),
                  tags$a(class = "chip-link", href = paste0("mailto:", p[3]), icon("mail", "sm"), p[3]))))),
          tags$p(class = "center muted", T("Departamento de Matemáticas, Física y Ciencia de Datos · Universidad del Norte · Barranquilla, Colombia")),
          tags$a(class = "book-cta", href = BOOK, target = "_blank", tags$span(class = "book-icon", icon("book")),
                 div(eyebrow("JUPYTER BOOK"), tags$h2(T("Lee el proyecto completo")), tags$p(T("Introducción, marco teórico, EDA, preprocesamiento, modelado y paper."))),
                 tags$span(class = "book-arrow", icon("arrow"))),
          do.call(grid, c(lapply(res, function(r) tags$a(class = "res", href = r[4], target = "_blank", tags$h4(icon(r[1]), r[2]), tags$p(r[3]))), cols = 4)))
}

PAGES <- list(inicio = function(d, tab) cover(), introduccion = function(d, tab) introduccion(d), eda = eda_page, preprocesamiento = preprocesamiento,
              modelado = modelado, simulador = simulador, conclusiones = function(d, tab) conclusiones(d), autoras = function(d, tab) autoras(d))
