# Sección 2 · EDA: filtros globales, distribución del objetivo, univariado, bivariado, explorador bivariado,
# multivariado, faltantes, resumen, preparación de datos y explorador con descarga (pages/eda.py del dash).

NAV <- list(c("contexto", N("Contexto"), "signal"), c("eda", N("EDA"), "chart"), c("modelos", N("Modelos"), "trophy"))
TITLES <- setNames(lapply(NAV, `[`, 2), vapply(NAV, `[`, "", 1))
DATA_PAGES <- "eda"
EXPLORABLES <- c(NUMERIC, "SeniorCitizen")

# ================================================================ 2. ANÁLISIS EXPLORATORIO
EDA_TABS <- c(N("1 · ETL y partición"), N("2 · Univariado"), N("3 · Bivariado"), N("4 · Explorador bivariado"), N("5 · Multivariado"), N("6 · Valores faltantes"),
              N("7 · Resumen"), N("8 · Preparación de datos"), N("9 · Explorador de datos"))

eda_page <- function(tab = 0) {
  tagList(page_head(T("Análisis exploratorio"), T("Del archivo original a los patrones de abandono. Todo el análisis se hace sobre el conjunto de entrenamiento; los filtros superiores recalculan las gráficas y tablas."), "EDA_"),
          uiOutput("eda-summary"),
          tabs("eda-tabs", T(EDA_TABS), tab))
}

eda_kpis <- function(d) {
  if (!nrow(d)) return(empty())
  tagList(scope(d),
          kpis(kpi(T("CLIENTES"), nrow(d), T("en la selección")), kpi(T("ABANDONO"), round(mean(d$target) * 100, 2), fmt(T("{n} clientes"), n = f0(sum(d$target))), "hot", 2, " %"),
               kpi(T("ANTIGÜEDAD MEDIA"), round(mean(d$tenure), 1), T("meses"), "", 1), kpi(T("CARGO MENSUAL MEDIO"), round(mean(d$MonthlyCharges), 2), T("dólares"), "", 2, "", "$")),
          note(T("Sin fechas en los datos"), T("El conjunto es una fotografía de corte transversal: no hay una variable de fecha para filtrar por periodo. Los filtros usan partición, contrato, internet y antigüedad.")))
}

eda_tab <- function(i, d, input) {
  if (!nrow(d)) return(tab_body(empty()))
  keep <- function(id, default) isolate(input[[id]]) %||% default
  num_opts <- setNames(as.list(NUMERIC), NUMERIC); cat_opts <- setNames(as.list(CATEGORICAL), CATEGORICAL)
  body <- switch(i + 1,
    tagList(kpis(kpi(T("FILAS ORIGINALES"), 7043, T("21 columnas")), kpi(T("ENTRENAMIENTO"), 5634, T("80 % · aquí se hace el EDA")), kpi(T("TEST"), 1409, T("20 % · reservado")),
                 kpi(T("DUPLICADOS"), sum(duplicated(d[RAW_COLS])), T("filas repetidas en la selección"))),
            grid(chart(T("División train / test estratificada"), split_figure(), T("El test se separa primero y conserva la proporción de abandono (26.5 %) en ambas particiones."), 320),
                 card(tags$h3(class = "chart-title", T("Estructura original")), table_ui(tt(structure_table()), page_size = 8),
                      tags$p(class = "insight", T("TotalCharges llega como texto: los espacios en blanco se convierten en valores faltantes y la columna pasa a numérica."))))),
    tagList(grid(chart(T("Variable objetivo"), target_figure(d), T("Clientes que permanecen y que abandonan: el desbalance obliga a usar métricas de la clase positiva."), 340, FALSE),
                 card(tags$h3(class = "chart-title", T("Estadísticas descriptivas")), table_ui(tt(numeric_summary(d))))),
            subhead(T("Variables numéricas")),
            card(cls = "controls-card", field(T("Variable"), dropdown("uni-num", NUMERIC, keep("uni-num", "tenure"), num_opts)), uiOutput("uni-num-output")),
            subhead(T("Variables categóricas")),
            card(cls = "controls-card", field(T("Variable"), dropdown("uni-cat", c("__all__", CATEGORICAL), keep("uni-cat", "__all__"), c(list(`__all__` = T("Todas las variables")), cat_opts))), uiOutput("uni-cat-output")),
            subhead(T("Outliers y sesgos")),
            card(table_ui(tt(outliers(d))), tags$p(class = "insight", T("Con el criterio IQR ninguna variable numérica presenta valores atípicos; TotalCharges tiene asimetría positiva. No se eliminan observaciones.")))),
    tagList(subhead(T("Variable objetivo vs variables numéricas")),
            card(cls = "controls-card", field(T("Variable"), dropdown("bi-num", NUMERIC, keep("bi-num", "tenure"), num_opts)), uiOutput("bi-num-output")),
            uiOutput("bi-pb-output"),
            subhead(T("Variable objetivo vs variables categóricas")),
            card(cls = "controls-card", field(T("Variable"), dropdown("bi-cat", CATEGORICAL, keep("bi-cat", "Contract"), cat_opts)), uiOutput("bi-cat-output")),
            uiOutput("bi-holm-output")),
    card(cls = "controls-card", div(class = "controls",
         field(T("Eje X"), dropdown("xy-x", c(EXPLORABLES, CATEGORICAL), keep("xy-x", "tenure"), setNames(as.list(c(EXPLORABLES, CATEGORICAL)), c(EXPLORABLES, CATEGORICAL)))),
         field(T("Eje Y"), dropdown("xy-y", EXPLORABLES, keep("xy-y", "MonthlyCharges"), setNames(as.list(EXPLORABLES), EXPLORABLES))),
         field(T("Color"), dropdown("xy-color", c("Churn", CATEGORICAL), keep("xy-color", "Churn"), c(list(Churn = T("Abandono")), cat_opts)))),
         uiOutput("xy-output")),
    multi_output(d),
    missing_output(d),
    eda_summary(),
    preparacion(),
    card(cls = "controls-card", field(T("Columnas visibles"), dropdown("explorer-cols", RAW_COLS, keep("explorer-cols", c("customerID", "Contract", "InternetService", "tenure", "MonthlyCharges", "Churn")),
                                                                  setNames(as.list(RAW_COLS), RAW_COLS), multi = TRUE)),
         uiOutput("explorer-table"),
         div(class = "export-row", downloadLink("export", class = "btn primary", icon("download"), T("Descargar CSV")),
             tags$p(class = "muted", T("Descarga un solo archivo con la selección actual de los filtros superiores y las columnas elegidas.")))))
  tab_body(body)
}

split_figure <- function() {
  tr <- rows(TRAIN_IDS); te <- rows(TEST_IDS); x <- c(T("Entrenamiento"), "Test")
  plot_ly() %>%
    add_trace(type = "bar", x = x, y = c(sum(tr$target == 0), sum(te$target == 0)), name = T("Permanece"), marker = list(color = BASE),
              text = c(sum(tr$target == 0), sum(te$target == 0)), textposition = "inside") %>%
    add_trace(type = "bar", x = x, y = c(sum(tr$target), sum(te$target)), name = T("Abandona"), marker = list(color = HOT),
              text = c(sum(tr$target), sum(te$target)), textposition = "inside") %>%
    layout(barmode = "relative", xaxis = cat_axis(x, title = ""), yaxis = list(title = T("Clientes")), legend = list(title = list(text = "Churn")))
}

structure_table <- function() {
  raw <- utils::read.csv(file.path(DATA, "WA_Fn-UseC_-Telco-Customer-Churn.csv"), nrows = 5, stringsAsFactors = FALSE)
  dtype <- function(x) if (is.integer(x)) "int64" else if (is.numeric(x)) "float64" else "object"
  df(Variable = RAW_COLS, `Tipo original` = vapply(raw[RAW_COLS], dtype, ""), `Valores únicos` = vapply(RAW[RAW_COLS], function(x) length(unique(x[!is.na(x)])), 1L))
}

target_figure <- function(d) {
  t <- target_summary(d); lab <- churn_label(t$Churn); cols <- outcome_colors()
  p <- plot_ly()
  for (k in seq_len(nrow(t))) p <- add_trace(p, type = "bar", x = lab[k], y = t$Clientes[k], name = lab[k], marker = list(color = cols[[lab[k]]]),
                                             text = sprintf("%s · %.2f%%", f0(t$Clientes[k]), t$`Porcentaje (%)`[k]), textposition = "outside")
  layout(p, xaxis = cat_axis(lab, title = ""), yaxis = list(title = T("Clientes"), range = c(0, max(t$Clientes) * 1.18)))
}

uni_num <- function(var, d) {
  s <- d[[var]][!is.na(d[[var]])]
  if (!length(s)) return(empty())
  h <- plot_ly() %>% add_trace(type = "histogram", x = s, nbinsx = 30, marker = list(color = BASE), name = T("Frecuencia")) %>%
    layout(xaxis = list(title = var), yaxis = list(title = T("Clientes")))
  b <- plot_ly() %>% add_trace(type = "box", y = s, name = var, marker = list(color = BASE), line = list(color = BASE), fillcolor = "rgba(46,134,171,.5)",
                               boxpoints = "outliers", quartilemethod = "linear")
  f <- subplots(list(h, b), c(T("Distribución"), T("Rango y cuartiles")), widths = c(.7, .3))
  f$x$layout$shapes <- list(vline(mean(s), HOT, "dash"))
  f$x$layout$annotations <- c(f$x$layout$annotations, list(list(text = fmt(T("media {m}"), m = sprintf("%.1f", mean(s))), x = mean(s), xref = "x", y = 1, yref = "y domain",
                                                                 xanchor = "left", yanchor = "top", showarrow = FALSE)))
  div(graph(f, 380, FALSE),
      tags$p(class = "insight", fmt(T("{n} valores · media {m} · mediana {md} · asimetría {s}."), n = f0(length(s)), m = sprintf("%.2f", mean(s)), md = sprintf("%.2f", stats::median(s)), s = sprintf("%.2f", skew(s)))))
}

uni_cat <- function(var, d) {
  if (!nrow(d)) return(empty())
  variables <- if (var == "__all__") CATEGORICAL else var
  cols <- if (length(variables) > 1) 4 else 1; nr <- ceiling(length(variables) / cols)
  plots <- lapply(variables, function(c) {
    v <- sort(table(as.character(cat_name(d[[c]])))); pc <- 100 * v / sum(v)
    plot_ly() %>% add_trace(type = "bar", x = as.vector(v), y = names(v), orientation = "h", marker = list(color = BASE), text = sprintf("%.1f%%", pc),
                            textposition = "outside", cliponaxis = FALSE, hovertemplate = paste0("%{y}<br>", T("Clientes"), ": %{x}<extra></extra>")) %>%
      layout(yaxis = cat_axis(names(v)))
  })
  f <- subplots(plots, variables, nrows = nr, hspace = .12, vspace = if (nr > 1) .08 else .1)
  h <- if (length(variables) == 1) 320 else 230 * nr
  insight <- if (length(variables) > 1) T("Distribución de todas las variables categóricas en la selección.") else fmt(T("Frecuencia de cada categoría de {v}."), v = var)
  div(graph(f, h, FALSE), tags$p(class = "insight", insight))
}

bi_num <- function(var, d) {
  if (!nrow(d) || length(unique(d$Churn)) < 2) return(list(empty(), NULL))
  lab <- churn_label(d$Churn); order <- T(c("Permanece", "Abandona")); cols <- outcome_colors()
  f <- plot_ly()
  ok <- !is.na(d[[var]])
  for (k in order) f <- add_trace(f, type = "box", x = lab[lab == k & ok], y = d[[var]][lab == k & ok], name = k, marker = list(color = cols[[k]]), line = list(color = cols[[k]]), fillcolor = if (k == order[1]) "rgba(46,134,171,.5)" else "rgba(228,87,46,.5)",
                                  boxpoints = "outliers", quartilemethod = "linear")
  f <- layout(f, xaxis = cat_axis(order, title = ""), yaxis = list(title = var))
  h <- holm_family(d); num <- h$num
  g <- sort(unique(lab))
  by <- do.call(rbind, lapply(g, function(k) { x <- d[[var]][lab == k]; x <- x[!is.na(x)]; df(Estado = k, N = length(x), Media = mean(x), DS = stats::sd(x), Mediana = stats::median(x)) }))
  test <- num[num$Variable == var, names(num) != "Variable", drop = FALSE]
  rk <- num[order(abs(num$`r punto-biserial`)), ]
  fp <- plot_ly() %>% add_trace(type = "bar", x = rk$`r punto-biserial`, y = rk$Variable, orientation = "h", marker = list(color = highlight(abs(rk$`r punto-biserial`))),
                                text = sprintf("%+.3f", rk$`r punto-biserial`), textposition = "outside") %>%
    layout(shapes = list(vline(0)), xaxis = list(title = T("r punto-biserial con el abandono"), range = c(-.45, .3)), yaxis = cat_axis(rk$Variable))
  left <- div(grid(graph(f, 380, FALSE), div(tags$h3(class = "chart-title", T("Resumen por clase")), table_ui(tt(by)), tags$h3(class = "chart-title", T("Prueba estadística")), table_ui(tt(test)))),
              tags$p(class = "insight", T("Mann–Whitney U para tenure y TotalCharges, t de Welch para MonthlyCharges; p corregido con Holm sobre los 19 contrastes.")))
  list(left, chart(T("Asociación punto-biserial"), fp, T("Correlación de cada variable numérica con el abandono; tenure (resaltada) es la más fuerte."), 280, FALSE))
}

bi_cat <- function(var, d) {
  if (!nrow(d) || length(unique(d$Churn)) < 2) return(list(empty(), NULL))
  ct <- crosstab(d[[var]], d$Churn, c("No", "Yes")); cats <- as.character(cat_name(if (is.numeric(d[[var]])) as.integer(rownames(ct)) else rownames(ct)))
  cols <- outcome_colors(); f <- plot_ly()
  for (k in c("No", "Yes")) f <- add_trace(f, type = "bar", x = ct[, k], y = cats, orientation = "h", name = churn_label(k), marker = list(color = cols[[churn_label(k)]]),
                                           text = ct[, k], textposition = "auto")
  f <- layout(f, barmode = "group", xaxis = list(title = T("Clientes (frecuencia absoluta)")), yaxis = cat_axis(cats, title = ""))
  rate <- setNames(100 * ct[, "Yes"] / rowSums(ct), cats)
  h <- holm_family(d); catt <- h$cat
  row <- catt[catt$Variable == var, names(catt) != "Variable", drop = FALSE]
  left <- div(grid(graph(f, max(300, 90 + 70 * nrow(ct)), TRUE),
                   div(tags$h3(class = "chart-title", T("Tasa de abandono por categoría")),
                       div(class = "rates", lapply(names(rate), function(k) div(class = "rate-row", tags$span(k),
                           div(class = "rate-bar", tags$i(class = if (rate[[k]] == max(rate)) "hot" else "", style = sprintf("width:%.1f%%", rate[[k]]))), tags$b(sprintf("%.1f%%", rate[[k]]))))),
                       tags$h3(class = "chart-title", T("Chi-cuadrado")), table_ui(tt(row)))))
  v <- catt[order(catt$`V de Cramér`), ]
  fv <- plot_ly() %>% add_trace(type = "bar", x = v$`V de Cramér`, y = v$Variable, orientation = "h", marker = list(color = highlight(v$`V de Cramér`)),
                                text = sprintf("%.3f", v$`V de Cramér`), textposition = "outside", customdata = v$`p Holm`,
                                hovertemplate = "%{y}<br>V = %{x:.3f}<br>p Holm = %{customdata:.2e}<extra></extra>") %>%
    layout(xaxis = list(title = T("V de Cramér con el abandono"), range = c(0, .5)), yaxis = cat_axis(v$Variable))
  holm_t <- h$fam[c("Variable", "p", "p Holm", "Rechaza H0")]; names(holm_t)[2] <- "p original"
  right <- div(chart(T("Fuerza de la asociación de cada variable categórica"), fv, T("Contrato (resaltada) es la más asociada; gender y PhoneService no rechazan H0 tras Holm."), 520, FALSE),
               details(tagList(icon("table"), T("Control de comparaciones múltiples (Holm)")), table_ui(tt(holm_t), page_size = 19)))
  list(left, right)
}

multi_output <- function(d) {
  if (nrow(d) < 30) return(empty())
  qplots <- lapply(seq_along(NUMERIC), function(j) {
    q <- qq(d[[NUMERIC[j]]]); k <- unique(as.integer(seq(0, length(q$theo) - 1, length.out = min(600, length(q$theo))))) + 1
    r <- range(q$theo)
    plot_ly() %>% add_trace(type = "scatter", mode = "markers", x = q$theo[k], y = q$obs[k], marker = list(size = 4, color = BASE, opacity = .55), showlegend = FALSE) %>%
      add_trace(type = "scatter", mode = "lines", x = r, y = q$inter + q$slope * r, line = list(color = HOT, width = 2), showlegend = FALSE) %>%
      layout(xaxis = list(title = T("Cuantiles teóricos")), yaxis = list(title = if (j == 1) T("Cuantiles observados") else ""))
  })
  fq <- subplots(qplots, NUMERIC, hspace = .08)
  corr <- stats::cor(d[NUMERIC], method = "spearman", use = "pairwise.complete.obs")
  fh <- heatmap(corr, NUMERIC, NUMERIC, DIVERGING, -1, 1, digits = 3) %>% layout(yaxis = list(autorange = "reversed"))
  set.seed(42); sp <- d[sample(nrow(d), min(1500, nrow(d))), ]; sp$Estado <- churn_label(sp$Churn)
  cols <- outcome_colors(); order <- T(c("Permanece", "Abandona"))
  fs <- plot_ly()
  for (k in order) { s <- sp[sp$Estado == k, ]
    fs <- add_trace(fs, type = "splom", name = k, dimensions = lapply(NUMERIC, function(c) list(label = c, values = s[[c]])),
                    marker = list(color = cols[[k]], size = 4, opacity = .35), showupperhalf = FALSE, diagonal = list(visible = FALSE)) }
  kplots <- lapply(1:2, function(j) {
    c <- c("MonthlyCharges", "TotalCharges")[j]; k <- kde(d, c); p <- plot_ly()
    for (cl in names(k$dens)) p <- add_trace(p, type = "scatter", mode = "lines", x = k$grid, y = k$dens[[cl]], name = churn_label(cl), legendgroup = cl,
                                            line = list(color = if (cl == "No") BASE else HOT, width = 2.5), fill = "tozeroy", fillcolor = if (cl == "No") "rgba(46,134,171,.5)" else "rgba(228,87,46,.5)", showlegend = j == 1)
    layout(p, yaxis = list(title = if (j == 1) T("Densidad estimada") else ""))
  })
  fk <- subplots(kplots, c("MonthlyCharges", "TotalCharges"))
  contracts <- c("Month-to-month", "One year", "Two year")
  cplots <- lapply(seq_along(contracts), function(j) {
    p <- plot_ly()
    for (k in order) { s <- sp[sp$Contract == contracts[j] & sp$Estado == k, ]
      p <- add_trace(p, type = "scattergl", mode = "markers", x = s$tenure, y = s$MonthlyCharges, name = k, legendgroup = k, showlegend = j == 1,
                     marker = list(color = cols[[k]], size = 4, opacity = .4)) }
    layout(p, xaxis = list(title = "tenure"), yaxis = list(title = if (j == 1) "MonthlyCharges" else ""))
  })
  fc <- subplots(cplots, cat_name(contracts), hspace = .02, share_y = TRUE, title_size = 13.5)   # títulos de faceta como en plotly express
  tagList(chart(T("Prueba de normalidad · gráficos Q-Q"), fq, T("Si los puntos siguieran la línea resaltada la variable sería normal; ninguna lo es, por eso se usa Spearman."), 360, FALSE),
          card(table_ui(tt(normality(d)))),
          grid(chart(T("Matriz de correlaciones de Spearman"), fh, T("tenure y TotalCharges comparten mucha información (ρ ≈ 0.89)."), 380, FALSE),
               card(tags$h3(class = "chart-title", T("Asociación entre variables categóricas")), table_ui(tt(categorical_pairs(d))),
                    tags$h3(class = "chart-title", T("Identificación de posibles redundancias")), table_ui(tt(redundancy(d))))),
          subhead(T("Visualizaciones avanzadas")),
          chart(T("Matriz de dispersión por clase"), fs, T("Muestra de hasta 1,500 clientes. Los que abandonan se concentran en baja antigüedad y cargos mensuales altos."), 560),
          chart(T("Densidad por clase"), fk, T("Estimación de densidad de los cargos para quienes permanecen y quienes abandonan."), 360),
          chart(T("Antigüedad y cargo mensual por contrato"), fc, T("El abandono se concentra en contratos mes a mes con poca antigüedad."), 400))
}

missing_output <- function(d) {
  if (!nrow(d)) return(empty())
  mt <- missing_table(d); miss <- is.na(d$TotalCharges)
  z <- matrix(sort(as.integer(miss), decreasing = TRUE), nrow = 1)
  fp <- plot_ly(type = "heatmap", z = z, x = seq_len(ncol(z)) - 1, y = "TotalCharges", colorscale = list(list(0, "rgba(46,134,171,.15)"), list(1, HOT)), zmin = 0, zmax = 1,
                showscale = FALSE, hovertemplate = paste0(T("Posición ordenada en la selección"), ": %{x}<br>", T("Ausente"), ": %{z}<extra></extra>")) %>%
    layout(xaxis = list(title = T("Posición ordenada en la selección")))
  absent <- d[miss, c("customerID", "tenure", "MonthlyCharges", "TotalCharges", "Churn")]
  nr <- table(rowSums(is.na(d[RAW_COLS])))
  rws <- df(`Nulos por fila` = as.integer(names(nr)), Filas = as.integer(nr))
  tagList(kpis(kpi(T("VALORES FALTANTES"), if (nrow(mt)) sum(mt$Nulos) else 0, T("solo en TotalCharges")), kpi(T("PORCENTAJE"), round(100 * mean(miss), 2), T("de la selección"), "", 2, " %"),
               kpi(T("CON TENURE = 0"), sum(d$tenure[miss] == 0), T("clientes recién dados de alta")), kpi(T("FILAS AFECTADAS"), sum(miss), T("un faltante cada una"))),
          chart(T("Patrón de valores faltantes"), fp, T("Cada línea vertical es un cliente; los faltantes (resaltados) son pocos y se concentran en clientes con tenure 0."), 220, FALSE),
          grid(card(tags$h3(class = "chart-title", T("Clientes con TotalCharges ausente")), table_ui(tt(absent))),
               card(tags$h3(class = "chart-title", T("Relación con la variable objetivo y evaluación de MCAR")), table_ui(tt(mcar_tests(d))), table_ui(tt(rws)),
                    tags$p(class = "insight", T("La ausencia no se asocia con el abandono, pero coincide por completo con tenure = 0: no es MCAR, es estructural, y se imputa con 0.")))))
}

eda_summary <- function() {
  t <- rows(TRAIN_IDS)
  rates <- churn_rates(t); top <- rates[rates$`Mayor de su variable`, ]; top <- utils::head(top[order(-top$`Tasa (%)`), ], 8)
  top <- top[nrow(top):1, ]; lab <- paste(top$Variable, "=", top$Categoría)
  f <- plot_ly() %>% add_trace(type = "bar", x = top$`Tasa (%)`, y = lab, orientation = "h", marker = list(color = highlight(top$`Tasa (%)`)),
                               text = sprintf("%.1f%%", top$`Tasa (%)`), textposition = "outside") %>%
    layout(shapes = list(vline(26.54, GREY, "dash")), annotations = list(list(text = T("promedio 26.5 %"), x = 26.54, y = 1, xref = "x", yref = "y domain", yanchor = "bottom", showarrow = FALSE)),
           xaxis = list(title = T("Tasa de abandono (%)"), range = c(0, 55)), yaxis = cat_axis(lab))
  findings <- c(N("El abandono es la clase minoritaria (26.5 %): se evaluará con métricas de la clase positiva y se compararán estrategias de balanceo."),
                N("Los clientes que abandonan tienen menor antigüedad y menor cargo acumulado, pero mayor cargo mensual; tenure es la asociación numérica más fuerte (r = −0.35)."),
                N("Contrato es la variable categórica más asociada (V de Cramér = 0.41), seguida de seguridad en línea y soporte técnico; gender y PhoneService no muestran asociación tras Holm."),
                N("Ninguna variable numérica es normal, por lo que se usa Spearman; tenure y TotalCharges están muy correlacionadas (ρ = 0.89) y se crea el cargo promedio mensual."),
                N("Las categorías \"sin internet\" y \"sin teléfono\" repiten información y se unifican en \"No\"."),
                N("Los 8 faltantes de TotalCharges son clientes con antigüedad 0: se reemplazan por 0. No hay valores atípicos según el IQR."))
  tagList(chart(T("Categorías con mayores tasas de abandono"), f, T("La categoría con mayor tasa de cada variable; la línea punteada es el promedio general."), 420, FALSE),
          subhead(T("Decisiones que deja el EDA")),
          div(class = "objectives g3", lapply(seq_along(findings), function(i) div(class = "obj", tags$span(class = "obj-n", sprintf("%02d", i)), tags$p(T(findings[i]))))))
}

# ---------------------------------------------------------------- explorador bivariado: el usuario elige los ejes y la variable de color
explorador_xy <- function(x, y, color, d) {
  if (!nrow(d)) return(empty())
  if (x == y) return(note(T("Elige dos variables distintas"), T("El eje X y el eje Y muestran la misma variable.")))
  set.seed(42); s <- d[sample(nrow(d), min(2000, nrow(d))), , drop = FALSE]
  s$.color <- if (color == "Churn") churn_label(s$Churn) else as.character(cat_name(s[[color]]))
  niveles <- if (color == "Churn") T(c("Permanece", "Abandona")) else sort(unique(s$.color))
  niveles <- niveles[niveles %in% s$.color]
  paleta <- if (color == "Churn") outcome_colors() else setNames(rep(c(BASE, HOT, GREY), 10)[seq_along(niveles)], niveles)
  simbolos <- setNames(rep(c("circle", "diamond", "square", "x", "triangle-up"), 4)[seq_along(niveles)], niveles)
  f <- plot_ly()
  if (x %in% CATEGORICAL) {
    xs <- sort(unique(s[[x]]))
    for (k in niveles) { m <- s$.color == k & !is.na(s[[y]])
      f <- add_trace(f, type = "box", x = s[[x]][m], y = s[[y]][m], name = k, marker = list(color = paleta[[k]]), line = list(color = paleta[[k]]), boxpoints = FALSE, quartilemethod = "linear") }
    f <- plotly_build(layout(f, xaxis = list(title = x, tickvals = xs, ticktext = as.character(cat_name(xs)), type = "category"), yaxis = list(title = y)))
    f$x$layout$boxmode <- "group"   # plotly.js lo admite aunque el esquema de plotly para R no lo declare
    insight <- fmt(T("Cajas de {y} por categoría de {x}, separadas por {c}."), y = y, x = x, c = if (color == "Churn") T("abandono") else color)
  } else {
    for (k in niveles) { m <- s$.color == k & !is.na(s[[x]]) & !is.na(s[[y]])
      f <- add_trace(f, type = "scattergl", mode = "markers", x = s[[x]][m], y = s[[y]][m], name = k, marker = list(color = paleta[[k]], symbol = simbolos[[k]], size = 5, opacity = .5)) }
    f <- layout(f, xaxis = list(title = x), yaxis = list(title = y))
    rho <- suppressWarnings(stats::cor(s[[x]], s[[y]], method = "spearman", use = "pairwise.complete.obs"))
    insight <- fmt(T("Muestra de hasta 2,000 clientes de la selección. Spearman entre {x} y {y}: ρ = {r}."), x = x, y = y, r = sprintf("%.2f", rho))
  }
  if (length(niveles) > 3 && color != "Churn") insight <- paste(insight, T("Con más de tres categorías, los símbolos distinguen los grupos que comparten color."))
  div(graph(f, 460, TRUE), tags$p(class = "insight", insight))
}

# ---------------------------------------------------------------- preparación de datos (capítulo de preprocesamiento)
STEPS <- list(c("target", N("Objetivo"), N("Churn Yes/No → 1/0. customerID solo para control.")), c("shield", N("TotalCharges"), N("Texto a número; los 8 vacíos de train (3 de test) tienen tenure 0 → 0.")),
              c("layers", N("Ingeniería"), N("“Sin internet / sin teléfono” → “No” y AvgChargePerMonth = TotalCharges / tenure.")), c("table", N("Codificar y escalar"), N("5 numéricas con StandardScaler y 15 categóricas con One-Hot: 24 columnas.")),
              c("scale", N("Calibrar"), N("Platt (sigmoide, 3 pliegues) dentro del entrenamiento de cada modelo; sin remuestreo.")), c("network", N("Modelo"), N("Se ajusta en entrenamiento y se evalúa en el pliegue reservado.")))

preparacion <- function() {
  m <- c(T("Imputar con 0"), T("Mediana + indicador")); a_ <- c(.8459, .8460)
  fi <- plot_ly() %>% add_trace(type = "bar", x = m, y = a_, error_y = list(type = "data", array = c(.0124, .0125), color = GREY), marker = list(color = c(HOT, BASE))) %>%
    bar_labels(a_, m, .802, 4, horizontal = FALSE) %>% layout(xaxis = cat_axis(m), yaxis = list(range = c(.8, .87), title = "AUC-ROC CV"))
  b <- read("baseline_test.csv"); cm <- confusion(b$y_true, as.integer(b$probability >= .5)); r <- roc_curve(b$y_true, b$probability)
  tagList(card(tags$h3(class = "chart-title", T("El pipeline paso a paso")),
               div(class = "pipeline", div(class = "flow-line"), lapply(seq_along(STEPS), function(k) div(class = "station", style = sprintf("animation-delay:%.2fs", (k - 1) * .15),
                   tags$span(class = "st-icon", icon(STEPS[[k]][1])), tags$h4(T(STEPS[[k]][2])), tags$p(T(STEPS[[k]][3])))))),
          grid(chart(T("¿Cero o mediana para TotalCharges?"), fi, T("Diferencia pareada por fold: +0.0001 ± 0.0001. Imputar con 0 es coherente con clientes sin facturación y no pierde desempeño."), 320, FALSE),
               card(tags$h3(class = "chart-title", T("Ingeniería de características")),
                    kpis(kpi(T("CORRELACIÓN CON MONTHLYCHARGES"), .9963, "AvgChargePerMonth", "", 4), kpi(T("COLUMNAS TRAS CODIFICAR"), 24, T("5 numéricas + 19 indicadores")), cls = "k2"),
                    tags$p(class = "insight", T("El cargo promedio mensual comparte casi toda la información con el cargo mensual; su aporte se evalúa en la ablación.")))),
          grid(chart(T("Modelo base del entregable 2 · matriz de confusión en test"), conf_heat(cm, 18), T("Regresión logística con umbral 0.5: detecta 293 de 374 abandonos, con 286 falsas alertas."), 340, FALSE),
               chart(T("Modelo base del entregable 2 · curva ROC en test"), roc_animation(list(list(name = T("Regresión logística"), fpr = r$fpr, tpr = r$tpr, color = BASE))), T("Pulsa play para trazar la curva. AUC = 0.8419."), 340)))
}
