# Resultados de los entregables 2 y 3 (112 configuraciones): se conservan como pestaña de la sección Modelos (src/entregable2.py del dash).
# Corresponden al capítulo MODELADO_AVANZADO del Jupyter Book. Usan predicciones y tablas guardadas; no cargan modelos.

cache <- new.env()
cached <- function(key, expr) { if (is.null(cache[[key]])) cache[[key]] <- expr; cache[[key]] }
oof <- function() cached("oof", read("oof_mejores.csv"))
anytime <- function() cached("anytime", read("anytime.csv"))
diversity <- function() cached("div", read("diversidad_generaciones.csv"))
shap_sample <- function() cached("shap", read("shap_muestra.csv"))
baseline <- function() cached("base", read("baseline_test.csv"))

BEST_BY_FAMILY <- c(knn = "knn__smote__bayesian_tpe", naive_bayes = "naive_bayes__none__random", logistic = "logistic__smote__grid",
                    decision_tree = "decision_tree__smote__bayesian_tpe", random_forest = "random_forest__smote__bayesian_tpe",
                    xgboost = "xgboost__smote__random", svm = "svm__smote__genetic")

# cat__Contract_Two year → Contrato: Dos años; num__tenure → Antigüedad (meses).
pretty <- function(feature) vapply(feature, function(f) {
  name <- sub("^[^_]+__", "", f)
  for (var in names(LABELS)[order(-nchar(names(LABELS)))]) {
    if (name == var) return(label(var))
    if (startsWith(name, paste0(var, "_"))) { lvl <- substring(name, nchar(var) + 2); return(paste0(label(var), ": ", cat_name(lvl))) }
  }
  name
}, "", USE.NAMES = FALSE)

# ================================================================ 4. MODELADO AVANZADO
METRICS <- c(f1_positive = N("F1 positivo"), recall_positive = "Recall", precision_positive = "Precision", roc_auc = "ROC-AUC", accuracy = "Accuracy")
MODEL_TABS <- c(N("1 · Resultados por modelo"), N("2 · Calibración"), N("3 · Optimizadores"), N("4 · Multifidelidad"), N("5 · Eficiencia computacional"),
                N("6 · Comparación estadística"), N("7 · Interpretabilidad"), N("8 · Umbral de alerta"), N("9 · Conclusiones"))

podium <- function() {
  p <- list(c("2", "XGBoost", "SMOTE · Random", .638, "second"), c("1", "Random Forest", "SMOTE · TPE", .638, "first"), c("3", T("Reg. logística"), "SMOTE · Grid", .634, "third"))
  card(eyebrow(T("PODIO · F1 MEDIO EXTERNO")),
       div(class = "podium", lapply(p, function(x) div(class = paste("pod", x[5]), tags$span(class = "medal", x[1]), tags$strong(x[2]), tags$small(x[3]), div(class = "bar-podium", counter(as.numeric(x[4]), 3))))),
       tags$p(class = "insight", T("Empate técnico: la diferencia entre RF y XGBoost es 0.0004, menor que la variación entre folds.")))
}

entregable2 <- function(keep) {
  tagList(note(T("Entregable 2"), T("Siete familias × cuatro balanceos × cuatro optimizadores = 112 configuraciones evaluadas con validación cruzada anidada y F1 como métrica. Se conservan como referencia; el modelo propio se comparó después con 11 benchmarks recalibrados.")),
          div(class = "hero", podium(), kpis(kpi(T("CONFIGURACIONES"), 112, "7 × 4 × 4"), kpi(T("UNIDADES EXTERNAS"), 1680, T("112 × 3 semillas × 5 folds")),
                                              kpi("FOLDS", "5 × 3", T("externos × internos")), kpi(T("PRESUPUESTO"), 24, T("evaluaciones por búsqueda")), cls = "k2")),
          tabs("model-tabs", T(MODEL_TABS), as.integer(keep("model-tabs", 0))))
}

model_tab <- function(i, input) {
  keep <- function(id, default) isolate(input[[id]]) %||% default
  fam <- setNames(as.list(model_name(names(MODELS))), names(MODELS))
  body <- switch(i + 1,
    tagList(card(cls = "controls-card", field(T("Familia de modelos (mejor configuración de cada una)"), dropdown("res-family", names(BEST_BY_FAMILY), keep("res-family", "random_forest"), fam)), uiOutput("res-output")),
            ranking_families(),
            card(cls = "controls-card", div(class = "controls",
                 field(T("Familia"), dropdown("model-family", c("Todas", names(MODELS)), keep("model-family", "Todas"), c(list(Todas = T("Todas")), fam))),
                 field(T("Balanceo"), dropdown("model-balance", c("Todos", names(BALANCES)), keep("model-balance", "Todos"), c(list(Todos = T("Todos")), setNames(as.list(balance_name(names(BALANCES))), names(BALANCES))))),
                 field(T("Métrica"), dropdown("model-metric", names(METRICS), keep("model-metric", "f1_positive"), setNames(as.list(T(unname(METRICS))), names(METRICS)))))),
            uiOutput("models-output"), robustez()),
    calibration_summary(),
    tagList(card(cls = "controls-card", field(T("Familia"), dropdown("opt-family", names(MODELS), keep("opt-family", "random_forest"), fam))), uiOutput("opt-output"), optimizer_effects()),
    multifidelidad(),
    eficiencia(),
    tagList(estadistica(), effect_sizes()),
    tagList(card(cls = "controls-card", field(T("Modelo explicado"), dropdown("explain-model", FINALISTS[1:2], keep("explain-model", FINALISTS[1]), setNames(as.list(T(unname(FINALIST_NAMES[FINALISTS[1:2]]))), FINALISTS[1:2]))),
                 uiOutput("explain-output")),
            note(T("Explicar no es causar"), T("Las atribuciones describen cómo decide el modelo, no qué provoca el abandono. Los casos locales se eligieron por máxima confianza, no por ser representativos."))),
    umbral_lab(keep),
    conclusiones())
  tab_body(body)
}

ranking_families <- function() {
  s <- SUMMARY[match(BEST_BY_FAMILY, SUMMARY$experiment_id), ]; s <- s[order(s$f1_positive_mean), ]; s$n <- config_name(s$experiment_id)
  f <- plot_ly() %>% add_trace(type = "bar", x = s$f1_positive_mean, y = s$n, orientation = "h", error_x = list(type = "data", array = s$f1_positive_std, color = GREY),
                               marker = list(color = highlight(s$f1_positive_mean, nrow(s)))) %>%
    bar_labels(s$f1_positive_mean, s$n, .551) %>% layout(xaxis = list(range = c(.55, .68), title = T("F1 positivo · media de 5 folds externos")), yaxis = cat_axis(s$n))
  chart(T("Mejor configuración de cada familia"), f, T("Barras: media de los folds externos; bigotes: desviación estándar. Random Forest y XGBoost empatan arriba (0.638)."), 380, FALSE)
}

estadistica <- function() {
  st <- file.path(DATA, "estadistica")
  fr <- jsonlite::fromJSON(file.path(st, "friedman.json")); nem <- jsonlite::fromJSON(file.path(st, "nemenyi_estado.json"))
  r <- read("estadistica", "rangos_medios.csv"); r <- r[order(r$mean_rank), ]; cd <- nem$critical_difference
  r$pos <- seq_len(nrow(r)); best <- r$mean_rank[1]; inside <- r$mean_rank <= best + cd
  fn <- plot_ly() %>% add_trace(type = "scatter", mode = "markers", x = r$mean_rank, y = r$pos, marker = list(color = ifelse(seq_len(nrow(r)) == 1, HOT, ifelse(inside, BASE, GREY)), size = 8),
                                text = config_name(r$experiment_id), hovertemplate = paste0("%{text}<br>", T("Rango medio"), ": %{x:.1f}<extra></extra>")) %>%
    layout(shapes = list(list(type = "rect", x0 = best, x1 = best + cd, y0 = 0, y1 = nrow(r) + 1, xref = "x", yref = "y", fillcolor = "rgba(46,134,171,.08)", line = list(width = 0), layer = "below")),
           annotations = list(list(x = best + cd / 2, y = nrow(r) - 2, xref = "x", yref = "y", text = fmt(T("diferencia crítica = {cd}"), cd = sprintf("%.1f", cd)), showarrow = FALSE)),
           yaxis = list(autorange = "reversed", title = T("Posición"), showticklabels = FALSE), xaxis = list(title = T("Rango medio (menor es mejor)")))
  ic <- read("estadistica", "ic_test_suplementarios.csv")
  vals <- list(random_forest__smote__bayesian_tpe = c(f1 = .627, recall = .735, auc = .844), xgboost__smote__random = c(f1 = .637, recall = .765, auc = .842),
               logistic__smote__grid = c(f1 = .625, recall = .794, auc = .842))
  names_ <- T(unname(FINALIST_NAMES[FINALISTS]))
  icp <- lapply(seq_along(c("f1", "recall", "auc")), function(j) {
    col <- c("f1", "recall", "auc")[j]
    v <- vapply(FINALISTS, function(e) vals[[e]][[col]], 1); x <- ic[ic$metric == col, ]; x <- x[match(FINALISTS, x$experiment_id), ]
    plot_ly() %>% add_trace(type = "scatter", mode = "markers", x = v, y = names_, marker = list(size = 11, color = highlight(v)),
                            error_x = list(type = "data", symmetric = FALSE, array = x$high - v, arrayminus = v - x$low, color = GREY), showlegend = FALSE) %>%
      layout(yaxis = cat_axis(names_, showticklabels = j == 1))
  })
  fig_ic <- subplots(icp, c("F1", "Recall", "AUC-ROC"), hspace = .08)
  dl <- read("estadistica", "delong_holm.csv")
  fin <- function(x) T(ifelse(x %in% names(FINALIST_NAMES), FINALIST_NAMES[x], x))
  dlt <- setNames(df(fin(dl$a), fin(dl$b), round(dl$delta_auc, 4), round(dl$z, 3), round(dl$p_raw, 4), sprintf("[%.4f, %.4f]", dl$normal_ci_low, dl$normal_ci_high), round(dl$p_holm, 3)),
                  c(T("Modelo A"), T("Modelo B"), T("Δ AUC"), "z", T("p sin ajustar"), T("IC 95 % Δ AUC"), T("p Holm")))
  curves <- lapply(FINALISTS, function(e) {
    p <- predictions(e); rc <- roc_curve(p$y_true, p$score)
    list(name = sprintf("%s · AUC %.3f", T(FINALIST_NAMES[[e]]), auc(p$y_true, p$score)), fpr = rc$fpr, tpr = rc$tpr,
         color = if (e == FINALISTS[1]) HOT else if (e == FINALISTS[2]) BASE else GREY)
  })
  tagList(kpis(kpi("FRIEDMAN χ²", round(fr$statistic, 2), sprintf("p = %s", sub("e-0?", "e-", sprintf("%.1e", fr$p_exploratory))), "", 2), kpi("KENDALL W", round(fr$kendall_W, 2), T("concordancia entre folds"), "", 2),
               kpi(T("PARES NEMENYI < 0.05"), 31, T("de 6,216 · exploratorio")), kpi(T("p DELONG (HOLM)"), 1.0, T("en los tres pares de finalistas"), "", 1)),
          chart(T("Nemenyi y diferencia crítica"), fn, T("Cada punto es una configuración ordenada por su rango medio en los 5 folds. Las que caen dentro de la banda no se distinguen de la mejor (resaltada)."), 460, FALSE),
          grid(chart(T("Finalistas en el test histórico · IC 95 % bootstrap"), fig_ic, T("Los intervalos de los tres finalistas se superponen ampliamente."), 330, FALSE),
               chart(T("Curvas ROC de los finalistas en el test"), roc_animation(curves), T("Pulsa play para trazarlas. Random Forest (resaltado) tiene el mayor AUC, pero la diferencia no es significativa."), 330)),
          card(tags$h3(class = "chart-title", T("DeLong dirigido con corrección de Holm")), table_ui(dlt),
               tags$p(class = "insight", T("Ningún par difiere tras Holm (p = 1.0) y todos los intervalos incluyen el cero. No detectar diferencias no prueba que sean equivalentes."))))
}

res_output <- function(family) {
  e <- BEST_BY_FAMILY[[family]]; o <- oof(); o <- o[o$experiment == e, ]
  cm <- confusion(o$y_true, o$y_pred)
  rc <- roc_curve(o$y_true, o$score_positive); a_ <- auc(o$y_true, o$score_positive)
  froc <- plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = rc$fpr, y = rc$tpr, line = list(color = BASE, width = 2.6), name = sprintf("AUC OOF = %.3f", a_)) %>%
    add_trace(type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), name = T("Azar")) %>%
    layout(xaxis = list(title = T("Tasa de falsos positivos")), yaxis = list(title = "Recall"))
  fr <- plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), name = T("Calibración perfecta"))
  for (cc in list(c("probability_native", T("Nativa"), SOFT), c("probability_sigmoid", T("Sigmoide (Platt)"), BASE), c("probability_isotonic", T("Isotónica"), HOT))) {
    v <- o[[cc[1]]]
    if (any(!is.na(v))) {
      b <- cut(v, seq(0, 1, length.out = 11), include.lowest = TRUE)
      x <- tapply(v, b, mean); y <- tapply(o$y_true, b, mean); ok <- !is.na(x) & !is.na(y)
      fr <- add_trace(fr, type = "scatter", mode = "lines+markers", x = unname(x[ok]), y = unname(y[ok]), name = cc[2], line = list(color = cc[3], width = 2.4), marker = list(color = cc[3]))
    }
  }
  fr <- layout(fr, xaxis = list(title = T("Probabilidad media"), range = c(0, 1)), yaxis = list(title = T("Frecuencia de abandono"), range = c(0, 1)))
  row <- SUMMARY[SUMMARY$experiment_id == e, ]
  m <- lapply(names(METRICS), function(k) kpi(toupper(T(METRICS[[k]])), round(row[[paste0(k, "_mean")]], 3), sprintf("± %.3f", row[[paste0(k, "_std")]]), if (k == "f1_positive") "hot" else "", 3))
  div(eyebrow(config_name(e)), do.call(kpis, c(m, cls = "k5")),
      div(class = "grid g3",
          chart(T("Matriz de confusión OOF"), conf_heat(cm, 17), fmt(T("Predicciones fuera de muestra de los 5 folds externos ({n} clientes)."), n = f0(nrow(o))), 330, FALSE),
          chart(T("Curva ROC OOF"), froc, "", 330), chart(T("Confiabilidad · 10 intervalos"), fr, T("Más cerca de la diagonal = probabilidades mejor calibradas."), 330)))
}

model_output <- function(family, balance, metric) {
  d <- SUMMARY
  if (family != "Todas") d <- d[d$model == family, ]
  if (balance != "Todos") d <- d[d$balance == balance, ]
  mean_ <- paste0(metric, "_mean"); sd_ <- paste0(metric, "_std")
  d <- d[order(-d[[mean_]]), ]; top <- utils::head(d, 12); top <- top[nrow(top):1, ]; top$n <- config_name(top$experiment_id)
  lo <- max(0, min(top[[mean_]]) - .06)
  f <- plot_ly() %>% add_trace(type = "bar", x = top[[mean_]], y = top$n, orientation = "h", error_x = list(type = "data", array = top[[sd_]], color = GREY),
                               marker = list(color = highlight(top[[mean_]], nrow(top)))) %>%
    bar_labels(top[[mean_]], top$n, lo + .001) %>%
    layout(xaxis = list(title = paste(T(METRICS[[metric]]), "·", T("media externa")), range = c(lo, min(1, max(top[[mean_]]) + .04))), yaxis = cat_axis(top$n))
  mods <- sort(unique(d$model)); bals <- sort(unique(d$balance))
  z <- outer(mods, bals, Vectorize(function(m, b) { v <- d[[mean_]][d$model == m & d$balance == b]; if (length(v)) mean(v) else NA }))
  heat <- heatmap(z, balance_name(bals), model_name(mods), SEQ, digits = 3, colorbar_title = T(METRICS[[metric]])) %>% layout(yaxis = list(autorange = "reversed"))
  cols <- unique(c("model", "balance", "optimizer", mean_, sd_, "recall_positive_mean", "precision_positive_mean", "roc_auc_mean"))
  view <- d[cols]; view$model <- model_name(view$model); view$balance <- balance_name(view$balance); view$optimizer <- optimizer_name(view$optimizer)
  div(grid(chart(fmt(T("Ranking de configuraciones · top {k} de {n}"), k = nrow(top), n = nrow(d)), f, T("La mejor configuración está resaltada; bigotes: desviación estándar entre folds."), 480, FALSE),
           chart(T("Familia × balanceo"), heat, T("Promedio sobre optimizadores: la columna SMOTE es la más alta en casi todas las familias."), 480, FALSE)),
      details(tagList(icon("table"), fmt(T("Tabla completa · {n} configuraciones"), n = nrow(d))), table_ui(view, filter = TRUE)))
}

opt_output <- function(family) {
  d <- anytime(); d <- d[d$model == family & d$replicate == 0, ]
  g <- stats::aggregate(cbind(f1 = d$best_inner_f1, sec = d$seconds), list(optimizer = d$optimizer, evaluation = d$evaluation), mean)
  g <- g[order(g$optimizer, g$evaluation), ]
  last <- g[g$evaluation == max(g$evaluation), ]; best_opt <- last$optimizer[order(last$f1)][nrow(last)]
  dashes <- c(grid = "dot", random = "dash", bayesian_tpe = "solid", genetic = "dashdot")
  lines <- function(x, xtitle) {
    p <- plot_ly()
    for (o in sort(unique(g$optimizer))) { s <- g[g$optimizer == o, ]
      p <- add_trace(p, type = "scatter", mode = "lines", x = s[[x]], y = s$f1, name = optimizer_name(o),
                     line = list(color = if (o == best_opt) HOT else BASE, dash = dashes[[o]], width = if (o == best_opt) 3 else 2)) }
    layout(p, xaxis = list(title = xtitle), yaxis = list(title = T("Mejor F1 interno")))
  }
  dv <- diversity(); dv <- dv[dv$model == family, ]
  fd <- plot_ly() %>% add_trace(type = "scatter", mode = "lines+markers", x = dv$generation, y = dv$unique_fraction, name = T("Fracción única"), line = list(color = BASE), marker = list(color = BASE)) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = dv$generation, y = dv$mean_normalized_distance, name = T("Distancia normalizada"), line = list(color = GREY, dash = "dash"), marker = list(color = GREY)) %>%
    layout(xaxis = list(title = T("Generación")), yaxis = list(title = T("Diversidad"), range = c(0, 1.05)))
  u <- UNITS[UNITS$model == family & UNITS$replicate == 0, ]; u$b <- optimizer_name(u$optimizer)
  ob <- unique(u$b)
  fb <- plot_ly() %>% add_trace(type = "box", x = u$b, y = u$search_seconds, marker = list(color = BASE), line = list(color = BASE), fillcolor = "rgba(46,134,171,.5)", showlegend = FALSE) %>%
    layout(xaxis = cat_axis(ob, title = ""), yaxis = list(title = T("Segundos de búsqueda")))
  div(grid(chart(T("Mejor F1 interno por evaluaciones únicas"), lines("evaluation", T("Evaluaciones únicas")), T("A igual presupuesto (24 evaluaciones) los buscadores convergen; el mejor al final está resaltado."), 380),
           chart(T("Mejor F1 interno por tiempo acumulado"), lines("sec", T("Tiempo acumulado (s)")), T("La misma búsqueda medida en segundos."), 380)),
      grid(chart(T("Diversidad del algoritmo genético"), fd, T("La población pierde diversidad con las generaciones: converge hacia pocas configuraciones."), 340),
           chart(T("Presupuesto y coste empírico"), fb, T("Segundos por unidad externa en el equipo del experimento."), 340, FALSE)))
}

# Valores de las variables (en escala original) para colorear el beeswarm de SHAP.
feature_values <- function(names_, ids) {
  r <- rows(ids); out <- list()
  for (f in names_) {
    n <- sub("^[^_]+__", "", f)
    v <- if (n == "AvgChargePerMonth") ifelse(r$tenure > 0, ifelse(is.na(r$TotalCharges), 0, r$TotalCharges) / r$tenure, r$MonthlyCharges)
         else if (n %in% names(r)) { x <- r[[n]]; x[is.na(x)] <- 0; x }
         else { var <- names(LABELS)[order(-nchar(names(LABELS)))]; var <- var[startsWith(n, paste0(var, "_"))][1]; as.numeric(as.character(r[[var]]) == substring(n, nchar(var) + 2)) }
    v <- as.numeric(v); out[[f]] <- if (length(unique(v)) > 2) rank(v, ties.method = "average") / length(v) else v
  }
  out
}

explain_output <- function(experiment) {
  s <- shap_sample(); s <- s[s$model == experiment, ]
  imp <- tapply(abs(s$shap), s$feature, mean); order_ <- names(sort(imp))
  ids <- unique(s$row_id); vals <- feature_values(order_, ids)
  y <- match(s$feature, order_) - 1; cval <- mapply(function(f, r) vals[[f]][match(r, ids)], s$feature, s$row_id)
  set.seed(0); jitter <- stats::rnorm(nrow(s), 0, .12)
  fb <- plot_ly() %>% add_trace(type = "scattergl", mode = "markers", x = s$shap, y = y + jitter, hovertemplate = "SHAP %{x:.3f}<extra></extra>",
                                marker = list(size = 4, color = unname(cval), colorscale = list(list(0, BASE), list(1, HOT)), opacity = .75,
                                              colorbar = list(title = list(text = T("Valor")), tickvals = c(0, 1), ticktext = T(c("bajo", "alto")), thickness = 10))) %>%
    layout(shapes = list(vline(0)), yaxis = list(tickvals = seq_along(order_) - 1, ticktext = pretty(order_)), xaxis = list(title = T("Valor SHAP (impacto en la probabilidad)")))
  path <- file.path(DATA, "interpretabilidad", experiment)
  cases <- jsonlite::fromJSON(file.path(path, "casos_locales.json"), simplifyVector = FALSE)
  lime <- jsonlite::fromJSON(file.path(path, "lime_resultados.json"), simplifyVector = FALSE)
  base <- s$base[1]
  local <- lapply(cases, function(case) {
    row <- case$row_id; sh <- utils::read.csv(file.path(path, sprintf("shap_local_%d.csv", row)), stringsAsFactors = FALSE)
    sh <- sh[order(-abs(sh$shap)), ]; top <- utils::head(sh, 10); rest <- sum(sh$shap[-(1:10)])
    yl <- c("E[f(X)]", pretty(top$feature), fmt(T("{n} variables más"), n = nrow(sh) - 10), "f(x)")
    fw <- plot_ly() %>% add_trace(type = "waterfall", orientation = "h", y = yl, x = c(base, top$shap, rest, 0), measure = c("absolute", rep("relative", nrow(top) + 1), "total"),
                                  increasing = list(marker = list(color = HOT)), decreasing = list(marker = list(color = BASE)), totals = list(marker = list(color = GREY)),
                                  connector = list(line = list(color = GREY, width = 1))) %>%
      layout(yaxis = cat_axis(yl, autorange = "reversed"), xaxis = list(title = T("Probabilidad de abandono")))
    lw <- Filter(function(x) x$row_id == row && x$seed == 42, lime)[[1]]
    ld <- do.call(rbind, lapply(lw$weights, function(w) df(r = w[[1]], w = as.numeric(w[[2]])))); ld <- ld[nrow(ld):1, ]
    fl <- plot_ly() %>% add_trace(type = "bar", x = ld$w, y = ld$r, orientation = "h", marker = list(color = ifelse(ld$w > 0, HOT, BASE))) %>%
      layout(shapes = list(vline(0)), xaxis = list(title = T("Peso del sustituto local")), yaxis = cat_axis(ld$r))
    ok <- isTRUE(case$correct)
    card(div(class = "card-head", tags$h3(paste(T("Cliente"), rows(row)$customerID)), tags$span(class = paste("pill", if (ok) "ok" else "bad"), icon(if (ok) "check" else "x", "sm"), if (ok) T("acierto") else T("error"))),
         tags$p(class = "muted", fmt(T("Realidad: {r} · probabilidad del modelo: {p}"), r = if (case$y_true == 1) T("abandonó") else T("permaneció"), p = pct(case$probability))),
         grid(div(tags$h4("SHAP · waterfall"), graph(fw, 420, FALSE)),
              div(tags$h4(T("LIME · clase abandono")), graph(fl, 420, FALSE), tags$p(class = "insight", fmt(T("R² ponderado del sustituto: {r}"), r = sprintf("%.2f", lw$weighted_r2))))))
  })
  div(chart(T("SHAP global · probabilidad nativa de abandono"), fb, T("Cada punto es un cliente. A la derecha empuja hacia el abandono; el color indica el valor de la variable (resaltado = alto)."), 560, FALSE),
      subhead(T("Casos locales")), local)
}

# ---------------------------------------------------------------- 8 · umbral de alerta
umbral_lab <- function(keep) {
  tagList(card(cls = "controls-card", div(class = "controls",
               field(T("Modelo finalista"), dropdown("threshold-model", FINALISTS, keep("threshold-model", FINALISTS[1]), setNames(as.list(T(unname(FINALIST_NAMES[FINALISTS]))), FINALISTS))),
               field(T("Umbral de alerta"), slider("threshold", .05, .95, .01, keep("threshold", .5), c(`0.1` = "0.10", `0.3` = "0.30", `0.5` = "0.50", `0.7` = "0.70", `0.9` = "0.90"), tooltip = TRUE))),
               uiOutput("threshold-output")),
          note(T("Decisión de negocio"), T("Bajar el umbral detecta más clientes en riesgo pero dispara más falsas alertas. Elegirlo bien requiere conocer el costo de cada contacto y de cada cliente perdido.")))
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

# ---------------------------------------------------------------- 9 · conclusiones
REPRO <- list(c("file", N("Versiones exactas"), N("requirements.txt fija cada librería usada en el entrenamiento.")),
              c("shield", N("Verificación automática"), N("verificar_reproduccion.py compara versiones, huellas SHA-256 de los datos y los artefactos de las 1,680 unidades.")),
              c("layers", N("Resultados versionados"), N("Pipelines, predicciones, checkpoints y semillas de cada unidad quedan guardados; el libro se construye sin reentrenar.")),
              c("code", N("Código del experimento"), N("Un apéndice reproduce las 32 celdas exactas que entrenaron las 112 configuraciones.")))

conclusiones <- function() {
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
  tagList(subhead(T("Hallazgos principales"), T("Conclusiones de los entregables 2 y 3; las del modelo propio están en el capítulo 9 del libro.")),
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
