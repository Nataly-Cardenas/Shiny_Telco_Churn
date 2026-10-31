# 4. Modelado avanzado · resultados complementarios del libro (entregable 3): robustez entre semillas,
# calibración Platt frente a isotónica, tamaño del efecto de los optimizadores, multifidelidad,
# eficiencia computacional y tamaños del efecto de la comparación estadística.

comp <- function(name) cached(paste0("comp_", name), read("complementarios", paste0(name, ".csv")))

robustez <- function() {
  r <- comp("robustez_semillas"); r$n <- model_name(r$model); r <- r[order(r$f1_positive_mean), ]
  f <- plot_ly() %>% add_trace(type = "scatter", mode = "markers", x = r$f1_positive_mean, y = r$n, name = T("Realización principal · IC 95 %"),
                               marker = list(size = 13, color = highlight(r$f1_positive_mean)),
                               error_x = list(type = "data", symmetric = FALSE, array = r$high - r$f1_positive_mean, arrayminus = r$f1_positive_mean - r$low, color = GREY))
  for (k in 1:2) f <- add_trace(f, type = "scatter", mode = "markers", x = r[[paste("F1 semilla", k)]], y = r$n, name = fmt(T("Semilla {k}"), k = k),
                                marker = list(size = 9, color = SOFT, symbol = c("diamond", "square")[k]))
  f <- layout(f, xaxis = list(title = T("F1 positivo · media de 5 folds externos")), yaxis = cat_axis(r$n))
  t <- r[nrow(r):1, c("n", "f1_positive_mean", "f1_positive_std", "F1 semilla 1", "F1 semilla 2", "low", "high")]
  names(t) <- c("Familia", "F1 realización principal", "DE entre folds", "F1 semilla 1", "F1 semilla 2", "IC 95 % inferior", "IC 95 % superior")
  tagList(subhead(T("Robustez entre semillas e intervalos de confianza")),
          grid(chart(T("F1 de cada familia en las tres realizaciones"), f, T("Las semillas cambian el optimizador, el modelo y el remuestreo con los mismos folds: el F1 apenas se mueve. Los intervalos son condicionales a modelos fijos."), 380),
               card(tags$h3(class = "chart-title", T("Media por realización e IC condicional")), table_ui(tt(t)),
                    tags$p(class = "insight", T("Los intervalos usan 2.000 remuestreos estratificados por fold; no incluyen reentrenamiento ni la selección entre 112 configuraciones.")))))
}

calibration_summary <- function() {
  rep <- comp("calibracion_representantes"); rep$n <- model_name(rep$model)
  bars <- function(kind) plot_ly() %>%
    add_trace(type = "bar", x = rep$n, y = rep[[paste(kind, "nativo")]], name = T("Nativa"), marker = list(color = SOFT)) %>%
    add_trace(type = "bar", x = rep$n, y = rep[[paste(kind, "sigmoid")]], name = T("Sigmoide (Platt)"), marker = list(color = BASE)) %>%
    add_trace(type = "bar", x = rep$n, y = rep[[paste(kind, "isotónica")]], name = T("Isotónica"), marker = list(color = HOT)) %>%
    layout(barmode = "group", xaxis = cat_axis(rep$n), yaxis = list(title = kind))
  u <- UNITS[UNITS$replicate == 0, ]
  sig <- stats::aggregate(u[c("cal_brier", "cal_ece")], list(experiment_id = u$experiment_id), mean)
  iu <- comp("isotonica_unidades"); iso <- stats::aggregate(iu[c("iso_brier", "iso_ece")], list(experiment_id = iu$experiment_id), mean)
  m <- merge(sig, iso, by = "experiment_id"); m$n <- config_name(m$experiment_id)
  scatter <- function(x, y, title) {
    better <- m[[y]] < m[[x]]; lo <- min(m[[x]], m[[y]]) * .97; hi <- max(m[[x]], m[[y]]) * 1.03
    f <- plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = c(lo, hi), y = c(lo, hi), line = list(dash = "dash", color = GREY), showlegend = FALSE, hoverinfo = "skip")
    for (g in list(list(better, T("Isotónica mejor"), HOT), list(!better, T("Sigmoide mejor"), BASE)))
      f <- add_trace(f, type = "scatter", mode = "markers", x = m[[x]][g[[1]]], y = m[[y]][g[[1]]], name = sprintf("%s (%d)", g[[2]], sum(g[[1]])),
                     marker = list(color = g[[3]], size = 8, opacity = .8), text = m$n[g[[1]]],
                     hovertemplate = paste0("%{text}<br>", T("sigmoide"), " %{x:.4f} · ", T("isotónica"), " %{y:.4f}<extra></extra>"))
    layout(f, xaxis = list(title = paste(title, "·", T("sigmoide (Platt)")), range = c(lo, hi)), yaxis = list(title = paste(title, "·", T("isotónica")), range = c(lo, hi)))
  }
  fam <- setNames(as.list(model_name(names(MODELS))), names(MODELS))
  tagList(
    kpis(kpi(T("ISOTÓNICA MEJORA A LA NATIVA"), 84, T("de 96 configuraciones, en Brier y ECE")), kpi(T("PLATT MEJORA A LA NATIVA"), 80, T("de 96 configuraciones, en Brier y ECE")),
         kpi(T("ISOTÓNICA CON MENOR ECE"), 74, T("de 112 frente a Platt")), kpi(T("ISOTÓNICA CON MENOR BRIER"), 67, T("de 112 frente a Platt"))),
    note(T("¿Por qué recalibrar?"), T(paste0("Balancear con SMOTE o con pesos le enseña al modelo un mundo donde la mitad de los clientes abandona, así que sus probabilidades quedan infladas. ",
                                            "Platt ajusta una curva en S con dos parámetros; la isotónica ajusta una escalera no decreciente (PAV) sin suponer ninguna forma. ",
                                            "Ambas usan las mismas tres particiones dentro del entrenamiento externo: el fold externo nunca entrena al calibrador."))),
    card(cls = "controls-card", field(T("Familia (mejor configuración)"), dropdown("cal-family", names(BEST_BY_FAMILY), "random_forest", fam)), uiOutput("cal-map-output")),
    grid(chart(T("Brier por familia · representantes"), bars("Brier"), T("Menor es mejor. Las dos recalibraciones recortan el Brier entre 9 % y 34 %; entre ellas la diferencia casi siempre está en la cuarta cifra decimal."), 360),
         chart(T("ECE por familia · representantes"), bars("ECE"), T("Las dos ramas bajan el ECE de 0.10–0.21 a 0.02–0.04. SVM no tiene probabilidad nativa: solo existe recalibrada."), 360)),
    grid(chart(T("Brier de las 112 configuraciones"), scatter("cal_brier", "iso_brier", "Brier"), T("Cada punto es una configuración; bajo la diagonal (resaltados) la isotónica gana. Casi todos quedan pegados a la diagonal."), 380),
         chart(T("ECE de las 112 configuraciones"), scatter("cal_ece", "iso_ece", "ECE"), T("La isotónica tiene menor ECE en 74 de 112 configuraciones."), 380)),
    note(T("Síntesis de la calibración"), T(paste0("Conviene la isotónica cuando la distorsión no tiene forma de S (GaussianNB, árboles) y Platt cuando el score ya es un margen o una fracción de vecinos (SVM, KNN). ",
                                                  "Ojo con el F1: calibrar acerca las probabilidades a la prevalencia real y, con el umbral fijo en 0.5, ambas ramas detectan menos abandonos. ",
                                                  "La calibración sirve para leer las probabilidades como riesgos; no se usó para elegir el ranking principal."))))
}

# Cómo transforma cada recalibrador el score nativo: curva en S (Platt) frente a escalera (isotónica).
cal_map <- function(family) {
  e <- BEST_BY_FAMILY[[family]]; o <- oof(); o <- o[o$experiment == e, ]
  native <- any(!is.na(o$probability_native)); xcol <- if (native) "probability_native" else "score_positive"
  s <- o[order(o[[xcol]]), ]; s <- s[seq(1, nrow(s), by = 3), ]
  f <- plot_ly()
  if (native) f <- add_trace(f, type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), name = T("Sin cambio"), hoverinfo = "skip")
  for (cc in list(c("probability_sigmoid", T("Sigmoide (Platt)"), BASE), c("probability_isotonic", T("Isotónica"), HOT)))
    f <- add_trace(f, type = "scatter", mode = "markers", x = s[[xcol]], y = s[[cc[1]]], name = cc[2], marker = list(color = cc[3], size = 5, opacity = .55))
  f <- layout(f, xaxis = list(title = if (native) T("Probabilidad nativa") else T("Margen de SVM (score)")), yaxis = list(title = T("Probabilidad recalibrada"), range = c(0, 1)))
  n_iso <- as.integer(mean(tapply(o$probability_isotonic, o$outer_fold, function(v) length(unique(v)))))
  div(eyebrow(config_name(e)), graph(f, 340),
      tags$p(class = "insight", fmt(T("Cada punto es un cliente fuera de muestra (5 folds externos). Platt dibuja una curva suave; la isotónica, escalones: en promedio {n} probabilidades distintas por fold."), n = n_iso)))
}

optimizer_effects <- function() {
  e <- comp("efecto_optimizadores"); opts <- unique(e$optimizer); names_ <- T(opts)
  f <- plot_ly()
  for (ev in list(c(8, HOT), c(24, BASE))) {
    s <- e[e$evaluaciones == as.numeric(ev[1]), ]; s <- s[match(opts, s$optimizer), ]
    f <- add_trace(f, type = "bar", x = names_, y = s$d_z_pareado, name = fmt(T("{n} evaluaciones"), n = ev[1]), marker = list(color = ev[2]),
                   text = sprintf("%+.2f", s$d_z_pareado), textposition = "outside")
  }
  f <- layout(f, barmode = "group", shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0, line = list(color = GREY))),
              xaxis = cat_axis(names_), yaxis = list(title = T("d_z pareado frente a Grid"), range = c(-.35, .6)))
  t <- e; names(t) <- c("Evaluaciones", "Optimizador", "Comparaciones", "ΔF1 interno", "δ de Cliff (sin emparejar)", "d_z pareado", "Biserial de rangos", "Magnitud")
  c_ <- comp("coste_busqueda"); c_$model <- model_name(c_$model); c_$optimizer <- optimizer_name(c_$optimizer)
  names(c_) <- c("Familia", "Optimizador", "Búsqueda (s)", "DE búsqueda", "Reajuste (s)", "Inferencia (s)", "Memoria (MiB)", "Evaluaciones", "Propuestas", "Segundos por evaluación")
  tagList(subhead(T("Tamaño del efecto frente a Grid")),
          grid(chart(T("Ventaja pareada de cada optimizador sobre Grid"), f, T("Con 8 evaluaciones (resaltado) hay una ventaja pequeña pero consistente (d_z ≈ 0.41–0.47); con 24 desaparece y queda del orden de 10⁻⁴ en F1."), 340),
               card(tags$h3(class = "chart-title", T("420 búsquedas emparejadas por optimizador y presupuesto")), table_ui(tt(t)),
                    tags$p(class = "insight", T("Sin emparejar, el δ de Cliff es despreciable (|δ| ≤ 0.07): la variación entre familias y folds es mucho mayor que la del optimizador.")))),
          details(tagList(icon("table"), T("Presupuesto y coste empírico por familia y optimizador")), table_ui(tt(c_), page_size = 28)))
}

MF_NAMES <- c(successive_halving = N("Successive Halving"), random_completo = N("Random completo"))

multifidelidad <- function() {
  r <- comp("multifidelidad_resumen"); pf <- comp("multifidelidad_por_fold")
  funnel <- plot_ly() %>% add_trace(type = "funnel", y = vapply(1:4, function(k) fmt(T("Ronda {k}"), k = k), ""), x = c(27, 9, 3, 1), text = c("r/27", "r/9", "r/3", "r"),
                                    textinfo = "value+text", marker = list(color = c(BASE, BASE, BASE, HOT)), connector = list(line = list(color = GREY, width = 1))) %>%
    layout(yaxis = cat_axis(vapply(1:4, function(k) fmt(T("Ronda {k}"), k = k), "")))
  cards <- lapply(c("random_forest", "svm", "xgboost"), function(m) {
    s <- r[r$model == m, ]; g <- function(meth, col) s[[col]][s$method == meth]
    speed <- g("random_completo", "segundos") / g("successive_halving", "segundos"); d <- g("successive_halving", "f1_externo") - g("random_completo", "f1_externo")
    kpi(toupper(model_name(m)), round(speed, 1), fmt(T("ΔF1 externo {d} · Spearman entre rondas {s}"), d = sprintf("%+.3f", d), s = sprintf("%.2f", g("successive_halving", "spearman_rondas"))),
        if (m == "svm") "hot" else "", 1, "×")
  })
  f <- plot_ly()
  for (ms in list(c("random_forest", "circle"), c("svm", "diamond"), c("xgboost", "square")))
    for (mc in list(c("random_completo", BASE), c("successive_halving", HOT))) {
      s <- pf[pf$model == ms[1] & pf$method == mc[1], ]
      f <- add_trace(f, type = "scatter", mode = "markers", x = s$search_seconds, y = s$outer_f1, name = T(MF_NAMES[[mc[1]]]), legendgroup = mc[1], showlegend = ms[1] == "random_forest",
                     marker = list(color = mc[2], symbol = ms[2], size = 11, opacity = .85), text = s$outer_fold,
                     hovertemplate = paste0(model_name(ms[1]), " · ", T(MF_NAMES[[mc[1]]]), "<br>", T("Fold"), " %{text}<br>%{x:.0f} s · F1 %{y:.3f}<extra></extra>"))
    }
  f <- layout(f, xaxis = list(type = "log", title = T("Tiempo de búsqueda por fold externo (s, escala log)")), yaxis = list(title = T("F1 en el fold externo")))
  t <- r; t$model <- model_name(t$model); t$method <- T(unname(MF_NAMES[t$method]))
  names(t) <- c("Familia", "Método", "Segundos", "DE segundos", "Ajustes", "Ajustes equivalentes", "F1 interno", "F1 externo", "DE F1 externo", "AUC externo", "Spearman entre rondas", "F1 Random (sección 1)")
  tagList(do.call(kpis, c(cards, list(kpi(T("CÓMPUTO TEÓRICO"), 6, T("12 ajustes completos frente a 72"), "", 0, "×")), cls = "k4")),
          grid(chart(T("Successive Halving: 27 candidatos, 4 rondas"), funnel, T("En cada ronda pasa el tercio con mejor F1 interno y el recurso se triplica: árboles en Random Forest y XGBoost (12 a 324), filas en SVM."), 340, FALSE),
               chart(T("Calidad frente a costo: cada punto es un fold externo"), f, T("Círculos: Random Forest; rombos: SVM; cuadrados: XGBoost. En Random Forest y SVM los puntos de SH (resaltados) quedan a la izquierda sin bajar: igual F1 con una fracción del tiempo. En XGBoost SH eligió peor en cuatro de cinco folds."), 340)),
          note(T("¿Cuándo conviene la multifidelidad?"), T(paste0("Cuando el recurso recortado domina el costo y no altera el orden de los candidatos: filas en SVM (su costo crece como n²) y árboles en Random Forest. ",
                                                                  "En XGBoost la tasa de aprendizaje interactúa con el número de árboles: con 12 árboles un buen candidato parece malo, y la fidelidad baja engaña."))),
          details(tagList(icon("table"), T("Resumen por familia y método (5 folds externos)")), table_ui(tt(t))))
}

profile_chart <- function(name, core) {
  p <- utils::head(comp(name), 8)
  p$f <- sub("\\)$", "", sub(":[0-9]+\\(", " · ", sub("^.*/", "", p[["filename:lineno(function)"]])))
  p <- p[!duplicated(p$f), ]; p <- p[nrow(p):1, ]
  plot_ly() %>% add_trace(type = "bar", x = p$cumtime, y = p$f, orientation = "h", marker = list(color = ifelse(grepl(core, p$f, fixed = TRUE), HOT, BASE)),
                          text = sprintf("%.3f s", p$cumtime), textposition = "outside", cliponaxis = FALSE) %>%
    layout(xaxis = list(title = T("Tiempo acumulado (s)"), range = c(0, max(p$cumtime) * 1.25)), yaxis = cat_axis(p$f))
}

VARIANT_FAMILIES <- c("svm", "xgboost", "random_forest", "knn", "logistic", "naive_bayes")

eficiencia <- function() {
  e <- comp("coste_empirico"); e$n <- model_name(e$model)
  fe <- plot_ly() %>% add_trace(type = "bar", x = e$n, y = e$refit_seconds, name = T("Reajuste"), marker = list(color = BASE)) %>%
    add_trace(type = "bar", x = e$n, y = e$predict_seconds, name = T("Predicción del fold externo"), marker = list(color = HOT)) %>%
    layout(barmode = "group", xaxis = cat_axis(e$n), yaxis = list(title = T("Segundos (media de las unidades)")))
  th <- comp("complejidad_teorica"); th$model <- model_name(th$model); names(th) <- c("Familia", "Entrenamiento", "Predicción", "Memoria", "Supuestos")
  ex <- comp("escalamiento_exponentes"); ex <- ex[order(ex$exponente_n), ]
  fx <- plot_ly() %>% add_trace(type = "bar", x = ex$exponente_n, y = ex$model, orientation = "h", name = T("Exponente en n"), marker = list(color = highlight(ex$exponente_n)),
                                text = sprintf("%.2f", ex$exponente_n), textposition = "outside", cliponaxis = FALSE) %>%
    layout(shapes = list(vline(1, GREY, "dot"), vline(2, GREY, "dot")),
           annotations = list(list(text = T("lineal"), x = 1, y = 1, xref = "x", yref = "y domain", yanchor = "bottom", showarrow = FALSE),
                              list(text = T("cuadrático"), x = 2, y = 1, xref = "x", yref = "y domain", yanchor = "bottom", showarrow = FALSE)),
           xaxis = list(title = T("Exponente empírico (pendiente log-log)"), range = c(-1, 2.6)), yaxis = cat_axis(ex$model))
  v <- comp("estandar_vs_optimizado"); rf <- v[v$model == "random_forest", ]; par <- comp("paralelismo_resumen")
  lo <- par[par$backend == "loky", ]; th_ <- par[par$backend == "threading", ]
  fp <- plot_ly() %>%
    add_trace(type = "scatter", mode = "lines", x = c(1, 2, 4, 8), y = c(1, 2, 4, 8), name = T("Ideal lineal"), line = list(dash = "dash", color = GREY)) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = c(1, 2, 4, 8), y = rf[["aceleración_entrenamiento"]], name = T("Random Forest: entrenamiento (hilos)"), line = list(color = BASE, width = 3), marker = list(color = BASE)) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = c(1, 2, 4, 8), y = rf[["aceleración_inferencia"]], name = T("Random Forest: inferencia (hilos)"), line = list(color = SOFT, width = 2, dash = "dot"), marker = list(color = SOFT)) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = lo$n_jobs, y = lo$speedup, name = T("Validación cruzada RF–SMOTE (loky)"), line = list(color = HOT, width = 3), marker = list(color = HOT)) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = c(1, th_$n_jobs), y = c(1, th_$speedup), name = T("Validación cruzada RF–SMOTE (threading)"), line = list(color = HOT, width = 2, dash = "dash"), marker = list(color = HOT)) %>%
    layout(xaxis = list(title = T("Trabajadores"), tickvals = c(1, 2, 4, 8)), yaxis = list(title = T("Aceleración frente a 1 trabajador")),
           legend = list(y = -.24), margin = list(l = 20, r = 20, t = 20, b = 120))
  cmp_ <- v; cmp_$model <- model_name(cmp_$model)
  names(cmp_) <- c("Familia", "Variante", "Complejidad", "Entrenamiento (s)", "Inferencia (s)", "Memoria (MiB)", "Aceleración entrenamiento", "Aceleración inferencia", "ΔF1")
  bench <- comp("benchmark_trabajadores"); bench$model <- model_name(bench$model)
  names(bench) <- c("Familia", "Trabajadores pedidos", "Trabajadores efectivos", "Total (s)", "DE", "Entrenamiento (s)", "Inferencia (s)", "Memoria (MiB)", "F1 interno")
  scale_opts <- unique(comp("escalamiento")$model)
  tagList(kpis(kpi("LINEARSVC", 36, T("más rápido al entrenar que SVC RBF; ΔF1 −0.002"), "hot", 0, "×"), kpi(T("INFERENCIA LINEARSVC"), 931, T("más rápida que SVC RBF"), "", 0, "×"),
               kpi("XGBOOST HIST", 2.8, T("más rápido que el método exacto"), "", 1, "×"), kpi(T("VALIDACIÓN CRUZADA"), 1.8, T("con 4 trabajadores y el mismo F1"), "", 1, "×")),
          subhead(T("¿Dónde se va el tiempo?"), T("cProfile sobre los dos modelos más costosos: el ajuste de SVM–SMOTE y la predicción de KNN–SMOTE.")),
          grid(chart(T("Ajuste del Pipeline SVM–SMOTE · 1.20 s"), profile_chart("perfil_svm_fit", "_dense_fit"), T("El 81 % del ajuste (0.98 s) ocurre en el solver de libsvm (resaltado); preprocesar y aplicar SMOTE suma 0.06 s."), 330, FALSE),
               chart(T("Predicción del Pipeline KNN–SMOTE · 0.164 s"), profile_chart("perfil_knn_predict", "compute"), T("El 84 % de la predicción (0.138 s) es el cálculo de distancias por pares (resaltado). El cuello de botella es el algoritmo, no el Pipeline."), 330, FALSE)),
          subhead(T("Implementaciones alternativas"), T("Para cada familia se entrena la versión estándar (la de las 112 configuraciones) y versiones optimizadas con los mismos hiperparámetros; mediana de 5 repeticiones por fold.")),
          card(cls = "controls-card", field(T("Familia"), dropdown("eff-family", VARIANT_FAMILIES, "svm", setNames(as.list(model_name(VARIANT_FAMILIES)), VARIANT_FAMILIES))), uiOutput("eff-variants-output")),
          subhead(T("Escalamiento empírico frente a la complejidad teórica")),
          grid(chart(T("Exponente empírico en n (filas)"), fx, T("SVM RBF (resaltado) crece como n²: duplicar los datos cuadruplica el tiempo, como predice la teoría. Exponentes menores que 1 indican que dominan costos fijos con estos tamaños."), 380, FALSE),
               card(cls = "controls-card", field(T("Modelo"), dropdown("eff-scale", scale_opts, "svm rbf", setNames(as.list(scale_opts), scale_opts))), uiOutput("eff-scale-output"))),
          subhead(T("Paralelización y costo medido")),
          grid(chart(T("Aceleración medida por paralelismo"), fp, T("Random Forest entrena 2× más rápido con 4 hilos y casi no mejora con 8; en inferencia los hilos empeoran el tiempo. La validación cruzada llega a 1.8× con F1 idéntico."), 380),
               chart(T("Reajuste y predicción por familia"), fe, T("Random Forest y SVM tienen los reajustes más costosos; SVM y KNN, la predicción más cara. Coherente con sus cotas teóricas."), 380)),
          card(tags$h3(class = "chart-title", T("Complejidad asintótica de cada familia")), table_ui(tt(th), page_size = 7),
               tags$p(class = "insight", T("n: tamaño tras balancear; p: columnas; T: árboles; h: profundidad; B: bins; L: hojas; m: variables por nodo; I: iteraciones; n_sv: vectores soporte. Paralelizar reduce el tiempo de pared, no estas cotas."))),
          card(tags$h3(class = "chart-title", T("Tabla comparativa: modelo estándar frente a optimizado")), table_ui(tt(cmp_), page_size = 21),
               tags$p(class = "insight", T("Tres cambios ganan claramente sin perder más de 0.002 de F1: LinearSVC (o Nyström) en lugar de SVC RBF, hist en XGBoost y paralelizar Random Forest y la validación cruzada. Quedan como recomendación para el modelo propuesto."))),
          note(T("Equipo y política de memoria"), T(paste0("CPU Intel Core Ultra 5 125H (14 núcleos, 18 hilos), unos 8 GB de RAM y sin GPU CUDA. Durante las 112 configuraciones la política de memoria dejó un solo trabajador efectivo, ",
                                                          "por eso el paralelismo se midió después. Una caché del registro aceleró 8.08× la consulta de artefactos con resultados idénticos. XGBoost con device=cuda volvió a CPU: no se reporta aceleración por GPU."))),
          details(tagList(icon("table"), T("Benchmark de 1 y 2 trabajadores durante la corrida")), table_ui(tt(bench), page_size = 14)))
}

eff_variants <- function(family) {
  v <- comp("estandar_vs_optimizado"); v <- v[v$model == family, ]; v <- v[nrow(v):1, ]
  bars <- lapply(c("aceleración_entrenamiento", "aceleración_inferencia"), function(col)
    plot_ly() %>% add_trace(type = "bar", x = v[[col]], y = v$variant, orientation = "h", marker = list(color = highlight(v[[col]])),
                            text = sprintf("%.2f×", v[[col]]), textposition = "outside", cliponaxis = FALSE, showlegend = FALSE) %>%
      layout(xaxis = list(type = "log", title = T("veces más rápido (escala log)")), yaxis = cat_axis(v$variant)))
  f <- subplots(bars, c(T("Aceleración al entrenar"), T("Aceleración al predecir")), hspace = .04, share_y = TRUE)
  f$x$layout$shapes <- list(vline(1, GREY, "dot", "x"), vline(1, GREY, "dot", "x2", "y2 domain"))
  t <- v[nrow(v):1, c("variant", "complejidad", "entrenamiento_s", "inferencia_s", "delta_f1")]; names(t) <- c("Variante", "Complejidad", "Entrenamiento (s)", "Inferencia (s)", "ΔF1")
  div(graph(f, max(260, 70 + 52 * nrow(v)), FALSE), table_ui(tt(t), page_size = 6),
      tags$p(class = "insight", T("Valores mayores que 1 significan más rápido que la versión estándar (la primera fila); la más rápida está resaltada. ΔF1 compara con la estándar en el fold externo.")))
}

eff_scale <- function(model) {
  d <- comp("escalamiento"); d <- d[d$model == model, ]
  lines_ <- lapply(c("n", "p"), function(axis) {
    s <- d[d$axis == axis, ]; s <- s[order(s$value), ]
    plot_ly() %>% add_trace(type = "scatter", mode = "lines+markers", x = s$value, y = s$median_seconds, line = list(color = if (model == "svm rbf") HOT else BASE, width = 3),
                            marker = list(size = 9, color = if (model == "svm rbf") HOT else BASE), showlegend = FALSE) %>%
      layout(xaxis = list(type = "log", title = if (axis == "n") T("n (filas de entrenamiento)") else T("p (columnas, con ruido añadido)")),
             yaxis = list(type = "log", title = if (axis == "n") T("Mediana de 3 repeticiones (s)") else ""))
  })
  f <- subplots(lines_, c(T("Escalamiento en n"), T("Escalamiento en p")), hspace = .12)
  x <- comp("escalamiento_exponentes"); x <- x[x$model == model, ]
  div(graph(f, 300, FALSE), tags$p(class = "insight", fmt(T("Pendiente log-log: {n} en n y {p} en p. Teoría en n: {t}."), n = sprintf("%.2f", x$exponente_n), p = sprintf("%.2f", x$exponente_p), t = x[["teórico_n"]])))
}

effect_sizes <- function() {
  b <- comp("efecto_balanceo"); fams <- model_name(b$model); z <- as.matrix(b[c("ADASYN", "Ponderación", "SMOTE")]); cols <- T(colnames(z))
  txt <- matrix(sprintf("%+.2f", z), nrow(z))
  fh <- plot_ly(type = "heatmap", z = z, x = cols, y = fams, zmin = -1, zmax = 1, colorscale = DIVERGING, text = txt, texttemplate = "%{text}",
                colorbar = list(title = list(text = "δ"), thickness = 14), hovertemplate = "%{y} · %{x}<br>δ = %{z:.2f}<extra></extra>") %>%
    layout(yaxis = list(autorange = "reversed"))
  fin <- comp("efecto_finalistas"); fn <- function(x) T(unname(FINALIST_NAMES[x]))
  ft <- setNames(df(fn(fin$a), fn(fin$b), round(fin$mean_delta_f1, 4), fin$folds_a_higher, fin$cliff_5_folds, round(fin$cliff_15_unidades, 3), round(fin$d_z_15, 3), round(fin$rank_biserial_15, 3), T(fin$magnitud)),
                 c(T("Modelo A"), T("Modelo B"), T("ΔF1 medio"), T("Folds con A mayor"), T("δ de Cliff (5 folds)"), T("δ de Cliff (15 unidades)"), "d_z", T("Biserial de rangos"), T("Magnitud")))
  tagList(subhead(T("Tamaños del efecto"), T("Un p-valor pequeño solo dice que una diferencia es difícil de atribuir al azar, no que importe. δ de Cliff: despreciable si |δ| < 0.147, pequeño hasta 0.33, mediano hasta 0.474 y grande por encima.")),
          kpis(kpi(T("δ RF FRENTE A XGBOOST"), -0.06, T("despreciable · 15 unidades"), "", 2), kpi(T("δ ENSAMBLES FRENTE A LOGÍSTICA"), 0.10, T("despreciable · d_z 0.23–0.31"), "", 2),
               kpi(T("PARES NEMENYI CON δ = 1"), 31, T("ΔF1 de 0.063 a 0.076 · efectos grandes"), "hot"), kpi(T("SMOTE CON EFECTO GRANDE"), 5, T("de 7 familias (δ ≥ 0.68)"))),
          grid(chart(T("δ de Cliff del F1 frente a sin balanceo"), fh, T("Balancear tiene efecto grande en Random Forest, XGBoost, SVM y logística. GaussianNB es la excepción: ya incorpora la prevalencia y SMOTE no cambia nada."), 380, FALSE),
               card(tags$h3(class = "chart-title", T("Finalistas: elegir uno u otro casi da igual")), table_ui(ft),
                    tags$p(class = "insight", T("Los tres efectos entre finalistas son despreciables. Los 31 pares que Nemenyi separa enfrentan configuraciones balanceadas con SVM sin balanceo: son las distancias más extremas.")))))
}
