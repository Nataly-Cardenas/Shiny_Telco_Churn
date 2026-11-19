# Sección «Modelos finales» (modelos_ml.py del dash): tabla de líderes, predicción en vivo con SHAP, diagnóstico,
# interpretabilidad global, comparación estadística, modelo original y ficha del modelo.
# Solo carga artefactos ya entrenados (artifacts/): nunca entrena.

MEJOR <- "logistic_splines"
NOMBRES_M <- c(NOMBRES, mapem_sin_prior = "RegionalMAPEM sin prior", mapem_K1 = "Pesos globales (K = 1)", mapem_regiones_duras = "Regiones duras",
               promedio_simple = "Promedio simple", stacking_logistico = "Stacking logístico")
ABLACION <- c("mapem_sin_prior", "mapem_K1", "mapem_regiones_duras", "promedio_simple", "stacking_logistico")
CORTO <- c(RegionalMAPEM = "RegionalMAPEM", logistic_splines = "Splines", hist_gb = "HistGB", lightgbm = "LightGBM", catboost = "CatBoost", xgboost = "XGBoost",
           random_forest = "RF", logistic = "Logística", knn = "KNN", naive_bayes = "NB", decision_tree = "Árbol", svm = "SVM", mapem_sin_prior = "Sin prior",
           mapem_K1 = "K = 1", mapem_regiones_duras = "Duras", promedio_simple = "Promedio", stacking_logistico = "Stacking")
ALFA <- .10
MODELOS_TABS <- c(N("Tabla de líderes"), N("Predicción en vivo"), N("Diagnóstico"), N("Interpretabilidad global"), N("Comparación estadística"),
                  N("Modelo original"), N("Ficha del modelo"))

# Tablas de esta sección: encabezados en varias líneas, como la tabla de src/ui.py del dash.
tabla <- function(...) table_ui(..., head_wrap = TRUE)

nombre <- function(m) T(ifelse(m %in% names(NOMBRES_M), NOMBRES_M[m], m))
corto <- function(m) T(ifelse(m %in% names(CORTO), CORTO[m], m))
model_choices <- function() setNames(as.list(nombre(MODELOS)), MODELOS)

modelos_ml <- function(tab = 0) {
  v <- resultado("veredicto_R3.json")
  tagList(page_head(T("Modelos finales"), T("Doce modelos finales reentrenados con los 5634 clientes de desarrollo: once benchmarks calibrados y RegionalMAPEM, el modelo propio."), "06_validacion"),
          kpis(kpi(T("LOG-LOSS DEL MODELO PROPIO"), round(v$log_loss_original, 5), T("validación anidada 5 × 5"), "hot", 5),
               kpi(T("MEJOR BENCHMARK"), round(v$log_loss_mejor_benchmark, 5), T("logística con splines"), "", 5),
               kpi(T("DIFERENCIA"), sprintf("%+.5f", v$diferencia), T("no significativa con ninguna prueba")),
               kpi(T("VEREDICTO R3"), T("No superior"), T("empate con el mejor benchmark"))),
          tabs("modelos-tabs", T(MODELOS_TABS), tab))
}

modelos_tab <- function(i, input) {
  keep <- function(id, default) isolate(input[[id]]) %||% default
  body <- switch(i + 1, leaderboard(keep), prediccion(keep), diagnostico(keep), interpretabilidad(keep), comparacion(), original(), ficha())
  tab_body(body)
}

# ---------------------------------------------------------------- tabla de líderes
lideres <- function() cached("lideres", {
  r <- telco("resumen_modelos.csv")
  costos <- tapply(resultado("costos_telco.csv")$seg_pred_por_1000, resultado("costos_telco.csv")$modelo, mean)
  filas <- lapply(r$modelo, function(m) {
    x <- r[r$modelo == m, ]; meta <- if (m %in% MODELOS) metadata(m) else NULL
    data.frame(id = m, tipo = if (m == ORIGINAL) "original" else if (m %in% ABLACION) "ablacion" else "benchmark",
               ll = x$log_loss_media, ll_de = x$log_loss_de, lo = x$log_loss_ic_bajo, hi = x$log_loss_ic_alto, f1 = x$f1_media, f1_de = x$f1_de,
               auc = x$auc_media, auc_de = x$auc_de, brier = x$brier_media, ece = x$ece_media,
               hold = if (!is.null(meta)) meta$metricas_externas$holdout$log_loss else NA, ms = if (!is.null(meta)) meta$costo$ms_por_cliente_lote_200 else NA,
               ms_val = if (m %in% names(costos)) costos[[m]] else NA, mb = if (!is.null(meta)) meta$costo$tamano_mb else NA, stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, filas); d[order(d$ll), ]
})

tabla_lideres <- function(incluir_ablacion = FALSE) {
  d <- lideres(); if (!incluir_ablacion) d <- d[d$tipo != "ablacion", ]
  t <- setNames(df(seq_len(nrow(d)), nombre(d$id), c(original = T("Modelo propio"), benchmark = "Benchmark", ablacion = T("Ablación o combinador"))[d$tipo],
                   T("Ninguno"), "TPE · 30", "Platt", pm(d$ll, d$ll_de), round(d$ll, 5), sprintf("[%.4f, %.4f]", d$lo, d$hi),
                   pm(d$f1, d$f1_de, 3), round(d$f1, 4), pm(d$auc, d$auc_de, 3), round(d$auc, 4), round(d$brier, 4), round(d$ece, 4),
                   round(d$hold, 4), round(d$ms, 3), round(d$mb, 3)),
                c("#", T("Modelo"), T("Tipo"), T("Balanceo"), T("Optimizador"), T("Calibración"), T("Log-loss (media ± DE)"), T("Log-loss"), T("IC 95 % log-loss"),
                  T("F1 (media ± DE)"), "F1", T("AUC (media ± DE)"), "AUC", "Brier", "ECE", T("Log-loss hold-out"), T("ms por cliente"), T("Tamaño (MB)")))
  tips <- setNames(list(T("Intervalo t sobre las 25 evaluaciones externas"), T("Predicción de un lote de 200 clientes, en milisegundos por cliente")), c(T("IC 95 % log-loss"), T("ms por cliente")))
  tabla(t, page_size = 17, filter = TRUE, digits = 5, hot_rows = which(d$id == ORIGINAL), tooltips = tips)
}

leaderboard <- function(keep) {
  d <- lideres(); d <- d[d$tipo != "ablacion", ]
  f <- bars_ci(nombre(d$id), d$ll, d$lo, d$hi, nombre(ORIGINAL), T("Log-loss medio externo (menor es mejor) · IC 95 %"))
  e2 <- SUMMARY[match(BEST_BY_FAMILY, SUMMARY$experiment_id), ]; e2 <- e2[order(-e2$f1_positive_mean), ]
  rangos <- read("estadistica", "rangos_medios.csv"); rk <- setNames(rangos$mean_rank, rangos$experiment_id)
  cd <- jsonlite::fromJSON(file.path(DATA, "estadistica", "nemenyi_estado.json"))$critical_difference; mejor <- min(rk)
  tb <- setNames(df(model_name(e2$model), balance_name(e2$balance), optimizer_name(e2$optimizer), pm(e2$f1_positive_mean, e2$f1_positive_std, 3),
                    pm(e2$roc_auc_mean, e2$roc_auc_std, 3), round(rk[e2$experiment_id], 1), ifelse(rk[e2$experiment_id] - mejor <= cd, T("Sí"), "No"),
                    c(T("Mejor combinación global"), rep(T("Mayor F1 medio externo de su familia"), nrow(e2) - 1))),
                 c(T("Familia"), T("Balanceo"), T("Optimizador"), T("F1 (media ± DE)"), T("AUC (media ± DE)"), T("Rango medio Friedman"), T("Finalista Nemenyi"), T("Motivo")))
  tagList(note(T("Cómo se eligieron"), T(paste0("Cada benchmark se optimizó con TPE (30 evaluaciones por pliegue externo) minimizando el log-loss del pipeline calibrado con Platt, sin remuestreo. ",
                                               "La tabla resume las 25 evaluaciones externas (5 repeticiones × 5 pliegues); el modelo propio está resaltado. Ordena o filtra cualquier columna."))),
          card(check("lideres-ablacion", T("Incluir la ablación y los combinadores estándar"), isTRUE(keep("lideres-ablacion", FALSE))), uiOutput("lideres-tabla")),
          chart(T("Log-loss externo con su intervalo de confianza"), f, T("Los intervalos de los cinco primeros se solapan casi por completo: RegionalMAPEM (resaltado) y la logística con splines empatan."), 480, FALSE),
          subhead(T("Mejor combinación de cada modelo base"), T("Selección sobre las 112 configuraciones (7 modelos × 4 balanceos × 4 optimizadores), por F1 medio externo.")),
          card(tabla(tb, page_size = 7), tags$p(class = "insight", fmt(T("Finalista Nemenyi: su rango medio está a menos de una diferencia crítica ({cd}) del mejor. Las siete mejores combinaciones quedan dentro del grupo de no rechazo."), cd = sprintf("%.1f", cd))),
               tags$p(class = "muted", T("Los modelos de la predicción en vivo son versiones re-optimizadas sin balanceo, con el log-loss como criterio; por eso no coinciden con estas combinaciones, que se muestran como referencia de la comparación previa."))))
}

# ---------------------------------------------------------------- predicción en vivo
# Umbral conforme al 90 % con las predicciones externas de la validación (5634) y su cobertura en el hold-out.
conforme <- function(modelo) cached(paste0("conf_", modelo), {
  P <- predicciones_finales(); v <- P[P$evaluacion == "validacion_r0", ]; h <- P[P$evaluacion == "holdout", ]
  s <- sort(ifelse(v$y == 1, 1 - v[[modelo]], v[[modelo]])); n <- length(s)
  q <- s[ceiling(min(1, ceiling((n + 1) * (1 - ALFA)) / n) * (n - 1)) + 1]   # numpy.quantile(method = "higher")
  inc1 <- 1 - h[[modelo]] <= q; inc0 <- h[[modelo]] <= q
  list(q = q, cobertura = mean(ifelse(h$y == 1, inc1, inc0)), tamano = mean(inc1 + inc0))
})

GRUPOS_FORM <- list(list(N("Cuenta"), c("tenure", "Contract", "PaymentMethod", "PaperlessBilling", "MonthlyCharges", "TotalCharges")),
                    list(N("Servicios"), c("PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")),
                    list(N("Demografía"), c("gender", "SeniorCitizen", "Partner", "Dependents")))
EJEMPLO <- function() TEST_IDS[4]

cliente_valores <- function(row) {
  c <- rows(row)
  setNames(lapply(ENTRADA, function(k) if (k %in% c("tenure", "SeniorCitizen")) as.integer(c[[k]]) else if (k %in% c("MonthlyCharges", "TotalCharges")) (if (is.na(c[[k]])) NULL else c[[k]]) else c[[k]]), ENTRADA)
}

control <- function(c, valor) {
  e <- esquema()[[c]]
  if (e$tipo == "categórica") return(dropdown(paste0("in-", c), unlist(e$categorias), valor))
  if (c == "SeniorCitizen") return(dropdown(paste0("in-", c), c("0", "1"), as.character(valor), list(`0` = cat_name(0), `1` = cat_name(1))))
  num_input(paste0("in-", c), e$min, e$max, if (c == "tenure") 1 else .01, valor, sprintf("%s – %s", format(e$min), format(e$max)))
}

prediccion <- function(keep) {
  if (length(disponibles()) < length(MODELOS))
    return(alert(T("Faltan artefactos"), T("No se encontraron todos los modelos en artifacts/. Ejecuta scripts/construir_artefactos.py; el tablero no entrena modelos."), "danger"))
  h <- rows(TEST_IDS)
  ejemplos <- setNames(as.list(as.character(TEST_IDS)), paste(h$customerID, "·", ifelse(h$target == 1, T("abandonó"), T("permaneció"))))
  fila <- as.integer(keep("pred-ejemplo", EJEMPLO())); val <- cliente_valores(fila)
  form <- lapply(GRUPOS_FORM, function(g) div(class = "form-group", tags$h4(T(g[[1]])),
    div(class = "g-3 row", lapply(g[[2]], function(c) div(class = "col-xl-4 col-md-6 col-12", field(paste(label(c), "·", c), control(c, keep(paste0("in-", c), val[[c]]))))))))
  tagList(div(class = "pred-layout",
      card(cls = "controls-card",
           div(class = "controls",
               field(T("Modelo"), dropdown("pred-modelo", MODELOS, keep("pred-modelo", ORIGINAL), model_choices())),
               field(T("Cargar un cliente del hold-out"), selectizeInput("pred-ejemplo", NULL, choices = ejemplos, selected = as.character(fila), width = "100%",
                                                                         options = list(dropdownParent = "body", searchField = "label"))),
               tags$button(id = "pred-azar", type = "button", class = "btn ghost action-button", icon("shuffle"), T("Cliente al azar"))),
           tags$p(class = "muted", T("El formulario se genera con el esquema de entrada guardado en metadata.json: tipos, categorías válidas y rangos observados en el desarrollo. Puedes editar cualquier campo.")),
           form,
           div(class = "controls",
               field(T("Umbral de alerta"), radio("pred-umbral-modo", c("f1", "custom"), c(T("El que maximiza el F1 en validación"), T("Personalizado")), keep("pred-umbral-modo", "f1"), "radio-row")),
               field(T("Valor del umbral personalizado"), slider("pred-umbral", .05, .95, .01, keep("pred-umbral", .5), c(`0.1` = "0.10", `0.3` = "0.30", `0.5` = "0.50", `0.7` = "0.70", `0.9` = "0.90")))),
           tags$p(class = "muted", T("Bajar el umbral detecta más clientes en riesgo pero dispara más falsas alertas. Elegirlo bien requiere conocer el costo de cada contacto y de cada cliente perdido.")),
           div(class = "export-row", tags$button(id = "pred-boton", type = "button", class = "btn primary action-button", icon("target"), T("Predecir")),
               tags$small(class = "muted", T("La explicación SHAP tarda unos segundos para RegionalMAPEM: combina a 11 modelos.")))),
      div(class = "pred-out", uiOutput("pred-salida"))),
    note(T("Qué muestra la predicción"), T(paste0("Probabilidad calibrada de abandono, la decisión con el umbral que maximiza el F1 en la validación, el conjunto conforme al 90 % y la explicación SHAP local. ",
                                                 "Es una herramienta de apoyo: no reemplaza la revisión humana antes de actuar sobre un cliente."))))
}

.shap_cache <- new.env()
explicar <- function(modelo, X) {
  key <- paste(modelo, paste(unlist(X[1, ]), collapse = "|"))
  if (is.null(.shap_cache[[key]])) {
    t0 <- Sys.time(); s <- shap_local(modelo, X)
    .shap_cache[[key]] <- c(s, list(seg = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  .shap_cache[[key]]
}

prediccion_salida <- function(modelo, entrada, modo = "f1", umbral = .5) {
  if (identical(entrada$tenure, 0L) && is.null(entrada$TotalCharges)) entrada$TotalCharges <- 0
  X <- tryCatch(validar(entrada), entrada_invalida = function(e) e)
  if (inherits(X, "entrada_invalida")) return(alert(T("Revisa los datos del cliente"), conditionMessage(X), "warning"))
  res <- tryCatch({ p <- predecir(modelo, X); list(p = p, e = explicar(modelo, X)) },
                  error = function(e) e)   # un error del modelo no debe detener la aplicación
  if (inherits(res, "error")) return(alert(T("No se pudo calcular la predicción"), paste0(class(res)[1], ": ", conditionMessage(res)), "danger"))
  p <- res$p; ex <- res$e; meta <- metadata(modelo); u <- if (identical(modo, "custom") && !is.null(umbral)) umbral else meta$umbral_f1; cf <- conforme(modelo)
  conjunto <- c(if (1 - p <= cf$q) T("Abandona"), if (p <= cf$q) T("Permanece"))
  nivel <- if (p >= u) HOT else BASE
  etiqueta <- function(c) { v <- X[[c]]; paste(label(c), "=", if (is.character(v)) cat_name(v) else format(v)) }
  fw <- shap_waterfall(ex$valores, ex$base, etiqueta)
  out <- tagList(
    kpis(kpi(T("PROBABILIDAD DE ABANDONO"), round(p * 100, 1), nombre(modelo), if (p >= u) "hot" else "", 1, " %"),
         kpi(T("DECISIÓN"), if (p >= u) T("Alerta de retención") else T("Sin alerta"), fmt(if (identical(modo, "custom")) T("umbral personalizado = {u}") else T("umbral F1 = {u}"), u = sprintf("%.3f", u))),
         kpi(T("CONJUNTO CONFORME 90 %"), if (length(conjunto)) paste(conjunto, collapse = " + ") else "∅", fmt(T("cobertura en hold-out: {c}"), c = pct(cf$cobertura))),
         kpi(T("SHAP CALCULADO EN"), sprintf("%.1f s", ex$seg), fmt(T("{n} clientes de referencia"), n = SHAP_FONDO))),
    grid(card(tags$h3(class = "chart-title", T("Probabilidad predicha")), graph(gauge(p, nombre(modelo), nivel), 260, FALSE, list(l = 34, r = 34, t = 50, b = 10)),
              tags$p(class = "insight", fmt(T("La aguja es la probabilidad calibrada. Con el umbral de alerta ({u}), este cliente {d}."), u = sprintf("%.2f", u),
                                            d = if (p >= u) T("recibe una alerta") else T("no recibe alerta")))),
         card(tags$h3(class = "chart-title", T("Incertidumbre: predicción conforme")),
              tags$p(fmt(T(paste0("Con cobertura nominal del 90 %, el conjunto de predicción contiene las clases cuya puntuación de no conformidad no supera q = {q}. ",
                                  "En el hold-out, estos conjuntos contienen la clase real en el {c} de los clientes, con {t} clases en promedio.")),
                         q = sprintf("%.3f", cf$q), c = pct(cf$cobertura), t = sprintf("%.2f", cf$tamano))),
              tags$p(class = "insight", if (length(conjunto) == 2) T("Un conjunto con las dos clases significa que el modelo no tiene certeza suficiente sobre este cliente.")
                                        else T("Un conjunto con una sola clase indica una predicción con la confianza requerida.")))),
    chart(T("Explicación SHAP local"), fw, fmt(T("Cada barra es lo que una variable mueve la probabilidad desde la media de referencia ({b}) hasta la de este cliente ({p}). Resaltado: empuja hacia el abandono."),
                                              b = sprintf("%.3f", ex$base), p = sprintf("%.3f", p)), 460, FALSE))
  if (modelo == ORIGINAL) {
    e <- artefacto(ORIGINAL); R <- regiones(X)[1, ]; W <- drop(R %*% e$weights); names(W) <- e$expertos; s <- sort(W)
    fp <- plot_ly() %>% add_trace(type = "bar", x = unname(s), y = corto(names(s)), orientation = "h", marker = list(color = highlight(s)), text = sprintf("%.2f", s), textposition = "outside") %>%
      layout(xaxis = list(title = T("Peso efectivo del experto para este cliente"), range = c(0, max(.5, max(s) * 1.25))), yaxis = cat_axis(corto(names(s))))
    out <- tagList(out, chart(T("Pesos de los expertos para este cliente"), fp,
                              fmt(T("Pertenencia a las {k} regiones: {r}. Los pesos efectivos son la mezcla de los pesos de cada región; el experto con más peso está resaltado."),
                                  k = length(R), r = paste(sprintf("%.0f%%", 100 * R), collapse = ", ")), 360, FALSE))
  }
  div(out)
}

# ---------------------------------------------------------------- diagnóstico
diagnostico <- function(keep) {
  tagList(card(cls = "controls-card", div(class = "controls",
                field(T("Modelo"), dropdown("diag-modelo", MODELOS, keep("diag-modelo", ORIGINAL), model_choices())),
                field(T("Evaluación"), radio("diag-eval", c("validacion_r0", "holdout"), c(T("Validación anidada (repetición 0, 5634 clientes)"), T("Hold-out (1409 clientes, una sola vez)")),
                                             keep("diag-eval", "validacion_r0"))))),
          uiOutput("diag-salida"))
}

ece <- function(y, p, bins = 10) {
  idx <- pmin(floor(p * bins), bins - 1)
  sum(vapply(unique(idx), function(b) mean(idx == b) * abs(mean(y[idx == b]) - mean(p[idx == b])), 0))
}
log_loss <- function(y, p) -mean(y * log(p) + (1 - y) * log(1 - p))
f1_score <- function(y, pred) { tp <- sum(y == 1 & pred == 1); fp <- sum(y == 0 & pred == 1); fn <- sum(y == 1 & pred == 0); if (tp == 0) 0 else 2 * tp / (2 * tp + fp + fn) }

diagnostico_salida <- function(modelo, evaluacion) {
  P <- predicciones_finales(); d <- P[P$evaluacion == evaluacion, ]
  U <- resultado("umbrales.csv"); rn <- U[[1]]
  u <- vapply(d$pliegue, function(f) U[[paste0("u_", modelo)]][rn == paste0(evaluacion, "_", f)], 0)
  y <- d$y; p <- pmin(pmax(d[[modelo]], 1e-6), 1 - 1e-6); pred <- as.integer(p >= u)
  cm <- confusion(y, pred); tp <- cm[2, 2]; fn <- cm[2, 1]; fp_ <- cm[1, 2]
  fuente <- if (evaluacion == "validacion_r0") T("5 pliegues externos de la repetición 0; cada pliegue usa su propio umbral") else T("hold-out, modelo final entrenado con los 5634 clientes")
  a_ <- auc(y, p)
  div(eyebrow(paste(nombre(modelo), "·", fuente)),
      kpis(kpi("LOG-LOSS", round(log_loss(y, p), 4), T("menor es mejor"), "hot", 4), kpi("F1", round(f1_score(y, pred), 3), T("umbral que maximiza F1 en validación"), "", 3),
           kpi("AUC", round(a_, 3), "", "", 3), kpi("BRIER", round(mean((p - y)^2), 4), fmt(T("ECE = {e}"), e = sprintf("%.3f", ece(y, p))), "", 4)),
      grid(chart(T("Matriz de confusión"), conf_heat(cm, 17), fmt(T("Detecta {tp} de {n} abandonos con {fp} falsas alertas."), tp = tp, n = tp + fn, fp = fp_), 330, FALSE),
           chart(T("Diagrama de calibración"), reliability(y, p), T("Más cerca de la diagonal = probabilidades mejor calibradas. El tamaño del punto indica cuántos clientes hay en el intervalo."), 330)),
      grid(chart(T("Curva ROC"), roc_fig(y, p, sprintf("AUC = %.3f", a_)), T("Capacidad de ordenar a los clientes por riesgo."), 330),
           chart(T("Curva precisión-recall"), pr_fig(y, p, nombre(modelo)), T("La línea gris es la prevalencia del abandono: lo que lograría un modelo sin información."), 330)))
}

# ---------------------------------------------------------------- interpretabilidad global
shap_largo <- function() cached("shap_largo", {
  S <- resultado("shap_global.csv")
  ids <- unique(S$row_id); X <- rows(ids)[ENTRADA]; X$TotalCharges[is.na(X$TotalCharges)] <- 0
  tasas <- rows(TRAIN_IDS)
  color <- lapply(ENTRADA, function(c) {
    if (c %in% NUMERIC) return(rank(X[[c]], ties.method = "average") / nrow(X))
    t <- tapply(tasas$target, tasas[[c]], mean); t <- (t - min(t)) / max(max(t) - min(t), 1e-12)
    unname(t[as.character(X[[c]])])
  })
  names(color) <- ENTRADA
  largo <- do.call(rbind, lapply(ENTRADA, function(c) data.frame(modelo = S$modelo, row_id = S$row_id, variable = c, shap = S[[c]],
                                                                 color = color[[c]][match(S$row_id, ids)], stringsAsFactors = FALSE)))
  largo
})

interpretabilidad <- function(keep) {
  tagList(card(cls = "controls-card", field(T("Modelo"), dropdown("shap-modelo", MODELOS, keep("shap-modelo", ORIGINAL), model_choices()))),
          uiOutput("shap-salida"),
          note(T("Explicar no es causar"), T("Las atribuciones describen cómo decide el modelo, no qué provoca el abandono.")))
}

shap_salida <- function(modelo) {
  s <- shap_largo(); s <- s[s$modelo == modelo, ]
  imp <- sort(tapply(abs(s$shap), s$variable, mean))
  fb <- plot_ly() %>% add_trace(type = "bar", x = unname(imp), y = label(names(imp)), orientation = "h", marker = list(color = highlight(imp)), text = sprintf("%.3f", imp), textposition = "outside") %>%
    layout(xaxis = list(title = T("|SHAP| medio (cambio en la probabilidad de abandono)")), yaxis = cat_axis(label(names(imp))))
  top <- rev(names(imp))[1:3]
  grid(chart(T("Resumen SHAP · 150 clientes del hold-out"), shap_beeswarm(s, label),
             T("Cada punto es un cliente. A la derecha empuja hacia el abandono. Color: valor de la variable (en las categóricas, alto = categoría con más abandono en el desarrollo)."), 560, FALSE),
       chart(T("Importancia media"), fb, fmt(T("Las tres variables con más peso son {a}, {b} y {c}."), a = label(top[1]), b = label(top[2]), c = label(top[3])), 560, FALSE))
}

# ---------------------------------------------------------------- comparación estadística
comparacion <- function() {
  fr <- resultado("friedman_telco.json"); rangos <- fr$rangos
  f <- cd_diagram(rangos, fr$CD, ORIGINAL, setNames(as.list(corto(names(rangos))), names(rangos)))
  fam <- resultado("familia_resultados.csv"); t <- fam[fam$calculada %in% c(TRUE, "True"), ]
  ent <- function(x) ifelse(is.na(x), "<NA>", format(as.integer(x)))
  vista <- setNames(df(t$bloque, t$dataset, t$metrica, t$prueba, ifelse(is.na(t$modelo_b) | t$modelo_b == "", NA, nombre(t$modelo_b)), t$dif_media, t$p_crudo, t$p_holm, t$cliff, t$magnitud,
                       paste0(ent(t$pliegues_gana), "/", ent(t$n_pliegues))),
                    c(T("Bloque"), T("Conjunto"), T("Métrica"), T("Prueba"), T("Rival"), T("Diferencia media"), "p", T("p Holm"), "Cliff δ", T("Magnitud"), T("Pliegues que gana")))
    rechazos <- sum(t$p_holm < .05, na.rm = TRUE)
  tagList(kpis(kpi("FRIEDMAN F_F", round(fr$F_F, 1), sprintf("p = %s", sub("e-0?", "e-", sprintf("%.1e", fr$p))), "", 1), kpi(T("MODELOS · BLOQUES"), paste(fr$k, "·", fr$N), T("modelos × evaluaciones externas")),
               kpi(T("DIFERENCIA CRÍTICA"), round(fr$CD, 2), "Nemenyi, α = 0.05", "", 2), kpi(T("CONTRASTES CON HOLM"), nrow(t), fmt(T("{r} rechazan H0"), r = rechazos))),
          chart(T("Diagrama de diferencia crítica · Telco, log-loss"), f, T(paste0("Rango medio de cada modelo en las 25 evaluaciones externas (1 = mejor). Las barras azules unen modelos que Nemenyi no distingue; ",
                                                                                    "RegionalMAPEM (resaltado) comparte grupo con la logística con splines, LightGBM, HistGradientBoosting y su propia ablación.")), 520, FALSE),
          card(tags$h3(class = "chart-title", T("Familia confirmatoria de contrastes")),
               tags$p(class = "muted", T("Las 223 comparaciones pre-registradas del modelo propio contra cada rival, con p ajustado por Holm sobre toda la familia. Filtra por bloque, prueba o rival.")),
               tabla(vista, page_size = 12, filter = TRUE, digits = 5),
               tags$p(class = "insight", T("Contra la logística con splines en Telco: p Holm = 1 en las tres pruebas y δ de Cliff = −0,008, muy por debajo del umbral de 0,147."))))
}

# ---------------------------------------------------------------- modelo original
PSEUDO <- paste(sep = "\n",
  "Entrada: X (n × p), y, OOF[n × 11] de los expertos calibrados, K, c, covarianza",
  "1. Z  ← cuantiles normales de las columnas continuas de X → 2 componentes principales",
  "2. r  ← responsabilidades de un GMM con K componentes sobre Z (EM propio, 3 inicios)",
  "3. g  ← pesos globales: EM con K = 1 y sin prior",
  "4. α_k ← 1 + c · g                           (prior Dirichlet hacia lo global)",
  "5. π_k ← 1/11 para cada región k",
  "6. repetir hasta que el cambio relativo de F sea ≤ 1e-9:",
  "       E: q_ikm ∝ r_ik · π_km · L_im          (L_im = verosimilitud del experto m)",
  "       M: π_km ∝ Σ_i q_ikm + α_km − 1",
  "7. predecir: p(x) = Σ_k r_k(x) Σ_m π_km p_m(x)")

ESCENARIOS <- c(ruido_x_05 = N("Ruido en X, 5 %"), ruido_x_10 = N("Ruido en X, 10 %"), ruido_x_20 = N("Ruido en X, 20 %"), ruido_y_05 = N("Etiquetas cambiadas, 5 %"),
                ruido_y_10 = N("Etiquetas cambiadas, 10 %"), atipicos_01 = N("Atípicos, 1 %"), atipicos_05 = N("Atípicos, 5 %"), n_10 = N("10 % de la muestra"),
                n_25 = N("25 % de la muestra"), n_50 = N("50 % de la muestra"))

original <- function() {
  v <- resultado("veredicto_R3.json"); conv <- resultado("convergencia.json")
  lp <- conv$log_posterior
  fc <- plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = seq(0, conv$n_iter, length.out = length(lp)), y = lp, line = list(color = BASE, width = 2.4), name = "F(π)") %>%
    layout(xaxis = list(title = T("Iteración del MAP-EM")), yaxis = list(title = T("Log-posterior F(π)")))
  busq <- conv$busqueda_log_loss_interno
  fs <- plot_ly()
  for (m in c(ORIGINAL, MEJOR, "hist_gb", "lightgbm")) {
    acc <- cummin(busq[[m]])
    fs <- add_trace(fs, type = "scatter", mode = "lines", x = seq_along(acc), y = acc, name = nombre(m),
                    line = list(color = if (m == ORIGINAL) HOT else if (m == MEJOR) BASE else GREY, width = if (m == ORIGINAL) 3 else 2))
  }
  fs <- layout(fs, xaxis = list(title = T("Evaluaciones de TPE")), yaxis = list(title = T("Mejor log-loss interno acumulado")))
  fam <- resultado("familia_resultados.csv")
  h <- fam[fam$modelo_b %in% MEJOR & fam$bloque %in% c("A1_principal_benchmarks", "A3_principal_friedman", "C_holdout"), ]
  pruebas <- setNames(df(ifelse(is.na(h$descripcion) | h$descripcion == "", h$prueba, h$descripcion), h$dataset, h$metrica, h$dif_media, h$p_crudo, h$p_holm, h$cliff, h$p_holm < .05),
                      c(T("Prueba"), T("Conjunto"), T("Métrica"), T("Diferencia media"), "p", T("p Holm"), "Cliff δ", T("Rechaza H0")))
  rob <- resultado("robustez_resumen.csv"); rob <- rob[rob$modelo %in% c(ORIGINAL, MEJOR), ]
  sem <- rob[startsWith(rob$escenario, "semilla"), ]
  fb <- plot_ly()
  for (m in c(ORIGINAL, MEJOR)) fb <- add_trace(fb, type = "box", y = sem$log_loss[sem$modelo == m], name = nombre(m), marker = list(color = if (m == ORIGINAL) HOT else BASE),
                                                line = list(color = if (m == ORIGINAL) HOT else BASE), boxpoints = "all", jitter = .4, pointpos = 0)
  fb <- layout(fb, yaxis = list(title = T("Log-loss externo (10 semillas × 5 pliegues)")), xaxis = cat_axis(nombre(c(ORIGINAL, MEJOR))))
  esc <- rob[!startsWith(rob$escenario, "semilla") & rob$escenario != "conforme", ]
  g <- tapply(esc$log_loss, list(esc$escenario, esc$modelo), mean)
  resumen <- setNames(df(T(ifelse(rownames(g) %in% names(ESCENARIOS), ESCENARIOS[rownames(g)], rownames(g))), round(g[, ORIGINAL], 4), round(g[, MEJOR], 4), round(g[, ORIGINAL] - g[, MEJOR], 4)),
                      c(T("Escenario"), "RegionalMAPEM", T("Logística con splines"), T("Diferencia")))
  W <- resultado("pesos_por_region_r0_f0.csv"); Wm <- as.matrix(W[-1])
  fw <- heatmap(Wm, corto(names(W)[-1]), vapply(seq_len(nrow(Wm)), function(k) fmt(T("Región {k}"), k = k), ""), SEQ, digits = 2, showscale = FALSE) %>% layout(yaxis = list(autorange = "reversed"))
  condiciones <- list(list(T("Mejor log-loss medio que el mejor benchmark"), v$condicion_1), list(T("Al menos 3 pruebas significativas tras Holm"), v$condicion_2),
                      list(T("|δ de Cliff| ≥ 0,147"), v$condicion_3), list(T("Consistencia en los 3 conjuntos adicionales"), v$condicion_4))
  tagList(
    card(cls = "glow", div(class = "verdict",
         div(eyebrow(T("VEREDICTO EXPLÍCITO")), tags$h2(T("RegionalMAPEM no es superior al mejor benchmark")),
             tags$p(fmt(T("Empata con la logística con splines en Telco (diferencia de {d} en log-loss, δ de Cliff = {c}) y solo es el mejor en media en Iranian. Cumple 1 de las 4 condiciones pre-registradas de superioridad (R3)."),
                        d = sprintf("%+.5f", v$diferencia), c = sprintf("%+.3f", v$cliff)))),
         tags$ul(class = "checks", lapply(condiciones, function(x) tags$li(class = if (isTRUE(x[[2]])) "ok" else "bad", icon(if (isTRUE(x[[2]])) "check" else "x", "sm"), " ", x[[1]]))))),
    grid(card(tags$h3(class = "chart-title", T("Descripción")),
              md(T(paste0("RegionalMAPEM combina las probabilidades de los 11 benchmarks calibrados con pesos que cambian según la región del cliente: ",
                          "$p(y=1\\mid x)=\\sum_k r_k(x)\\sum_m \\pi_{km}\\,p_m(x)$. Las regiones salen de un GMM sobre dos componentes principales de las variables continuas, ",
                          "y los pesos se estiman por máxima posterior con EM, con un prior de Dirichlet $\\pi_k\\sim\\mathrm{Dir}(1+c\\,g)$ que los encoge hacia los pesos globales $g$. ",
                          "Con regiones fijas la log-posterior es cóncava y el EM converge al óptimo global."))),
              tags$h3(class = "chart-title", T("Diferencias con la literatura")),
              tags$ul(lapply(c(N("Frente a la mezcla de expertos con EM: los expertos no se reentrenan y la compuerta no es supervisada; solo se estiman los pesos."),
                               N("Frente a la selección dinámica: no elige un clasificador por vecindad, mezcla a todos con pesos suaves."),
                               N("Frente al stacking local: no ajusta un metamodelo por región, sino pesos en el símplex con un prior hacia lo global."),
                               N("Frente a un ensamble ponderado: con K = 1 se reduce a él; el prior controla cuánto se aleja cada región.")), function(x) tags$li(T(x))))),
         card(tags$h3(class = "chart-title", T("Pseudocódigo")), div(class = "prose code", HTML(paste0("<pre><code>", htmlEscape(PSEUDO), "</code></pre>"))),
              tags$p(class = "insight", T("Implementación propia en numpy y scipy (src/original_model.py), con 12 pruebas unitarias que corren en integración continua.")))),
    grid(chart(T("Convergencia del MAP-EM · modelo final"), fc, fmt(T("La log-posterior nunca baja y se estabiliza tras {n} iteraciones (K = {k}, c = {c}, covarianza {cov})."),
                                                                  n = conv$n_iter, k = conv$params$K, c = sprintf("%.2f", conv$params$c), cov = conv$params$covarianza), 340, FALSE),
         chart(T("Curva de la búsqueda de hiperparámetros"), fs, T("Mejor log-loss interno acumulado en las 30 evaluaciones de la búsqueda final (5634 clientes)."), 340)),
    card(tags$h3(class = "chart-title", T("Pruebas de hipótesis frente a la logística con splines")), tabla(pruebas, page_size = 8, digits = 5, wrap = TRUE),
         tags$p(class = "insight", T("Ninguna prueba rechaza H0 tras Holm. El McNemar del hold-out (F1) tiene p crudo bajo, pero no sobrevive a la corrección."))),
    subhead(T("Resumen de los estudios de robustez"), T("Repetición 0, cinco pliegues externos; detalle completo en el capítulo 7 del libro.")),
    grid(chart(T("Estabilidad por semilla"), fb, T("La logística con splines no depende de la semilla; RegionalMAPEM cambia menos de 0,0005 entre semillas, muy por debajo de la variación entre pliegues."), 360, FALSE),
         card(tags$h3(class = "chart-title", T("Ruido, atípicos y tamaño de muestra")), tabla(resumen, page_size = 10),
              tags$p(class = "insight", T("Diferencia negativa: el modelo propio tiene menor log-loss. Ninguna diferencia pasa de 0,003 y el signo cambia entre escenarios.")))),
    chart(T("Pesos por región · repetición 0, pliegue 0"), fw, T("Cada fila es una región. Los pesos cambian entre regiones (HistGradientBoosting va de 0,24 a 0,54), pero el modelo con pesos globales (K = 1) logra el mismo log-loss: en Telco las regiones no aportan."), 300, FALSE))
}

# ---------------------------------------------------------------- ficha del modelo
ficha <- function() {
  meta <- metadata(ORIGINAL); hp <- meta$hiperparametros
  filas <- setNames(df(c(T("Creado"), T("Commit del experimento"), T("Semilla"), T("Hiperparámetros"), T("Entrenamiento"), "scikit-learn · numpy · pandas", T("Tamaño"), "SHA-256"),
                       c(meta$creado, substr(meta$fuente$commit, 1, 12), paste(meta$semilla$valor, "·", meta$semilla$derivacion), sprintf("K = %s, c = %.4f, %s", hp$K, hp$c, hp$covarianza),
                         fmt(T("{n} clientes, {k} abandonos"), n = meta$entrenamiento$clientes, k = meta$entrenamiento$abandonos),
                         paste(meta$versiones$`scikit-learn`, "·", meta$versiones$numpy, "·", meta$versiones$pandas), paste(meta$costo$tamano_mb, "MB +", T("11 expertos")),
                         paste0(substr(meta$verificacion$sha256, 1, 16), "…"))), c(T("Campo"), T("Valor")))
  grid(card(cls = "model-card", md(documento("MODEL_CARD.md"))),
       div(card(tags$h3(class = "chart-title", T("Artefacto cargado")), tabla(filas, page_size = 10, wrap = TRUE),
                div(class = "author-links", lapply(c(ORIGINAL, MEJOR), function(m) tags$a(class = "chip-link", href = sprintf("artifacts/%s/metadata.json", m), target = "_blank", icon("download"), "metadata.json")))),
           note(T("Usos no recomendados"), T("Decidir de forma automática sobre un cliente sin revisión humana, explicar causas del abandono o aplicarlo a otra población sin volver a validarlo."), "amber")))
}
