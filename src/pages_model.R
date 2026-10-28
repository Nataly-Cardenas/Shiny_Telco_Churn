# 3. Preprocesamiento y 4. Modelado avanzado.

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

# Curvas ROC que se trazan con un botón de play (mismo tipo de gráfica que el libro).
roc_animation <- function(curves) {
  steps <- 24; p <- plot_ly()
  for (c in curves) p <- add_trace(p, type = "scatter", mode = "lines", x = c$fpr, y = c$tpr, name = c$name, line = list(color = c$color, width = if (c$color == HOT) 3 else 2.4))
  p <- add_trace(p, type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), name = T("Azar"), line = list(dash = "dash", color = GREY))
  p <- layout(p, xaxis = list(title = T("Tasa de falsos positivos"), range = c(0, 1)), yaxis = list(title = T("Tasa de verdaderos positivos (Recall)"), range = c(0, 1.02)),
              updatemenus = list(list(type = "buttons", showactive = FALSE, x = 1, y = -.18, xanchor = "right", buttons = list(list(
                label = "▶", method = "animate", args = list(NULL, list(frame = list(duration = 45, redraw = FALSE), transition = list(duration = 0), fromcurrent = FALSE)))))))
  p <- plotly_build(p)
  p$x$frames <- lapply(seq_len(steps), function(k) list(name = as.character(k), traces = I(seq_along(curves) - 1),
    data = lapply(curves, function(c) { n <- max(2, as.integer(length(c$fpr) * k / steps)); list(type = "scatter", x = c$fpr[seq_len(n)], y = c$tpr[seq_len(n)]) })))
  p
}

conf_heat <- function(cm, size) {
  heatmap(cm, c(T("Predice permanencia"), T("Predice abandono")), c(T("Permanece"), T("Abandona")), SEQ, textsize = size, showscale = FALSE) %>%
    layout(yaxis = list(autorange = "reversed"), xaxis = list(tickangle = 0))
}

# ================================================================ 3. PREPROCESAMIENTO
STEPS <- list(c("target", N("Objetivo"), N("Churn Yes/No → 1/0. customerID solo para control.")), c("shield", N("TotalCharges"), N("Texto a número; los 8 vacíos de train (3 de test) tienen tenure 0 → 0.")),
              c("layers", N("Ingeniería"), N("“Sin internet / sin teléfono” → “No” y AvgChargePerMonth = TotalCharges / tenure.")), c("table", N("Codificar y escalar"), N("4 numéricas con StandardScaler y 16 categóricas con One-Hot: 24 columnas.")),
              c("scale", N("Balancear"), N("Ninguno, pesos, SMOTE o ADASYN; solo dentro del entrenamiento.")), c("network", N("Modelo"), N("Se ajusta en entrenamiento y se evalúa en el fold reservado.")))
COEFS <- data.frame(f = c("cat__Contract_Two year", "num__tenure", "cat__InternetService_No", "cat__InternetService_Fiber optic", "cat__Contract_One year", "num__TotalCharges",
                          "cat__PaymentMethod_Electronic check", "cat__OnlineSecurity_Yes", "cat__StreamingMovies_Yes", "cat__PaperlessBilling_Yes", "cat__StreamingTV_Yes",
                          "cat__PhoneService_Yes", "cat__TechSupport_Yes", "cat__MultipleLines_Yes", "num__MonthlyCharges", "cat__Dependents_Yes", "cat__SeniorCitizen_1",
                          "cat__OnlineBackup_Yes", "cat__PaymentMethod_Mailed check", "num__AvgChargePerMonth", "cat__gender_Male", "cat__Partner_Yes",
                          "cat__PaymentMethod_Credit card (automatic)", "cat__DeviceProtection_Yes"),
                    c = c(-1.422598, -1.153496, -1.079819, 1.036427, -0.725538, 0.486039, 0.398853, -0.371758, 0.348344, 0.334539, 0.323924, -0.31652, -0.304333, 0.28687,
                          -0.272618, -0.231701, 0.153587, -0.110944, 0.0482, -0.046767, 0.029063, 0.019222, -0.00774, 0.004777), stringsAsFactors = FALSE)
PREP_TABS <- c(N("1 · Pasos"), N("2 · Validación cruzada"), N("3 · Ablación"), N("4 · Evaluación en test"))

preprocesamiento <- function(d, tab = 0) tagList(page_head("preprocesamiento", T("Cómo se transforman los datos antes de modelar, sin espiar el test, y cuánto logra el modelo base.")),
                                                 tabs("prep-tabs", T(PREP_TABS), tab))

prep_tab <- function(i) {
  body <- switch(i + 1, {
    m <- c(T("Imputar con 0"), T("Mediana + indicador")); a_ <- c(.8459, .8460)
    fi <- plot_ly() %>% add_trace(type = "bar", x = m, y = a_, error_y = list(type = "data", array = c(.0124, .0125), color = GREY), marker = list(color = c(HOT, BASE))) %>%
      bar_labels(a_, m, .802, 4, horizontal = FALSE) %>% layout(xaxis = cat_axis(m), yaxis = list(range = c(.8, .87), title = "AUC-ROC CV"))
    tagList(card(tags$h3(class = "chart-title", T("El pipeline paso a paso")),
                 div(class = "pipeline", div(class = "flow-line"), lapply(seq_along(STEPS), function(k) div(class = "station", style = sprintf("animation-delay:%.2fs", (k - 1) * .15),
                     tags$span(class = "st-icon", icon(STEPS[[k]][1])), tags$h4(T(STEPS[[k]][2])), tags$p(T(STEPS[[k]][3])))))),
            grid(chart(T("¿Cero o mediana para TotalCharges?"), fi, T("Diferencia pareada por fold: +0.0001 ± 0.0001. Imputar con 0 es coherente con clientes sin facturación y no pierde desempeño."), 320, FALSE),
                 card(tags$h3(class = "chart-title", T("Ingeniería de características")),
                      kpis(kpi(T("CORRELACIÓN CON MONTHLYCHARGES"), .9963, "AvgChargePerMonth", "", 4), kpi(T("COLUMNAS TRAS CODIFICAR"), 24, T("4 numéricas + 20 indicadores")), cls = "k2"),
                      tags$p(class = "insight", T("El cargo promedio mensual comparte casi toda la información con el cargo mensual; su aporte se evalúa en la ablación.")))))
  }, {
    m <- T(c("Regresión logística", "Etiquetas permutadas", "Dummy estratificado", "Dummy mayoritaria")); a_ <- c(.8454, .4868, .5065, .5)
    fc <- plot_ly() %>% add_trace(type = "bar", x = a_, y = m, orientation = "h", error_x = list(type = "data", array = c(.0123, .0220, .0171, 0), color = GREY),
                                  marker = list(color = highlight(a_)), text = sprintf("%.4f", a_), textposition = "inside", insidetextanchor = "start") %>%
      layout(shapes = list(vline(.5, GREY, "dot")), xaxis = list(range = c(0, 1), title = "AUC-ROC CV (5 folds)"), yaxis = cat_axis(m))
    tagList(kpis(kpi("AUC-ROC CV", .8454, "± 0.0123", "", 4), kpi("PR-AUC CV", .6581, "± 0.0191", "", 4), kpi("RECALL CV", .8020, "± 0.0341", "", 4), kpi(T("CONTROL PERMUTADO"), .4868, T("AUC del azar"), "", 4)),
            chart(T("Validación cruzada en entrenamiento"), fc, T("El modelo supera con claridad a los controles: con etiquetas permutadas o un clasificador trivial el AUC ronda 0.5."), 340, FALSE))
  }, {
    abl <- df(c = T(c("Sin gender", "Sin PhoneService", "Sin PhoneService ni MultipleLines", "Sin gender ni PhoneService", "Sin AvgChargePerMonth")),
              d = c(.0001, .0002, -.0007, .0004, .0005), sd = c(.0005, .0003, .0018, .0004, .0003), dpr = c(-.0003, -.0003, -.0106, -.0000, .0019))
    fa <- plot_ly() %>% add_trace(type = "bar", x = abl$d, y = abl$c, orientation = "h", error_x = list(type = "data", array = abl$sd, color = GREY), marker = list(color = BASE)) %>%
      layout(shapes = list(vline(0)), xaxis = list(title = T("Δ AUC-ROC frente al modelo completo"), tickformat = ".4f"), yaxis = cat_axis(abl$c))
    names(abl) <- c("Configuración", "Δ AUC", "DE", "Δ PR-AUC")
    tagList(chart(T("Ablación por validación cruzada"), fa, T("Retirar gender, PhoneService o AvgChargePerMonth cambia el AUC menos que la variación entre folds; las variables se conservan."), 340, FALSE),
            card(table_ui(tt(abl))))
  }, {
    b <- baseline(); cm <- confusion(b$y_true, as.integer(b$probability >= .5))
    r <- roc_curve(b$y_true, b$probability)
    co <- COEFS[nrow(COEFS):1, ]; co$n <- pretty(co$f)
    fco <- plot_ly() %>% add_trace(type = "bar", x = co$c, y = co$n, orientation = "h", marker = list(color = ifelse(grepl("gender|PhoneService", co$f), HOT, BASE))) %>%
      layout(shapes = list(vline(0)), xaxis = list(title = T("Coeficiente (escala estandarizada)")), yaxis = cat_axis(co$n))
    tagList(kpis(kpi("ACCURACY", .7395, T("frente a 0.7346 de predecir siempre “permanece”"), "", 4), kpi("PRECISION", .5060, "", "", 4), kpi("RECALL", .7834, "", "hot", 4), kpi("F1", .6149, "", "", 4),
                 kpi("AUC-ROC", .8419, T("IC 95 % [0.8214, 0.8631]"), "", 4), kpi("PR-AUC", .6340, "", "", 4), cls = "k6"),
            grid(chart(T("Matriz de confusión"), conf_heat(cm, 18), T("Detecta 293 de 374 abandonos, pero 286 de sus 579 alertas son falsas: hay margen de mejora."), 360, FALSE),
                 chart(T("Curva ROC"), roc_animation(list(list(name = T("Regresión logística"), fpr = r$fpr, tpr = r$tpr, color = BASE))), T("Pulsa play para trazar la curva. AUC = 0.8419 en el test."), 360)),
            chart(T("Coeficientes de la regresión logística"), fco, T("Contrato a dos años, antigüedad y no tener internet reducen el riesgo; la fibra óptica lo aumenta. gender y PhoneService (resaltadas) pesan poco."), 640, FALSE))
  })
  tab_body(body)
}

# ================================================================ 4. MODELADO AVANZADO
METRICS <- c(f1_positive = N("F1 positivo"), recall_positive = "Recall", precision_positive = "Precision", roc_auc = "ROC-AUC", accuracy = "Accuracy")
MODEL_TABS <- c(N("1 · Resultados por modelo"), N("2 · Calibración"), N("3 · Optimizadores"), N("4 · Comparación estadística"), N("5 · Interpretabilidad"))

podium <- function() {
  p <- list(c("2", "XGBoost", "SMOTE · Random", .638, "second"), c("1", "Random Forest", "SMOTE · TPE", .638, "first"), c("3", T("Reg. logística"), "SMOTE · Grid", .634, "third"))
  card(eyebrow(T("PODIO · F1 MEDIO EXTERNO")),
       div(class = "podium", lapply(p, function(x) div(class = paste("pod", x[5]), tags$span(class = "medal", x[1]), tags$strong(x[2]), tags$small(x[3]), div(class = "bar-podium", counter(as.numeric(x[4]), 3))))),
       tags$p(class = "insight", T("Empate técnico: la diferencia entre RF y XGBoost es 0.0004, menor que la variación entre folds.")))
}

modelado <- function(d, tab = 0) {
  tagList(page_head("modelado", T("Siete familias × cuatro balanceos × cuatro optimizadores = 112 configuraciones evaluadas con validación cruzada anidada.")),
          div(class = "hero", podium(), kpis(kpi(T("CONFIGURACIONES"), 112, "7 × 4 × 4"), kpi(T("UNIDADES EXTERNAS"), 1680, T("112 × 3 semillas × 5 folds")),
                                              kpi("FOLDS", "5 × 3", T("externos × internos")), kpi(T("PRESUPUESTO"), 24, T("evaluaciones por búsqueda")), cls = "k2")),
          tabs("model-tabs", T(MODEL_TABS), tab))
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
            uiOutput("models-output")),
    calibration_summary(),
    tagList(card(cls = "controls-card", field(T("Familia"), dropdown("opt-family", names(MODELS), keep("opt-family", "random_forest"), fam))), uiOutput("opt-output")),
    estadistica(),
    tagList(card(cls = "controls-card", field(T("Modelo explicado"), dropdown("explain-model", FINALISTS[1:2], keep("explain-model", FINALISTS[1]), setNames(as.list(T(unname(FINALIST_NAMES[FINALISTS[1:2]]))), FINALISTS[1:2]))),
                 uiOutput("explain-output")),
            note(T("Explicar no es causar"), T("Las atribuciones describen cómo decide el modelo, no qué provoca el abandono. Los casos locales se eligieron por máxima confianza, no por ser representativos."))))
  tab_body(body)
}

ranking_families <- function() {
  s <- SUMMARY[match(BEST_BY_FAMILY, SUMMARY$experiment_id), ]; s <- s[order(s$f1_positive_mean), ]; s$n <- config_name(s$experiment_id)
  f <- plot_ly() %>% add_trace(type = "bar", x = s$f1_positive_mean, y = s$n, orientation = "h", error_x = list(type = "data", array = s$f1_positive_std, color = GREY),
                               marker = list(color = highlight(s$f1_positive_mean, nrow(s)))) %>%
    bar_labels(s$f1_positive_mean, s$n, .551) %>% layout(xaxis = list(range = c(.55, .68), title = T("F1 positivo · media de 5 folds externos")), yaxis = cat_axis(s$n))
  chart(T("Mejor configuración de cada familia"), f, T("Barras: media de los folds externos; bigotes: desviación estándar. Random Forest y XGBoost empatan arriba (0.638)."), 380, FALSE)
}

calibration_summary <- function() {
  u <- UNITS[UNITS$replicate == 0, ]
  g <- stats::aggregate(u[c("brier", "cal_brier", "ece", "cal_ece")], list(model = u$model), mean, na.action = na.pass, na.rm = TRUE)
  g[is.na(g)] <- NA; for (c in c("brier", "ece")) g[[c]][is.nan(g[[c]])] <- NA
  g$n <- model_name(g$model)
  bars <- function(a_, b_, title) plot_ly() %>% add_trace(type = "bar", x = g$n, y = g[[a_]], name = T("Nativa"), marker = list(color = BASE)) %>%
    add_trace(type = "bar", x = g$n, y = g[[b_]], name = T("Sigmoide"), marker = list(color = HOT)) %>% layout(barmode = "group", xaxis = cat_axis(g$n), yaxis = list(title = title))
  tagList(kpis(kpi(T("CONFIGURACIONES MEJORADAS"), 80, T("de 96 con probabilidad nativa")), kpi(T("MÉTODO"), "Platt", T("sigmoide dentro del entrenamiento")),
               kpi(T("INTERVALOS ECE"), 10, T("uniformes")), kpi("SVM", T("solo sigmoide"), T("sin probabilidad nativa"))),
          grid(chart(T("Puntaje de Brier por familia"), bars("brier", "cal_brier", "Brier"), T("Menor es mejor. Promedio sobre balanceos, optimizadores y folds externos."), 360),
               chart(T("ECE por familia"), bars("ece", "cal_ece", "ECE"), T("Distancia entre la probabilidad predicha y la frecuencia observada; la rama sigmoide la reduce en casi todas las familias."), 360)),
          note(T("Síntesis de la calibración"), T("La calibración sigmoide redujo a la vez Brier y ECE en 80 de 96 configuraciones con probabilidad nativa. No mejora todas, y no se usó para elegir el ranking principal.")))
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
  for (cc in list(c("probability_native", T("Nativa"), BASE), c("probability_sigmoid", T("Sigmoide"), HOT))) {
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
