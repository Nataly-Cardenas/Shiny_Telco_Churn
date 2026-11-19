# Portada, 1. Introducción y 2. Análisis exploratorio.

NAV <- list(c("introduccion", N("Introducción"), "signal"), c("eda", N("Análisis exploratorio"), "chart"), c("preprocesamiento", N("Preprocesamiento"), "flask"),
            c("modelado", N("Modelado avanzado"), "trophy"), c("modelos_ml", N("Modelos finales"), "network"), c("conclusiones", N("Conclusiones"), "flag"),
            c("autoras", N("Autoras"), "users"))
TITLES <- c(list(inicio = N("Inicio")), setNames(lapply(NAV, `[`, 2), vapply(NAV, `[`, "", 1)))
CHAPTER <- list(introduccion = "intro", eda = "EDA_", preprocesamiento = "PREPROCESAMIENTO", modelado = "MODELADO_AVANZADO")
DATA_PAGES <- "eda"

# ---------------------------------------------------------------- portada
# Señales del perfil asociadas a más (▲) o menos (▼) abandono en el EDA.
flags <- function(c) {
  out <- c(if (c$Contract == "Month-to-month") paste("▲", T("Contrato mes a mes")), if (c$Contract == "Two year") paste("▼", T("Contrato a dos años")),
           if (c$InternetService == "Fiber optic") paste("▲", T("Fibra óptica")), if (c$PaymentMethod == "Electronic check") paste("▲", T("Paga con cheque electrónico")),
           if (c$tenure <= 12) paste("▲", T("Menos de un año como cliente")), if (c$tenure > 48) paste("▼", T("Más de 4 años como cliente")),
           if (c$TechSupport == "No") paste("▲", T("Sin soporte técnico")), if (c$OnlineSecurity == "Yes") paste("▼", T("Con seguridad en línea")))
  utils::head(out, 4)
}

# 30 clientes reales del hold-out, mezclando riesgo alto, medio y bajo según RegionalMAPEM (los mismos del dash, misma semilla).
COVER_ROWS <- as.data.frame(jsonlite::fromJSON(file.path(DATA, "portada_clientes.json")))

cover_customers <- function() {
  lapply(seq_len(nrow(COVER_ROWS)), function(i) {
    c <- rows(COVER_ROWS$row_id[i])
    list(id = c$customerID, contract = cat_name(c$Contract), internet = cat_name(c$InternetService), payment = cat_name(c$PaymentMethod),
         tenure = c$tenure, monthly = c$MonthlyCharges, prob = round(COVER_ROWS$prob[i], 3), churn = c$target, flags = I(as.character(flags(c))))
  })
}

phone <- function() {
  text <- list(months = T("meses"), risk = list(alto = T("Riesgo alto"), medio = T("Riesgo medio"), bajo = T("Riesgo bajo")),
               churned = T("En la realidad: abandonó"), stayed = T("En la realidad: permaneció"),
               toast = list(alto = T("Alerta de retención enviada"), medio = T("Cliente en observación"), bajo = T("Cliente estable")))
  div(class = "phone-stage", div(
    id = "phone", class = "phone", `data-customers` = jsonlite::toJSON(cover_customers(), auto_unbox = TRUE), `data-text` = jsonlite::toJSON(text, auto_unbox = TRUE),
    div(class = "phone-status", tags$span("9:41"), tags$span(class = "sig", tags$b(), tags$b(), tags$b(), tags$b()), tags$span(class = "bat", "5G")),
    div(class = "phone-body",
        div(class = "between", tags$span(class = "pc-app", T("TELCO · RADAR DE CLIENTES")), tags$span(class = "pc-live", T("EN VIVO"))),
        div(class = "pc-row", tags$small(T("Cliente")), tags$strong(class = "pc-id")),
        div(class = "pc-pills", tags$span(class = "pc-contract pill"), tags$span(class = "pc-internet pill"), tags$span(class = "pc-payment pill")),
        div(class = "pc-grid", div(tags$small(T("Antigüedad")), tags$strong(class = "pc-tenure")), div(tags$small(T("Cargo mensual")), tags$strong(class = "pc-monthly"))),
        div(class = "pc-signals", tags$small(T("Señales del perfil")), tags$ul(class = "pc-flags")),
        div(class = "pc-risk",
            div(class = "ring", div(class = "ring-label", tags$strong(class = "ring-pct"), tags$small(T("prob. de abandono")))),
            div(class = "pc-verdict", tags$strong(class = "pc-level"), tags$small(T("RegionalMAPEM · hold-out")), tags$small(class = "pc-truth"))),
        div(class = "toast", tags$span(class = "toast-dot"), tags$span(class = "toast-text")))))
}

cover <- function() {
  div(class = "cover",
      tags$canvas(id = "net-canvas", class = "net-canvas"),
      div(class = "cover-glow"),
      div(class = "cover-inner",
          div(class = "cover-text",
              eyebrow("TELCO CUSTOMER CHURN · UNIVERSIDAD DEL NORTE"),
              tags$h1(class = "cover-title", T("Señal"), tags$br(), tags$span(class = "glitch", `data-text` = T("perdida"), T("perdida"))),
              tags$p(class = "cover-lead", T("Uno de cada cuatro clientes "), tags$b(T("cuelga para siempre"), .noWS = "outside"), T(". Descubre quiénes son, por qué se van y cómo anticiparlo con datos.")),
              div(class = "cover-stats",
                  div(counter(7043), tags$small(T("clientes analizados"))),
                  div(class = "hot", counter(26.5, 1, " %"), tags$small(T("abandonan el servicio"))),
                  div(counter(12), tags$small(T("modelos finales comparados"))),
                  div(counter(0.41127, 5), tags$small(T("log-loss de RegionalMAPEM")))),
              tags$p(class = "cover-verdict muted", tags$b(T("Veredicto: ")), T("el modelo propio empata con la mejor referencia (logística con splines, 0.41130); no la supera de forma significativa.")),
              div(class = "cover-cta",
                  tags$a(class = "btn primary", href = "#introduccion", T("Entrar al dashboard"), " →"),
                  tags$a(class = "btn ghost", href = BOOK, target = "_blank", T("Jupyter Book"), icon("arrow", "sm"))),
              div(class = "cover-hint", icon("cursor"),
                  tags$span(T("Los puntos rojos son clientes a punto de irse: "), tags$b(T("pasa el cursor (o el dedo) sobre ellos para retenerlos."))),
                  tags$span(class = "score", T("Perdidos: "), tags$b(id = "cover-lost", "0"), " · ", T("Retenidos por ti: "), tags$b(id = "cover-saved", "0")))),
          phone()),
      div(class = "cover-foot", "Laura S. Rivera Figueredo · Natalý A. Cárdenas Izaquita"))
}

# ================================================================ 1. INTRODUCCIÓN
FLIP <- list(c("target", N("Abandono y clasificación binaria"), N("El churn es la cancelación del servicio. Se estima para cada cliente la probabilidad P(abandono | x) y se separa a quienes se van (1) de quienes permanecen (0).")),
             c("chart", N("Análisis exploratorio"), N("Antes de modelar se mide la asociación de cada variable con el abandono: correlación punto-biserial, V de Cramér y Spearman, con corrección de Holm.")),
             c("scale", N("Desbalance de clases"), N("Solo el 26.5 % abandona. SMOTE y ADASYN crean ejemplos sintéticos de la minoría para que el modelo no la ignore.")),
             c("block", N("Fuga de información"), N("Imputar, codificar, escalar y balancear solo con datos de entrenamiento; si no, las métricas resultan optimistas.")),
             c("tree", N("Modelos de clasificación"), N("Regresión logística como modelo base, frente a KNN, Naive Bayes, árbol de decisión, Random Forest, XGBoost y SVM.")),
             c("repeat", N("Validación cruzada anidada"), N("Un bucle interno elige hiperparámetros con Grid, Random, TPE o un algoritmo genético; uno externo mide el desempeño con datos que no intervinieron.")),
             c("sigma", N("Métricas"), N("F1 = 2TP / (2TP + FP + FN) equilibra detectar abandonos y evitar falsas alertas; AUC-ROC mide qué tan bien se ordena el riesgo. Friedman, Nemenyi y DeLong comparan modelos.")),
             c("bulb", N("Interpretabilidad"), N("SHAP reparte cada predicción entre las variables y LIME explica casos individuales. Describen al modelo, no causas del abandono.")))
OBJECTIVES <- list(c(N("Describir el problema y los datos"), N("Caracterizar el conjunto, su calidad y el desbalance de la variable objetivo.")),
                   c(N("Reservar un conjunto de prueba"), N("Separar un hold-out estratificado de 1,409 clientes (semilla 42) antes de cualquier ajuste.")),
                   c(N("Identificar los perfiles de abandono"), N("Analizar cada variable y su asociación con el abandono para encontrar los segmentos con más cancelaciones.")),
                   c(N("Preparar los datos sin fuga de información"), N("Ajustar imputación, codificación y escalado dentro de cada pliegue, solo con datos de entrenamiento.")),
                   c(N("Comparar once modelos de referencia"), N("Optimizarlos con el mismo presupuesto (TPE, 30 evaluaciones), calibrarlos con Platt y compararlos con validación anidada 5 × 5 y log-loss.")),
                   c(N("Proponer un modelo propio"), N("Programar RegionalMAPEM, que pondera expertos calibrados según la región del cliente con pesos estimados por MAP-EM.")),
                   c(N("Verificar con pruebas estadísticas"), N("Contrastar al modelo propio con los benchmarks (223 comparaciones con Holm) y con tres conjuntos de churn adicionales.")),
                   c(N("Poner los resultados a disposición"), N("Presentar el análisis en un Jupyter Book, un paper, una ficha del modelo y este tablero interactivo.")))
PIPELINE <- list(c("table", N("1 · ETL"), N("Limpieza por filas y partición 80/20 estratificada; el preprocesamiento va dentro del pipeline.")),
                 c("chart", N("2 · EDA"), N("Distribuciones y asociaciones con el abandono, con corrección de Holm, solo sobre desarrollo.")),
                 c("tree", N("3 · Modelos de referencia"), N("Once modelos calibrados con Platt, incluidos los métodos recientes.")),
                 c("hourglass", N("4 · Optimización"), N("TPE con 30 evaluaciones por modelo y pliegue, el mismo presupuesto para todos.")),
                 c("repeat", N("5 · Validación anidada"), N("5 repeticiones × 5 pliegues externos; el log-loss es la métrica principal.")),
                 c("scale", N("6 · Comparación estadística"), N("Friedman, Nemenyi, pruebas por pares y bayesiana; 223 comparaciones con Holm.")),
                 c("network", N("7 · Modelo original"), N("RegionalMAPEM: pesos por región estimados con MAP-EM.")),
                 c("layers", N("8 · Evaluación ampliada"), N("Tres conjuntos adicionales, robustez, calibración, escalabilidad y veredicto.")))
GROUPS <- list(list(N("Identificador"), "customerID"), list(N("Demografía"), c("gender", "SeniorCitizen", "Partner", "Dependents")),
               list(N("Servicios"), c("PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")),
               list(N("Cuenta"), c("tenure", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges")), list(N("Objetivo"), "Churn"))

people_row <- function() {
  leaving <- c(2, 7, 11, 16, 19)   # 5 de 20 ≈ 26.5 % de abandono
  div(class = "people", lapply(0:19, function(i) tags$span(class = paste0("person", if (i %in% leaving) " leaving" else ""), style = sprintf("animation-delay:%.1fs", (i %% 5) * .9))))
}

introduccion <- function(d) {
  variables <- do.call(rbind, lapply(GROUPS, function(g) do.call(rbind, lapply(g[[2]], function(v) df(
    Grupo = T(g[[1]]), Variable = v,
    Significado = if (v %in% names(LABELS)) label(v) else if (v == "customerID") T("Código del cliente") else T("Abandono durante el último mes"),
    Tipo = if (v %in% NUMERIC) T("Numérica") else if (v == "customerID") T("Identificador") else T("Categórica"),
    Papel = if (v == "Churn") T("Objetivo") else if (v == "customerID") T("No predictor") else T("Candidato"))))))
  tagList(
    page_head("introduccion", T("Retener a un cliente cuesta menos que conseguir uno nuevo. Este proyecto busca anticipar quién está por cancelar su servicio para actuar a tiempo.")),
    div(class = "hero",
        div(class = "hero-text", eyebrow(T("CONTEXTO DEL PROBLEMA")), tags$h2(T("1 de cada 4 clientes "), tags$span(class = "txt-hot", T("se desconecta"))),
            tags$p(T(paste0("El conjunto Telco Customer Churn de IBM reúne servicios contratados, datos de cuenta y perfil demográfico de cada cliente, ",
                            "junto con si canceló en el último mes. Dos dificultades: los que se van son minoría, y los datos son una foto de un momento, ",
                            "así que los hallazgos son asociaciones y no causas."))), people_row()),
        kpis(kpi(T("CLIENTES"), 7043, T("una fila por cliente")), kpi(T("ABANDONOS"), 1869, T("cancelaron en el último mes")),
             kpi(T("TASA DE ABANDONO"), 26.5, T("la clase minoritaria"), "hot", 1, " %"), kpi(T("VARIABLES"), 19, T("predictoras candidatas")), cls = "k2")),
    subhead(T("Problema y relevancia")),
    card(tags$p(class = "big-text", T("La fuga de clientes es una de las principales fuentes de pérdida de ingresos en telecomunicaciones: retener a un cliente cuesta menos que adquirir uno nuevo y los contratos de servicio generan ingresos recurrentes "),
                tags$a("(Verbeke et al., 2012; ", href = "https://doi.org/10.1016/j.ejor.2011.09.031", target = "_blank", .noWS = "outside"), tags$a("Neslin et al., 2006)", href = "https://doi.org/10.1509/jmkr.43.2.204", target = "_blank", .noWS = "outside"),
                T(". Un buen modelo permite dirigir las campañas de retención a quienes tienen mayor riesgo, siempre que sus probabilidades estén bien calibradas. En estos datos, el 26.5 % de los clientes abandonó en el último mes.")),
         tags$p(class = "muted", T(paste0("Verbeke, W., Dejaeger, K., Martens, D., Hur, J. y Baesens, B. (2012). New insights into churn prediction in the telecommunication sector: a profit driven data mining approach. European Journal of Operational Research, 218(1), 211–229. ",
                                          "Neslin, S. A., Gupta, S., Kamakura, W., Lu, J. y Mason, C. H. (2006). Defection detection: measuring and understanding the predictive accuracy of customer churn models. Journal of Marketing Research, 43(2), 204–211.")))),
    subhead(T("Pregunta de investigación")),
    card(cls = "glow", tags$p(class = "big-text", T("¿Combinar los once modelos de referencia calibrados con pesos que dependen de la región del cliente reduce, de forma estadísticamente significativa, el log-loss frente al mejor modelo de referencia individual en la predicción de abandono?"))),
    subhead(T("Objetivo general")),
    card(cls = "glow", tags$p(class = "big-text", T(paste0("Identificar a los clientes de una empresa de telecomunicaciones con mayor riesgo de abandonar el servicio y los factores asociados a ese abandono, ",
                                                       "y evaluar con un protocolo reproducible si un modelo propio de combinación por regiones (RegionalMAPEM) mejora, con respaldo estadístico, a los modelos de referencia.")))),
    subhead(T("Objetivos específicos")),
    div(class = "objectives", lapply(seq_along(OBJECTIVES), function(i) div(class = "obj", tags$span(class = "obj-n", sprintf("%02d", i)), tags$h4(T(OBJECTIVES[[i]][1])), tags$p(T(OBJECTIVES[[i]][2]))))),
    subhead(T("Datos"), T("Ficha del conjunto principal.")),
    card(div(class = "facts", lapply(list(c(T("Fuente"), T("IBM, publicado como conjunto de muestra en Kaggle (Telco Customer Churn)")), c(T("Tamaño"), T("7,043 clientes · 21 columnas (19 predictoras)")),
                                          c(T("Variable objetivo"), T("Churn: se dio de baja en el último mes (26.5 %)")), c(T("Tipo de tarea"), T("Clasificación binaria con clases desbalanceadas")),
                                          c(T("Periodo"), T("Sin periodo de recolección declarado: una fotografía de corte transversal de un operador ficticio")),
                                          c(T("Licencia"), T("La de la publicación en Kaggle; datos de muestra de IBM para uso educativo y de demostración"))),
                                     function(f) div(tags$small(f[1]), tags$strong(f[2]))))),
    subhead(T("Pipeline del proyecto"), T("De los datos crudos a la evaluación del modelo original.")),
    div(class = "repro", lapply(PIPELINE, function(r) div(class = "station", tags$span(class = "st-icon", icon(r[1])), tags$h4(T(r[2])), tags$p(T(r[3]))))),
    subhead(T("Marco teórico"), T("Pasa el cursor (o toca) cada tarjeta para voltearla.")),
    div(class = "flips", lapply(FLIP, function(f) div(class = "flip", tabindex = "0", div(class = "flip-inner",
        div(class = "flip-front", tags$span(class = "flip-icon", icon(f[1])), tags$h4(T(f[2]))),
        div(class = "flip-back", tags$h4(T(f[2])), tags$p(T(f[3]))))))),
    subhead(T("Descripción de las variables del proyecto"), T("Diccionario resumido de los metadatos de Kaggle.")),
    card(table_ui(variables, page_size = 21)),
    subhead(T("Limitaciones, ética y autoras")),
    grid(card(eyebrow(T("LIMITACIONES")), tags$ul(lapply(c("Un solo conjunto principal: las conclusiones confirmatorias son sobre Telco; tres conjuntos adicionales dan consistencia.",
                                                          "Los datos son sintéticos, de un solo operador y sin dimensión temporal: no se trasladan a otros mercados sin validarlos.",
                                                          "Los hallazgos son asociaciones, no causas.", "Sin costos de intervención, el log-loss no se traduce en dinero."), function(x) tags$li(T(x))))),
         card(eyebrow(T("ÉTICA")), tags$ul(lapply(c("Los datos no contienen información personal real.", "El modelo es una herramienta de apoyo: no reemplaza la revisión humana antes de actuar sobre un cliente.",
                                                    "Usos no recomendados y consideraciones éticas en la ficha del modelo (Modelos finales)."), function(x) tags$li(T(x)))),
              tags$p(class = "muted", T("Autoras: Laura S. Rivera Figueredo y Natalý A. Cárdenas Izaquita · Universidad del Norte.")),
              tags$p(class = "muted", T("Versión 1.0 · protocolo-v1 · actualizado el 17 de noviembre de 2026.")))))
}

# ================================================================ 2. ANÁLISIS EXPLORATORIO
# Nombre del eje con sus unidades.
unit <- function(var) switch(var, tenure = T("Antigüedad (meses)"), MonthlyCharges = T("Cargo mensual ($)"), TotalCharges = T("Cargo acumulado ($)"),
                             AvgChargePerMonth = T("Cargo promedio mensual ($)"), label(var))
EDA_TABS <- c(N("1 · ETL y partición"), N("2 · Univariado"), N("3 · Bivariado"), N("4 · Multivariado"), N("5 · Valores faltantes"), N("6 · Resumen"), N("7 · Explorador de datos"))

eda_page <- function(d, tab = 0) {
  tagList(page_head("eda", T("Del archivo original a los patrones de abandono. Todo el análisis se hace sobre el conjunto de entrenamiento; los filtros superiores recalculan las gráficas y tablas.")),
          uiOutput("eda-summary"),
          tabs("eda-tabs", T(EDA_TABS), tab))
}

eda_kpis <- function(d) {
  if (!nrow(d)) return(empty())
  tagList(scope(d),
          kpis(kpi(T("CLIENTES"), nrow(d), T("en la selección")), kpi(T("ABANDONO"), round(mean(d$target) * 100, 2), fmt(T("{n} clientes"), n = f0(sum(d$target))), "hot", 2, " %"),
               kpi(T("ANTIGÜEDAD MEDIA"), round(mean(d$tenure), 1), T("meses"), "", 1), kpi(T("CARGO MENSUAL MEDIO"), round(mean(d$MonthlyCharges), 2), T("dólares"), "", 2, "", "$")))
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
    multi_output(d),
    missing_output(d),
    eda_summary(),
    tagList(
    card(cls = "controls-card", div(class = "controls",
         field(T("Eje x"), dropdown("free-x", EJES, keep("free-x", "tenure"), setNames(lapply(EJES, unit), EJES))),
         field(T("Eje y"), dropdown("free-y", EJES, keep("free-y", "MonthlyCharges"), setNames(lapply(EJES, unit), EJES))),
         field(T("Color"), dropdown("free-color", c("Churn", CATEGORICAL), keep("free-color", "Churn"), c(list(Churn = T("Abandono")), setNames(lapply(CATEGORICAL, unit), CATEGORICAL))))),
         uiOutput("free-output")),
    card(cls = "controls-card", field(T("Columnas visibles"), dropdown("explorer-cols", RAW_COLS, keep("explorer-cols", c("customerID", "Contract", "InternetService", "tenure", "MonthlyCharges", "Churn")),
                                                                  setNames(as.list(RAW_COLS), RAW_COLS), multi = TRUE)),
         uiOutput("explorer-table"),
         div(class = "export-row", downloadLink("export", class = "btn primary", icon("download"), T("Descargar CSV")),
             tags$p(class = "muted", T("Descarga un solo archivo con la selección actual de los filtros superiores y las columnas elegidas."))))))
  tab_body(body)
}

# Explorador libre: dispersión entre dos numéricas, cajas entre numérica y categórica o conteos entre dos categóricas.
EJES <- c(NUMERIC, CATEGORICAL)
PX_COLORS <- c("#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A", "#19D3F3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52")   # paleta por defecto de plotly express
free_output <- function(x, y, color, d) {
  if (!nrow(d)) return(empty())
  val <- function(c) if (c == "Churn") churn_label(d$Churn) else if (c %in% NUMERIC) d[[c]] else as.character(cat_name(d[[c]]))
  g <- val(color)
  niveles <- if (color == "Churn") T(c("Permanece", "Abandona")) else unique(g)
  paleta <- if (color == "Churn") outcome_colors() else setNames(rep(PX_COLORS, 3)[seq_along(niveles)], niveles)
  nombre_c <- if (color == "Churn") T("Abandono") else unit(color)
  f <- plot_ly()
  if (x %in% NUMERIC && y %in% NUMERIC) {
    set.seed(0); i <- sample(nrow(d), min(2000, nrow(d)))
    for (k in niveles) { m <- i[g[i] == k & !is.na(d[[x]][i]) & !is.na(d[[y]][i])]
      f <- add_trace(f, type = "scatter", mode = "markers", x = d[[x]][m], y = d[[y]][m], name = k, legendgroup = k, marker = list(color = paleta[[k]], opacity = .6)) }
    f <- layout(f, xaxis = list(title = unit(x)), yaxis = list(title = unit(y)), legend = list(title = list(text = nombre_c)))
    rho <- suppressWarnings(stats::cor(d[[x]], d[[y]], method = "spearman", use = "pairwise.complete.obs"))
    note_ <- fmt(T("Muestra de hasta 2,000 clientes de la selección. Spearman entre {x} y {y}: ρ = {r}."), x = unit(x), y = unit(y), r = sprintf("%.2f", rho))
  } else if (x %in% NUMERIC || y %in% NUMERIC) {
    n <- if (x %in% NUMERIC) x else y; k_ <- if (x %in% NUMERIC) y else x
    kv <- val(k_)
    for (k in niveles) { m <- g == k & !is.na(d[[n]])
      f <- if (x %in% NUMERIC) add_trace(f, type = "box", x = d[[n]][m], y = kv[m], orientation = "h", name = k, marker = list(color = paleta[[k]]), line = list(color = paleta[[k]]))
           else add_trace(f, type = "box", x = kv[m], y = d[[n]][m], name = k, marker = list(color = paleta[[k]]), line = list(color = paleta[[k]])) }
    cats <- unique(kv)
    f <- if (x %in% NUMERIC) layout(f, xaxis = list(title = unit(n)), yaxis = cat_axis(cats, title = unit(k_)), legend = list(title = list(text = nombre_c)))
         else layout(f, xaxis = cat_axis(cats, title = unit(k_)), yaxis = list(title = unit(n)), legend = list(title = list(text = nombre_c)))
    # boxmode existe en plotly.js aunque el esquema de plotly para R no lo declare: se fija al dibujar.
    f <- htmlwidgets::onRender(f, "function(el) { Plotly.relayout(el, {boxmode: 'group'}); }")
    note_ <- fmt(T("Cajas de {y} por categoría de {x}, separadas por {c}."), y = unit(n), x = unit(k_), c = nombre_c)
  } else {
    xv <- val(x); yv <- val(y); xs <- unique(xv); ys <- unique(yv)
    z <- t(table(factor(xv, xs), factor(yv, ys))); z <- matrix(as.numeric(z), nrow = length(ys))
    f <- add_trace(f, type = "heatmap", x = xs, y = ys, z = z, colorscale = list(list(0, "#16213a"), list(1, HOT)), texttemplate = "%{z}",
                   colorbar = list(title = list(text = "count")), hovertemplate = paste0(unit(x), "=%{x}<br>", unit(y), "=%{y}<br>count=%{z}<extra></extra>")) %>%
      layout(xaxis = cat_axis(xs, title = unit(x)), yaxis = cat_axis(ys, title = unit(y)))
    note_ <- T("Número de clientes por combinación de categorías; el color de puntos no aplica entre dos categóricas.")
  }
  div(graph(f, 460, TRUE), tags$p(class = "insight", note_))
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
    layout(xaxis = list(title = unit(var)), yaxis = list(title = T("Clientes")))
  b <- plot_ly() %>% add_trace(type = "box", y = s, name = unit(var), marker = list(color = BASE), line = list(color = BASE), fillcolor = "rgba(46,134,171,.5)",
                               boxpoints = "outliers", quartilemethod = "linear")
  f <- subplots(list(h, b), c(T("Distribución"), T("Rango y cuartiles")), widths = c(.7, .3))
  f$x$layout$shapes <- list(vline(mean(s), HOT, "dash"))
  f$x$layout$yaxis2$title <- list(text = unit(var))
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
  f <- layout(f, xaxis = cat_axis(order, title = ""), yaxis = list(title = unit(var)))
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
