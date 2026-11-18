# Sección 1 · Contexto: portada, problema, pregunta, datos, metodología, limitaciones y autoras (pages/contexto.py del dash).

FUENTE_CIFRA <- "https://hbr.org/2014/10/the-value-of-keeping-the-right-customers"
FLIP <- list(c("target", N("Abandono y clasificación binaria"), N("El churn es la cancelación del servicio. Se estima para cada cliente la probabilidad P(abandono | x) y se separa a quienes se van (1) de quienes permanecen (0).")),
             c("chart", N("Análisis exploratorio"), N("Antes de modelar se mide la asociación de cada variable con el abandono: correlación punto-biserial, V de Cramér y Spearman, con corrección de Holm.")),
             c("block", N("Fuga de información"), N("Imputar, codificar, escalar y calibrar solo con datos de entrenamiento; si no, las métricas resultan optimistas.")),
             c("repeat", N("Validación cruzada anidada"), N("Un bucle interno elige hiperparámetros con TPE; uno externo mide el desempeño con datos que no intervinieron. Aquí, 5 repeticiones × 5 pliegues.")),
             c("sigma", N("Log-loss y calibración"), N("El log-loss castiga las probabilidades seguras y equivocadas. Platt ajusta una sigmoide para que un 30 % predicho se parezca a un 30 % observado.")),
             c("layers", N("Ensambles por región"), N("RegionalMAPEM pondera a 11 clasificadores con pesos distintos en cada región de clientes, estimados con MAP-EM y un prior de Dirichlet.")),
             c("scale", N("Comparación estadística"), N("t corregida de Nadeau-Bengio, permutaciones, Wilcoxon y Friedman-Nemenyi, con corrección de Holm y tamaño del efecto de Cliff.")),
             c("bulb", N("Interpretabilidad"), N("SHAP reparte cada predicción entre las variables. Describe cómo decide el modelo, no las causas del abandono.")))
OBJECTIVES <- list(c(N("Describir el problema y los datos"), N("Caracterizar el conjunto, su calidad y el desbalance de la variable objetivo.")),
                   c(N("Reservar un hold-out"), N("Separar el 20 % de los clientes con una partición estratificada antes de cualquier análisis.")),
                   c(N("Identificar los perfiles de abandono"), N("Analizar cada variable y su asociación con el abandono para encontrar los segmentos con más cancelaciones.")),
                   c(N("Comparar 11 benchmarks"), N("Optimizar cada modelo con el mismo presupuesto (30 evaluaciones de TPE) y calibrarlo con Platt dentro del bucle interno.")),
                   c(N("Proponer un modelo original"), N("Formular RegionalMAPEM, demostrar su convergencia e implementarlo desde cero con pruebas unitarias.")),
                   c(N("Validar con un protocolo pre-registrado"), N("Congelar hipótesis, métricas y pruebas antes de correr los experimentos definitivos.")),
                   c(N("Evaluar más allá de Telco"), N("Repetir la comparación en tres conjuntos de churn adicionales y estudiar robustez, calibración e incertidumbre.")),
                   c(N("Poner los modelos a disposición"), N("Publicar el Jupyter Book y este tablero con los artefactos finales para predecir en vivo.")))
GROUPS <- list(list(N("Identificador"), "customerID"), list(N("Demografía"), c("gender", "SeniorCitizen", "Partner", "Dependents")),
               list(N("Servicios"), c("PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")),
               list(N("Cuenta"), c("tenure", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges")), list(N("Objetivo"), "Churn"))
PIPELINE <- list(c("table", N("ETL"), N("TotalCharges a número, categorías redundantes unificadas y cargo promedio mensual.")),
                 c("chart", N("EDA"), N("Asociaciones con el abandono sobre los 5634 clientes de desarrollo.")),
                 c("tree", N("11 benchmarks"), N("De KNN y Naive Bayes a LightGBM y CatBoost, todos calibrados con Platt.")),
                 c("sliders", N("Optimización"), N("TPE con 30 evaluaciones por modelo y pliegue externo, minimizando el log-loss.")),
                 c("repeat", N("Validación anidada"), N("5 repeticiones × 5 pliegues externos; 3 pliegues internos.")),
                 c("scale", N("Comparación estadística"), N("223 contrastes con Holm, Friedman-Nemenyi y Cliff.")),
                 c("layers", N("Modelo original"), N("RegionalMAPEM: pesos por región estimados con MAP-EM.")),
                 c("flask", N("Evaluación ampliada"), N("3 conjuntos extra, robustez, conforme y hold-out abierto una vez.")))

people_row <- function() {
  leaving <- c(2, 7, 11, 16, 19)   # 5 de 20 ≈ 26.5 % de abandono
  div(class = "people", lapply(0:19, function(i) tags$span(class = paste0("person", if (i %in% leaving) " leaving" else ""), style = sprintf("animation-delay:%.1fs", (i %% 5) * .9))))
}

# Señales del perfil asociadas a más (▲) o menos (▼) abandono en el EDA.
flags <- function(c) {
  out <- c(if (c$Contract == "Month-to-month") paste("▲", T("Contrato mes a mes")), if (c$Contract == "Two year") paste("▼", T("Contrato a dos años")),
           if (c$InternetService == "Fiber optic") paste("▲", T("Fibra óptica")), if (c$PaymentMethod == "Electronic check") paste("▲", T("Paga con cheque electrónico")),
           if (c$tenure <= 12) paste("▲", T("Menos de un año como cliente")), if (c$tenure > 48) paste("▼", T("Más de 4 años como cliente")),
           if (c$TechSupport == "No") paste("▲", T("Sin soporte técnico")), if (c$OnlineSecurity == "Yes") paste("▼", T("Con seguridad en línea")))
  utils::head(out, 4)
}

# 30 clientes reales del hold-out, mezclando riesgo alto, medio y bajo según RegionalMAPEM (los mismos del dash).
COVER_ROWS <- jsonlite::fromJSON(file.path(DATA, "portada_clientes.json"))

cover_customers <- function() {
  lapply(seq_along(COVER_ROWS$row_id), function(i) {
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
  r <- telco("resumen_modelos.csv"); ll <- r$log_loss_media[r$modelo == ORIGINAL]
  div(class = "cover",
      tags$canvas(id = "net-canvas", class = "net-canvas"),
      div(class = "cover-glow"),
      div(class = "cover-inner",
          div(class = "cover-text",
              eyebrow("TELCO CUSTOMER CHURN · UNIVERSIDAD DEL NORTE"),
              tags$h1(class = "cover-title", T("Señal"), tags$br(), tags$span(class = "glitch", `data-text` = T("perdida"), T("perdida"))),
              tags$p(class = "cover-lead", T("Uno de cada cuatro clientes "), tags$b(T("cuelga para siempre")), T(". Descubre quiénes son, por qué se van y cómo anticiparlo con datos.")),
              div(class = "cover-stats",
                  div(counter(7043), tags$small(T("clientes analizados"))),
                  div(class = "hot", counter(26.5, 1, " %"), tags$small(T("abandonan el servicio"))),
                  div(counter(12), tags$small(T("modelos finales"))),
                  div(counter(round(ll, 3), 3), tags$small(T("log-loss del modelo propio")))),
              div(class = "cover-cta",
                  tags$a(class = "btn primary", href = "#problema", T("Entrar al dashboard"), " →"),
                  tags$a(class = "btn ghost", href = "#modelos", T("Predecir un cliente"), icon("arrow", "sm"))),
              div(class = "cover-hint", icon("cursor"),
                  tags$span(T("Los puntos rojos son clientes a punto de irse: "), tags$b(T("pasa el cursor (o el dedo) sobre ellos para retenerlos."))),
                  tags$span(class = "score", T("Perdidos: "), tags$b(id = "cover-lost", "0"), " · ", T("Retenidos por ti: "), tags$b(id = "cover-saved", "0")))),
          phone()),
      div(class = "cover-foot", "Laura S. Rivera Figueredo · Natalý A. Cárdenas Izaquita"))
}

problema <- function() {
  tagList(
    div(class = "hero", id = "problema",
        div(class = "hero-text", eyebrow(T("CONTEXTO DEL PROBLEMA")), tags$h2(T("1 de cada 4 clientes "), tags$span(class = "txt-hot", T("se desconecta"))),
            tags$p(T(paste0("Cada cancelación es un ingreso recurrente que se pierde, y conseguir un cliente nuevo cuesta entre 5 y 25 veces más que conservar uno ",
                            "que ya existe. Anticipar quién está por irse permite dirigir las acciones de retención a los clientes en riesgo en lugar de a toda la base."))),
            tags$p(class = "muted small", T("Fuente de la cifra: "), tags$a(href = FUENTE_CIFRA, target = "_blank", "Gallo, A. (2014). The value of keeping the right customers. Harvard Business Review.")),
            people_row()),
        kpis(kpi(T("CLIENTES"), 7043, T("una fila por cliente")), kpi(T("ABANDONOS"), 1869, T("cancelaron en el último mes")),
             kpi(T("TASA DE ABANDONO"), 26.5, T("la clase minoritaria"), "hot", 1, " %"), kpi(T("VARIABLES"), 19, T("predictoras candidatas")), cls = "k2")),
    subhead(T("Pregunta de investigación")),
    card(cls = "glow",
         tags$p(class = "big-text", T(paste0("¿Combinar 11 clasificadores calibrados con pesos que cambian según la región del espacio de clientes da probabilidades de abandono ",
                                             "mejores (menor log-loss) que el mejor de esos clasificadores por sí solo?"))),
         tags$p(class = "insight", T(paste0("Respuesta corta: no en Telco. RegionalMAPEM empata con la logística con splines (0,41127 frente a 0,41130) y supera a 7 de los 11 benchmarks, ",
                                            "pero ninguna prueba del protocolo confirma la diferencia. El detalle está en la sección Modelos.")))),
    subhead(T("Objetivo general")),
    card(tags$p(class = "big-text", T(paste0("Identificar a los clientes de una empresa de telecomunicaciones con mayor riesgo de abandonar el servicio y los factores asociados a ese abandono, ",
                                             "construyendo y comparando de forma reproducible modelos de clasificación, incluido un modelo propio, que entreguen probabilidades de abandono bien calibradas.")))),
    subhead(T("Objetivos específicos")),
    div(class = "objectives", lapply(seq_along(OBJECTIVES), function(i) div(class = "obj", tags$span(class = "obj-n", sprintf("%02d", i)), tags$h4(T(OBJECTIVES[[i]][1])), tags$p(T(OBJECTIVES[[i]][2]))))))
}

datos <- function() {
  variables <- do.call(rbind, lapply(GROUPS, function(g) do.call(rbind, lapply(g[[2]], function(v) df(
    Grupo = T(g[[1]]), Variable = v,
    Significado = if (v %in% names(LABELS)) label(v) else if (v == "customerID") T("Código del cliente") else T("Abandono durante el último mes"),
    Tipo = if (v %in% c(NUMERIC, "SeniorCitizen")) T("Numérica") else if (v == "customerID") T("Identificador") else T("Categórica"),
    Papel = if (v == "Churn") T("Objetivo") else if (v == "customerID") T("No predictor") else T("Predictora"))))))
  names(variables) <- T(names(variables))
  ficha <- df(c(T("Fuente"), T("Periodo"), T("Licencia"), T("Observaciones"), T("Variables"), T("Variable objetivo"), T("Tipo de tarea"), T("Faltantes")),
              c(T("IBM, conjunto de muestra publicado en Kaggle (blastchar/telco-customer-churn)"), T("Una sola fotografía de corte transversal; el archivo no trae fechas"),
                T("La de la publicación en Kaggle; datos de demostración de IBM para uso educativo"), T("7043 clientes: 5634 de desarrollo y 1409 de hold-out"),
                T("21 columnas: 19 predictoras, el identificador y la respuesta"), T("Churn (Yes/No): canceló en el último mes; 26,5 % Yes"),
                T("Clasificación binaria con salida probabilística"), T("11 vacíos de TotalCharges, todos con tenure = 0; se imputan con 0")))
  names(ficha) <- c(T("Campo"), T("Valor"))
  tagList(subhead(T("Descripción de los datos"), T("Ficha resumida de DATA_STATEMENT.md y diccionario de variables.")),
          grid(card(tags$h3(class = "chart-title", T("Ficha del conjunto")), table_ui(ficha, page_size = 8, wrap = TRUE)),
               card(tags$h3(class = "chart-title", T("Diccionario de variables")), table_ui(variables, page_size = 8, filter = TRUE))))
}

metodologia <- function() {
  tagList(subhead(T("Metodología"), T("Del archivo original al veredicto del modelo propio.")),
          card(div(class = "pipeline wide", div(class = "flow-line"), lapply(seq_along(PIPELINE), function(k) div(class = "station", style = sprintf("animation-delay:%.2fs", (k - 1) * .12),
               tags$span(class = "st-icon", icon(PIPELINE[[k]][1])), tags$h4(T(PIPELINE[[k]][2])), tags$p(T(PIPELINE[[k]][3])))))),
          subhead(T("Marco teórico"), T("Pasa el cursor (o toca) cada tarjeta para voltearla.")),
          div(class = "flips", lapply(FLIP, function(f) div(class = "flip", tabindex = "0", div(class = "flip-inner",
              div(class = "flip-front", tags$span(class = "flip-icon", icon(f[1])), tags$h4(T(f[2]))),
              div(class = "flip-back", tags$h4(T(f[2])), tags$p(T(f[3]))))))))
}

cierre <- function() {
  limits <- c(N("Un solo operador ficticio y una sola fotografía en el tiempo: los resultados no se trasladan sin validar a otra población."),
              N("En Telco las regiones no aportan frente a pesos globales: el modelo propio se comporta como un ensamble ponderado."),
              N("RegionalMAPEM necesita a los 11 expertos para predecir, lo que encarece la inferencia."),
              N("Sin costos de retención explícitos, el umbral de alerta no se traduce en dinero."))
  ethics <- c(N("gender y SeniorCitizen están entre las variables. Su peso SHAP es bajo, pero no se hizo un análisis formal de equidad entre grupos."),
              N("El riesgo predicho no debe usarse para negar beneficios ni cambiar condiciones de un cliente sin revisión humana."),
              N("Las atribuciones SHAP describen al modelo, no causas del abandono."))
  people <- list(c("laura.jpg", "Laura S. Rivera Figueredo", "sriveral@uninorte.edu.co", "https://github.com/lauraformore", "https://www.linkedin.com/in/laura-s-rivera-figueredo-862451315/"),
                 c("nataly.jpg", "Natalý A. Cárdenas Izaquita", "nizaquita@uninorte.edu.co", "https://github.com/Nataly-Cardenas", "https://www.linkedin.com/in/n%C3%A1taly-c%C3%A1rdenas-iz%C3%A1quita"))
  res <- list(c("code", T("Código y notebooks"), T("Repositorio Telco_Churn_JBook"), REPO), c("sliders", T("Este dashboard"), T("Repositorio Shiny_Telco_Churn"), SHINY_REPO),
              c("table", T("Conjunto de datos"), "Kaggle · Telco Customer Churn (IBM)", "https://www.kaggle.com/datasets/blastchar/telco-customer-churn"), c("book", "Jupyter Book", T("Informe técnico completo"), BOOK))
  tagList(
    grid(card(eyebrow(T("LIMITACIONES")), tags$ul(lapply(limits, function(x) tags$li(T(x))))),
         card(eyebrow(T("ÉTICA Y SESGO")), tags$ul(lapply(ethics, function(x) tags$li(T(x)))))),
    subhead(T("Autoras")),
    div(class = "authors", lapply(people, function(p) div(class = "author",
        div(class = "avatar", tags$img(src = paste0("img/", p[1]), alt = fmt(T("Foto de {n}"), n = p[2]))), tags$h2(p[2]),
        tags$p(class = "muted", T("Ciencia de Datos · Universidad del Norte")),
        div(class = "author-links", tags$a(class = "chip-link linkedin", href = p[5], target = "_blank", icon("linkedin", "sm"), "LinkedIn"),
            tags$a(class = "chip-link", href = p[4], target = "_blank", icon("code", "sm"), "GitHub"),
            tags$a(class = "chip-link", href = paste0("mailto:", p[3]), icon("mail", "sm"), p[3]))))),
    tags$p(class = "center muted", T("Departamento de Matemáticas, Física y Ciencia de Datos · Universidad del Norte · Barranquilla, Colombia")),
    do.call(grid, c(lapply(res, function(r) tags$a(class = "res", href = r[4], target = "_blank", tags$h4(icon(r[1]), r[2]), tags$p(r[3]))), cols = 4)),
    tags$p(class = "center muted version", fmt(T("Versión {v} · actualizado el {f} · referencias completas en el Jupyter Book."), v = VERSION, f = ACTUALIZADO)))
}

contexto_page <- function() {
  tagList(cover(),
          div(class = "page contexto",
              page_head(T("Contexto"), T("Por qué importa anticipar el abandono, qué datos usamos y cómo llegamos al modelo propio."), "intro"),
              problema(), datos(), metodologia(), cierre()))
}
