# Asistente del dashboard: responde preguntas sobre el proyecto, sus datos y cada sección, sin límite de preguntas.
#
# Funciona en dos modos:
# - Con la variable de entorno ANTHROPIC_API_KEY, redacta la respuesta con Claude a partir del contexto recuperado.
# - Sin ella (o si la API falla), responde con la entrada más parecida de la base de conocimiento local (data/faq.json).

MODEL <- "claude-opus-5-5"
FAQ <- jsonlite::fromJSON(file.path(DATA, "faq.json"), simplifyDataFrame = FALSE)
SUGGESTIONS <- c(N("¿Qué clientes abandonan más?"), N("¿Cuál fue el mejor modelo?"), N("¿Qué es SMOTE y por qué se usó?"), N("¿Cómo uso el simulador?"), N("¿Cómo descargo los datos?"))

SYSTEM <- paste(sep = "\n",
  'Eres el asistente del dashboard "Señal perdida", sobre el proyecto Telco Customer Churn de Laura S. Rivera Figueredo y Natalý A. Cárdenas Izaquita (Universidad del Norte).',
  "Respondes preguntas de cualquier visitante sobre el proyecto, sus resultados y cómo usar el dashboard.",
  "",
  "Hechos del proyecto:",
  "- Datos: Telco Customer Churn de IBM, 7,043 clientes, 21 columnas, 26.5 % de abandono. Partición estratificada 80/20 (5,634 entrenamiento, 1,409 test).",
  "- Tasas de abandono más altas: cheque electrónico 45.7 %, contrato mes a mes 42.7 %, fibra óptica 42.1 %, sin seguridad en línea 41.9 %, sin soporte técnico 41.8 %. Con contrato a dos años: 2.9 %.",
  "- Diseño: 7 familias de modelos × 4 balanceos × 4 optimizadores = 112 configuraciones, validación cruzada anidada (5 folds externos, 3 internos), F1 de la clase de abandono como métrica principal.",
  "- Resultados: Random Forest + SMOTE F1 0.638 ± 0.023 (AUC 0.844), XGBoost + SMOTE 0.638, regresión logística 0.634; sin diferencias estadísticamente detectables. SMOTE mejoró 6 de 7 familias. La calibración sigmoide mejoró Brier y ECE en 80 de 96 configuraciones.",
  "- SHAP: contrato, servicio de internet y antigüedad son las variables más importantes. Las atribuciones no son causales.",
  "- Secciones del dashboard: Portada (red de clientes y celular con clientes reales); Introducción (contexto, objetivos, marco teórico, variables);",
  "  Análisis exploratorio (ETL y partición, univariado, bivariado, multivariado, valores faltantes, resumen, explorador con descarga CSV y filtros);",
  "  Preprocesamiento (pipeline, imputación, validación cruzada, ablación, evaluación del modelo base en test); Modelado avanzado (podio, resultados por modelo,",
  "  calibración, optimizadores, comparación estadística, SHAP y LIME); Simulador (perfil de riesgo, clientes reales, umbral de alerta); Conclusiones; Autoras.",
  "  Botones ES/EN y modo claro/oscuro arriba a la derecha.",
  "",
  "Reglas:",
  "- Responde en el idioma que te indiquen, en 2 a 5 frases, con markdown sencillo. Cita cifras solo si aparecen en estos hechos o en el contexto recibido.",
  "- Si la pregunta no tiene que ver con el proyecto o el dashboard, dilo con amabilidad y sugiere qué se puede preguntar.",
  "- Si el contexto no alcanza para responder, dilo y recomienda la sección del dashboard donde buscar.")

# ---------------------------------------------------------------- búsqueda TF-IDF por n-gramas de caracteres
# Equivale a TfidfVectorizer(analyzer='char_wb', ngram_range=(2, 4), strip_accents='unicode', lowercase=True, sublinear_tf=True).
strip_accents <- function(x) chartr("áéíóúüñàèìòùâêîôûäëïöÁÉÍÓÚÜÑÀÈÌÒÙÂÊÎÔÛÄËÏÖ", "aeiouunaeiouaeiouaeioAEIOUUNAEIOUAEIOUAEIO", x)

ngrams <- function(text) {
  words <- strsplit(gsub("\\s+", " ", tolower(strip_accents(text))), " ", fixed = TRUE)[[1]]; words <- words[nzchar(words)]
  out <- character()
  for (w in words) {
    w <- paste0(" ", w, " "); chars <- strsplit(w, "")[[1]]; L <- length(chars)
    for (n in 2:4) {
      if (L <= n) { out <- c(out, w); break }   # palabra corta: se cuenta una sola vez
      out <- c(out, vapply(seq_len(L - n + 1), function(o) paste(chars[o:(o + n - 1)], collapse = ""), ""))
    }
  }
  out
}

tfidf_index <- local({
  idx <- NULL
  function() {
    if (!is.null(idx)) return(idx)
    qs <- unlist(lapply(FAQ, `[[`, "q")); owner <- rep(seq_along(FAQ), vapply(FAQ, function(f) length(f$q), 1L))
    grams <- lapply(qs, ngrams); vocab <- sort(unique(unlist(grams)))
    dfreq <- tabulate(match(unlist(lapply(grams, unique)), vocab), length(vocab))
    idf <- log((1 + length(qs)) / (1 + dfreq)) + 1
    idx <<- list(vocab = vocab, idf = idf, owner = owner, m = do.call(rbind, lapply(grams, function(g) vectorize(g, vocab, idf))))
    idx
  }
})

vectorize <- function(g, vocab, idf) {
  v <- numeric(length(vocab)); t <- table(match(g, vocab)); t <- t[!is.na(names(t))]
  if (length(t)) { k <- as.integer(names(t)); v[k] <- (1 + log(as.numeric(t))) * idf[k] }
  nrm <- sqrt(sum(v^2)); if (nrm > 0) v / nrm else v
}

retrieve <- function(question) {
  ix <- tfidf_index()
  qs <- as.vector(ix$m %*% vectorize(ngrams(question), ix$vocab, ix$idf))
  fs <- vapply(seq_along(FAQ), function(i) max(qs[ix$owner == i]), 1)
  o <- order(-fs)[1:3]
  lapply(o, function(i) list(score = fs[i], i = i))
}

DATA_TERMS <- list(  # (patrón, columna, valor)
  list("mes a mes|month.to.month|mensual", "Contract", "Month-to-month"), list("un a[nñ]o|one.year|anual", "Contract", "One year"), list("dos a[nñ]os|two.year|2 a[nñ]os", "Contract", "Two year"),
  list("fibra|fiber", "InternetService", "Fiber optic"), list("\\bdsl\\b", "InternetService", "DSL"), list("sin internet|no internet", "InternetService", "No"),
  list("cheque electr|electronic check", "PaymentMethod", "Electronic check"), list("cheque por correo|mailed check", "PaymentMethod", "Mailed check"),
  list("transferencia|bank transfer", "PaymentMethod", "Bank transfer (automatic)"), list("tarjeta|credit card", "PaymentMethod", "Credit card (automatic)"),
  list("adultos? mayor|senior", "SeniorCitizen", 1), list("\\bmujer|\\bfemale|\\bwomen\\b", "gender", "Female"), list("\\bhombre|\\bmale\\b|\\bmen\\b", "gender", "Male"),
  list("sin pareja|without (a )?partner", "Partner", "No"), list("con pareja|with (a )?partner", "Partner", "Yes"),
  list("sin dependientes|without dependents", "Dependents", "No"), list("con dependientes|with dependents", "Dependents", "Yes"),
  list("sin soporte|no tech support|without tech support", "TechSupport", "No"), list("con soporte|with tech support", "TechSupport", "Yes"),
  list("sin seguridad|no online security|without online security", "OnlineSecurity", "No"), list("con seguridad|with online security", "OnlineSecurity", "Yes"),
  list("factura electr|paperless", "PaperlessBilling", "Yes"))

norm_text <- function(text) tolower(strip_accents(text))
has <- function(pattern, x) grepl(pattern, x, perl = TRUE)

# Calcula la tasa de abandono en entrenamiento para el perfil que menciona la pregunta; NULL si no aplica.
data_answer <- function(question, lang) {
  q <- norm_text(question)
  if (!has("tasa|abandon|churn|cuant|porcentaje|rate|how many|se van|leave|riesgo|risk", q)) return(NULL)
  d <- rows(TRAIN_IDS); desc <- character(); used <- character()
  for (t in DATA_TERMS) {
    col <- t[[2]]; value <- t[[3]]
    if (!col %in% used && has(norm_text(t[[1]]), q)) {
      d <- d[d[[col]] == value, ]; used <- c(used, col); name <- LABELS[[col]]
      desc <- c(desc, if (lang == "en") paste0(EN[[name]] %||% name, ": ", if (identical(value, 1)) "Yes" else value)
                      else paste0(name, ": ", if (identical(value, 1)) "Sí" else (if (value %in% names(ES)) ES[[value]] else value)))
    }
  }
  m <- regmatches(q, regexec("(menos de|less than|under)\\s+(\\d+)\\s*(mes|month)", q, perl = TRUE))[[1]]
  if (length(m)) { d <- d[d$tenure < as.integer(m[3]), ]; desc <- c(desc, paste0(if (lang == "en") "tenure < " else "antigüedad < ", m[3])) }
  m <- regmatches(q, regexec("(mas de|more than|over)\\s+(\\d+)\\s*(mes|month)", q, perl = TRUE))[[1]]
  if (length(m)) { d <- d[d$tenure > as.integer(m[3]), ]; desc <- c(desc, paste0(if (lang == "en") "tenure > " else "antigüedad > ", m[3])) }
  if (has("primer ano|first year", q) && !grepl("antigüedad", paste(desc, collapse = " "), fixed = TRUE)) {
    d <- d[d$tenure <= 12, ]; desc <- c(desc, if (lang == "en") "tenure ≤ 12" else "antigüedad ≤ 12 meses")
  }
  if (!length(desc)) return(NULL)
  if (!nrow(d)) return(if (lang == "en") "No training customers match that profile." else "Ningún cliente de entrenamiento tiene ese perfil.")
  n <- nrow(d); k <- sum(d$target); profile <- paste(desc, collapse = " · ")
  if (lang == "en")
    return(sprintf("In the training set, customers with **%s** are %s; %s of them churned (**%s**), versus 26.5 %% overall. It is an observed association, not a cause. You can explore it in *Simulator → Risk profile* and *Exploratory analysis → 3 · Bivariate*.", profile, f0(n), f0(k), pct(k / n)))
  sprintf("En el conjunto de entrenamiento, los clientes con **%s** son %s; de ellos abandonaron %s (**%s**), frente al 26.5 %% general. Es una asociación observada, no una causa. Puedes explorarlo en *Simulador → Perfil de riesgo* y en *Análisis exploratorio → 3 · Bivariado*.", profile, f0(n), f0(k), pct(k / n))
}

faq_text <- function(i, lang) FAQ[[i]][[if (lang == "en") "en" else "es"]]

local_answer <- function(question, lang) {
  top <- retrieve(question)[[1]]
  if (top$score >= .5) return(faq_text(top$i, lang))
  computed <- data_answer(question, lang)
  if (!is.null(computed)) return(computed)
  if (top$score >= .35) return(faq_text(top$i, lang))
  T("No encontré una respuesta a eso en el dash. Puedes preguntarme por los datos, los factores de abandono, los modelos, las métricas o cómo usar cada sección.")
}

has_api <- function() nzchar(Sys.getenv("ANTHROPIC_API_KEY"))

# Respuesta redactada por Claude con el contexto recuperado; NULL si no hay API o falla.
llm_answer <- function(question, history, lang) {
  if (!has_api() || !requireNamespace("httr2", quietly = TRUE)) return(NULL)
  faqs <- retrieve(question)
  context <- paste(vapply(Filter(function(f) f$score > .08, faqs), function(f) paste0("- ", FAQ[[f$i]]$es), ""), collapse = "\n")
  computed <- data_answer(question, "es")
  if (!is.null(computed)) context <- paste0(context, "\n- Cálculo sobre los datos de entrenamiento: ", computed)
  language <- if (lang == "en") "inglés" else "español"
  messages <- lapply(utils::tail(history, 6), function(m) list(role = m$role, content = m$text))
  messages <- c(messages, list(list(role = "user", content = sprintf("<contexto>\n%s\n</contexto>\n\nResponde en %s.\n\nPregunta: %s", context, language, question))))
  body <- list(model = MODEL, max_tokens = 1024, system = SYSTEM, messages = messages, cache_control = list(type = "ephemeral"),
               output_config = list(effort = "low"), fallbacks = "default")
  r <- tryCatch(httr2::request("https://api.anthropic.com/v1/messages") |>
                  httr2::req_headers(`x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"), `anthropic-version` = "2026-10-21",
                                     `anthropic-beta` = "server-side-fallback-2026-07-01", `content-type` = "application/json") |>
                  httr2::req_body_json(body, auto_unbox = TRUE) |> httr2::req_timeout(60) |> httr2::req_perform() |> httr2::resp_body_json(),
                error = function(e) NULL)
  if (is.null(r) || identical(r$stop_reason, "refusal")) return(NULL)
  text <- trimws(paste(vapply(Filter(function(b) identical(b$type, "text"), r$content), `[[`, "", "text"), collapse = ""))
  if (nzchar(text)) text else NULL
}

answer <- function(question, history, lang) {
  faqs <- retrieve(question)
  # Preguntas sobre perfiles concretos: la cifra calculada es más precisa que una respuesta general.
  computed <- if (faqs[[1]]$score < .8) data_answer(question, lang) else NULL
  llm_answer(question, history, lang) %||% computed %||% local_answer(question, lang)
}

# ---------------------------------------------------------------- interfaz
chat_ui <- function() {
  div(id = "chat", class = "chat",
      div(id = "chat-panel", class = "chat-panel",
          div(class = "chat-head", tags$span(class = "chat-avatar", icon("signal")), div(tags$strong(id = "chat-title"), tags$small(id = "chat-sub")),
              tags$button(id = "chat-close", class = "icon-btn", type = "button", icon("close"))),
          div(class = "chat-quota", tags$span(id = "chat-quota"), tags$button(id = "chat-new", class = "chat-new", type = "button", icon("reset", "sm"), tags$span(id = "chat-new-t"))),
          div(class = "chat-loading", uiOutput("chat-messages", class = "chat-messages")),
          tags$form(class = "chat-form", id = "chat-form", tags$input(id = "chat-input", type = "text", autocomplete = "off", maxlength = "400"),
                    tags$button(id = "chat-send", class = "chat-send", type = "button", icon("send")))),
      tags$button(id = "chat-fab", class = "chat-fab", type = "button", icon("chat")))
}

md_html <- function(text) {
  h <- commonmark::markdown_html(text, extensions = TRUE)
  HTML(gsub("<a href=", "<a target=\"_blank\" href=", h, fixed = TRUE))
}

bubble <- function(role, text) div(class = paste("msg", role), md_html(text))

chat_messages <- function(log) {
  hello <- T("¡Hola! Soy el asistente de Señal perdida. Pregúntame lo que quieras sobre el proyecto, sus datos o cualquier sección del dash: no hay límite de preguntas.")
  tagList(bubble("assistant", hello),
          if (!length(log)) div(class = "chat-chips", lapply(SUGGESTIONS, function(s) tags$button(type = "button", class = "chat-chip", `data-q` = T(s), T(s)))),
          lapply(log, function(m) bubble(m$role, m$text)))
}

chat_texts <- function(log) {
  asked <- sum(vapply(log, function(m) m$role == "user", TRUE))
  list(quota = fmt(T("Preguntas ilimitadas · {n} en esta conversación"), n = asked), title = T("Asistente Señal"),
       sub = paste(T("Datos: proyecto Telco Churn"), "·", if (has_api()) T("Respuestas con Claude") else T("Respuestas del proyecto")),
       placeholder = T("Pregunta lo que quieras sobre el dash…"), new = T("Nueva conversación"))
}
