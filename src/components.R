# Componentes de la interfaz y estilo común de las gráficas.

BOOK <- "https://nataly-cardenas.github.io/Telco_Churn_JBook/"
REPO <- "https://github.com/Nataly-Cardenas/Telco_Churn_JBook"
SHINY_REPO <- "https://github.com/Nataly-Cardenas/Shiny_Telco_Churn"

# ---------------------------------------------------------------- paleta
# Un solo color para todas las series; el segundo color solo resalta el mejor resultado, la clase de abandono
# o el dato que la gráfica quiere señalar. Son los mismos tonos del Jupyter Book.
BASE <- "#2E86AB"; HOT <- "#E4572E"; GREY <- "#9AA5B1"
LOW <- BASE; MID <- GREY; HIGH <- HOT                    # niveles de riesgo del simulador
SEQ <- list(list(0, "rgba(46,134,171,.06)"), list(1, BASE))   # escala de una sola tinta
DIVERGING <- list(list(0, BASE), list(.5, "rgba(154,165,177,.15)"), list(1, HOT))
GRID <- "rgba(139,147,167,.18)"
SOFT <- "rgba(46,134,171,.45)"   # el mismo azul, más suave: la versión de referencia (sin calibrar, estándar)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Colores de barras: todas en BASE y la mejor (o la indicada, índice desde 1) en HOT.
highlight <- function(values, best = NULL) {
  if (is.null(best)) best <- if (length(values)) which.max(values) else 0
  ifelse(seq_along(values) %in% best, HOT, BASE)
}

f2 <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
f0 <- function(x) formatC(round(x), format = "d", big.mark = ",")
pct <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")

# ---------------------------------------------------------------- traducción de datos
cat_name <- function(v) {
  if (is.numeric(v)) return(if (get_lang() == "en") c("No", "Yes")[v + 1] else c("No", "Sí")[v + 1])
  if (get_lang() == "en") return(v)
  out <- unname(ES[v]); ifelse(is.na(out), v, out)
}
label <- function(var) T(ifelse(var %in% names(LABELS), LABELS[var], var))
model_name <- function(m) T(unname(MODELS[m]))
balance_name <- function(b) T(unname(BALANCES[b]))
optimizer_name <- function(o) T(unname(OPTIMIZERS[o]))
config_name <- function(e) vapply(strsplit(e, "__", fixed = TRUE), function(p) paste(model_name(p[1]), balance_name(p[2]), optimizer_name(p[3]), sep = " · "), "")
outcome_colors <- function() setNames(c(BASE, HOT), T(c("Permanece", "Abandona")))
churn_label <- function(v) ifelse(v == "Yes", T("Abandona"), T("Permanece"))

# ---------------------------------------------------------------- componentes
card <- function(..., cls = "") tags$section(class = paste("card", cls), ...)
note <- function(title, text, cls = "") div(class = paste("note", cls), tags$strong(title), tags$p(text))
grid <- function(..., cols = 2) div(class = paste0("grid g", cols), ...)
eyebrow <- function(text) tags$span(class = "eyebrow", text)
field <- function(text, control, id = NULL) div(class = "field", tags$label(id = id, text), control)
counter <- function(value, decimals = 0, suffix = "", prefix = "")
  tags$span(class = "count", `data-count` = as.character(value), `data-decimals` = as.character(decimals), `data-suffix` = suffix, `data-prefix` = prefix)
kpi <- function(label_, value, sub = "", accent = "", decimals = 0, suffix = "", prefix = "") {
  v <- if (is.numeric(value)) counter(value, decimals, suffix, prefix) else value
  div(class = "kpi", tags$span(class = "k-label", label_), tags$strong(class = paste("k-value", accent), v), tags$small(sub))
}
kpis <- function(..., cls = "") div(class = paste("kpis", cls), ...)
details <- function(summary, ...) tags$details(class = "details", tags$summary(summary), ...)
subhead <- function(title, sub = NULL) div(class = "section-head", tags$h2(title), if (!is.null(sub)) tags$p(class = "muted", sub))
empty <- function() note(T("Sin clientes en este segmento"), T("Amplía los filtros para volver a calcular las visualizaciones."))
scope <- function(d) div(class = "scope", tags$span(class = "live-dot"), paste0(" ", fmt(T("{n} clientes en la selección"), n = f0(nrow(d)))))

pm <- function(m, s, d = 4) sprintf(paste0("%.", d, "f ± %.", d, "f"), m, s)

# Mensaje de error o advertencia (equivale a dbc.Alert con el estilo del tablero).
alert <- function(title, text, kind = "warning") div(class = paste("alert-dash", kind), role = "alert", tags$strong(title), tags$p(text))

# Markdown con fórmulas: KaTeX las dibuja en el navegador (equivale a dcc.Markdown(mathjax=True)).
# Las fórmulas se apartan antes de convertir el markdown para que sus _ y \\ no se lean como formato.
md <- function(text, cls = "prose") {
  m <- gregexpr("\\$\\$[^$]+\\$\\$|\\$[^$\n]+\\$", text)
  math <- regmatches(text, m)[[1]]
  keys <- sprintf("MATHX%03dX", seq_along(math))
  if (length(math)) regmatches(text, m) <- list(keys)
  h <- commonmark::markdown_html(text, extensions = TRUE)
  for (i in seq_along(math)) h <- sub(keys[i], htmlEscape(math[i]), h, fixed = TRUE)
  div(class = paste(cls, "md-math"), HTML(h))
}

# Botones de opción (dcc.RadioItems) y casilla (dcc.Checklist de una opción) que Shiny reconoce como entradas.
radio <- function(id, values, labels, selected, cls = "radio") {
  div(id = id, class = paste("shiny-input-radiogroup", cls), lapply(seq_along(values), function(i)
    tags$label(tags$input(type = "radio", name = id, value = values[i], checked = if (values[i] == selected) NA), tags$span(paste0(" ", labels[i])))))
}
check <- function(id, text, value = FALSE) div(class = "check", tags$label(tags$input(id = id, type = "checkbox", checked = if (value) NA), tags$span(paste0(" ", text))))

# Caja numérica (dcc.Input type = number de Dash 4, con los botones − y + a los lados; app.js los hace funcionar).
num_input <- function(id, min, max, step, value = NULL, placeholder = NULL)
  div(class = "dash-input-container dash-input num-input",
      tags$input(id = id, type = "number", class = "dash-input-element", min = min, max = max, step = step, value = value, placeholder = placeholder, autocomplete = "off"),
      tags$button(type = "button", class = "dash-input-stepper dash-stepper-decrement", `aria-controls` = id, `aria-label` = "Decrease value", "−"),
      tags$button(type = "button", class = "dash-input-stepper dash-stepper-increment", `aria-controls` = id, `aria-label` = "Increase value", "+"))

# Lista de selección (selectize) con etiquetas traducidas.
dropdown <- function(id, values, value, labels = NULL, multi = FALSE) {
  lab <- vapply(values, function(v) if (!is.null(labels) && v %in% names(labels)) labels[[v]] else as.character(cat_name(v)), "")
  selectizeInput(id, NULL, choices = setNames(values, lab), selected = value, multiple = multi, width = "100%",
                 options = c(list(searchField = if (multi || length(values) > 8) "label" else list(), dropdownParent = "body"),   # el menú se abre sobre todo el contenido, como el portal de Dash
                             if (multi) list(items = as.list(value))))   # varias opciones: en el orden elegido, como el value de Dash
}

# Deslizador con marcas y caja numérica (mismo aspecto que dcc.Slider / dcc.RangeSlider de Dash 4).
slider <- function(id, min, max, step, value, marks, tooltip = FALSE) {
  range <- length(value) == 2
  box <- function(v, cls) tags$input(type = "number", class = paste("ds-box", cls), min = min, max = max, step = step, value = v)
  div(id = id, class = paste("dslider", if (range) "range" else "", if (tooltip) "tip" else ""), `data-min` = min, `data-max` = max, `data-step` = step,
      if (range) box(value[1], "lo"),
      div(class = "ds-body",
          div(class = "ds-track", div(class = "ds-rail"), div(class = "ds-fill"),
              if (range) tags$input(type = "range", class = "ds-in lo", min = min, max = max, step = step, value = value[1]),
              tags$input(type = "range", class = "ds-in hi", min = min, max = max, step = step, value = value[length(value)]),
              if (tooltip) tags$span(class = "ds-tip")),
          div(class = "ds-marks", lapply(seq_along(marks), function(i) tags$span(`data-p` = sprintf("%.4f", 100 * (as.numeric(names(marks)[i]) - min) / (max - min)), marks[[i]])))),
      box(value[length(value)], "hi"))
}

# ---------------------------------------------------------------- tablas
# Traduce encabezados y textos de una tabla.
tt <- function(d) {
  names(d) <- T(names(d))
  for (c in names(d)) if (is.character(d[[c]])) d[[c]] <- T(d[[c]])
  d
}

# Tabla con orden, paginación y filtro en el navegador (mismo aspecto que dash_table.DataTable).
# hot_rows: filas resaltadas (style_data_conditional del dash); tooltips: texto de ayuda por encabezado; wrap: celdas en varias líneas.
# Número como lo escribe el DataTable de Dash (String(x) de JavaScript): notación científica solo por debajo de 1e-6.
js_num <- function(x) {
  if (is.na(x)) return("")
  if (x != 0 && abs(x) < 1e-6) { e <- floor(log10(abs(x))); m <- signif(x / 10^e, 15); return(paste0(format(m, digits = 15, trim = TRUE), "e", e)) }
  format(x, scientific = FALSE, digits = 15, trim = TRUE)
}

# Íconos del paginador del DataTable de Dash (Font Awesome: angle-double-left, angle-left, angle-right, angle-double-right).
PAGER_ICONS <- div(class = "dt-icons", hidden = NA, lapply(list(c("dl", 448, "M223.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L319.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L393.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34zm-192 34l136 136c9.4 9.4 24.6 9.4 33.9 0l22.6-22.6c9.4-9.4 9.4-24.6 0-33.9L127.9 256l96.4-96.4c9.4-9.4 9.4-24.6 0-33.9L201.7 103c-9.4-9.4-24.6-9.4-33.9 0l-136 136c-9.5 9.4-9.5 24.6-.1 34z"), c("l", 256, "M31.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L127.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L201.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34z"),
                                                                c("r", 256, "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34z"), c("dr", 448, "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34zm192-34l-136-136c-9.4-9.4-24.6-9.4-33.9 0l-22.6 22.6c-9.4 9.4-9.4 24.6 0 33.9l96.4 96.4-96.4 96.4c-9.4 9.4-9.4 24.6 0 33.9l22.6 22.6c9.4 9.4 24.6 9.4 33.9 0l136-136c9.4-9.2 9.4-24.4 0-33.8z")), function(i)
  tags$span(`data-k` = i[1], HTML(sprintf('<svg aria-hidden="true" focusable="false" class="svg-inline--fa" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s 512"><path fill="currentColor" d="%s"></path></svg>', i[2], i[3])))))

# Ícono de orden del DataTable de Dash (fa-sort); cada mitad se apaga según la dirección del orden.
SORT_ICON <- tags$span(class = "dt-sort", HTML(paste0('<svg aria-hidden="true" focusable="false" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512">',
  '<path class="dn" fill="currentColor" d="M41 288h238c21.4 0 32.1 25.9 17 41L177 448c-9.4 9.4-24.6 9.4-33.9 0L24 329c-15.1-15.1-4.4-41 17-41z"></path>',
  '<path class="up" fill="currentColor" d="M296 183L177 64c-9.4-9.4-24.6-9.4-33.9 0L24 183c-15.1 15.1-4.4 41 17 41h238c21.4 0 32.1-25.9 17-41z"></path></svg>')))

# head_wrap: encabezados en varias líneas (la tabla de src/ui.py del dash, usada en «Modelos finales»).
table_ui <- function(d, page_size = 10, filter = FALSE, digits = 4, wrap = FALSE, hot_rows = integer(), tooltips = list(), head_wrap = FALSE) {
  if (is.null(d) || !ncol(d)) return(NULL)
  cell <- function(v) {
    if (is.logical(v)) return(ifelse(is.na(v), "", ifelse(v, T("Sí"), "No")))
    if (is.numeric(v)) return(ifelse(is.na(v), "", vapply(round(v, digits), js_num, "")))
    ifelse(is.na(v), "", as.character(v))
  }
  cols <- lapply(d, cell)
  num <- vapply(d, is.numeric, TRUE)
  body <- vapply(seq_len(nrow(d)), function(i) paste0(if (i %in% hot_rows) "<tr class=\"hot-row\">" else "<tr>",
                                                     paste0("<td>", htmlEscape(vapply(cols, `[`, "", i)), "</td>", collapse = ""), "</tr>"), "")
  div(class = paste("dtable", if (wrap) "wrap", if (head_wrap) "hwrap"), `data-page-size` = page_size,
      tags$div(class = "dt-scroll", tags$table(
        tags$thead(tags$tr(lapply(seq_along(d), function(j) tags$th(`data-num` = if (num[j]) "1" else "0", title = tooltips[[names(d)[j]]], div(class = "th-in", SORT_ICON, tags$span(class = "dt-name", names(d)[j]))))),
                   if (filter) tags$tr(class = "dt-filter", lapply(seq_along(d), function(j) tags$th(div(tags$input(type = "text", placeholder = "filter data..."),
                                                                                                  tags$input(type = "button", class = "dash-filter--case", title = "Toggle filter case sensitivity", value = "Aa")))))),
        tags$tbody(HTML(paste(body, collapse = ""))))),
      PAGER_ICONS, div(class = "dt-pager"))
}

# ---------------------------------------------------------------- gráficas
style_fig <- function(p, height = 360, legend = TRUE, margin = NULL) {
  # boxmode existe en plotly.js aunque el esquema de plotly para R no lo declare: solo se silencia ese aviso.
  p <- withCallingHandlers(plotly_build(p), warning = function(w) if (grepl("boxmode", conditionMessage(w))) invokeRestart("muffleWarning"))
  L <- p$x$layout
  below <- !is.null(L$legend$y) && L$legend$y < 0          # leyenda pedida debajo de la gráfica
  margin <- margin %||% if (!is.null(L$margin$b)) L$margin  # margen propio de la figura
  axis <- list(gridcolor = GRID, zerolinecolor = GRID, linecolor = GRID, automargin = TRUE)
  # La matriz de dispersión crea un eje por dimensión al dibujar: se declaran para darles el mismo estilo.
  nd <- max(0, unlist(lapply(p$x$data, function(t) if (identical(t$type, "splom")) length(t$dimensions) else 0)))
  extra <- if (nd > 1) c(paste0("xaxis", 2:nd), paste0("yaxis", 2:nd)) else NULL
  for (ax in union(c("xaxis", "yaxis", extra), grep("^[xy]axis[0-9]*$", names(L), value = TRUE))) L[[ax]] <- utils::modifyList(L[[ax]] %||% list(), axis)
  L <- utils::modifyList(L, list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", autosize = TRUE, showlegend = legend,
    font = list(family = "Inter, sans-serif", color = "#8792a8", size = 13.5), colorway = c(BASE, HOT, GREY),
    margin = margin %||% list(l = 20, r = 20, t = 36, b = 30, pad = 0),
    legend = list(orientation = "h", y = if (below) L$legend$y else 1.14, yanchor = if (below) "top" else "auto", title = list(text = ""), bgcolor = "rgba(0,0,0,0)"),
    hoverlabel = list(bgcolor = "#0b1426", bordercolor = BASE, font = list(family = "Inter", color = "#eaf2ff", size = 13)),
    # Ejes que Plotly crea al dibujar (matriz de dispersión) heredan la rejilla de la plantilla: se ajusta también ahí.
    template = list(layout = list(xaxis = axis, yaxis = axis))))
  L$height <- NULL   # la altura la pone el contenedor (graph): así cada gráfica cabe completa en la pantalla al proyectar
  p$x$layout <- L
  p$x$config <- utils::modifyList(p$x$config %||% list(), list(displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d")))
  p$height <- "100%"; p$width <- "100%"
  p$sizingPolicy$defaultWidth <- "100%"
  p
}

# Nunca más alta que la pantalla disponible bajo la barra superior.
graph <- function(p, height = 360, legend = TRUE, margin = NULL)
  div(class = "graph", style = sprintf("height:min(%dpx, calc(100vh - 230px));min-height:%dpx;width:100%%", height, min(height, 240)), style_fig(p, height, legend, margin))

chart <- function(title, p, insight = "", height = 360, legend = TRUE)
  card(tags$h3(class = "chart-title", title), graph(p, height, legend), if (nzchar(insight)) tags$p(class = "insight", insight))

# Valores escritos al inicio visible de cada barra, lejos de los bigotes de error (el eje no empieza en cero).
bar_labels <- function(p, values, cats, at, digits = 3, horizontal = TRUE) {
  txt <- formatC(values, format = "f", digits = digits)
  if (horizontal) add_trace(p, type = "scatter", mode = "text", x = rep(at, length(values)), y = cats, text = txt, textposition = "middle right",
                            textfont = list(color = "#ffffff", size = 13), hoverinfo = "skip", showlegend = FALSE, inherit = FALSE)
  else add_trace(p, type = "scatter", mode = "text", x = cats, y = rep(at, length(values)), text = txt, textposition = "top center",
                 textfont = list(color = "#ffffff", size = 13), hoverinfo = "skip", showlegend = FALSE, inherit = FALSE)
}

# Eje categórico en el orden dado (Plotly en R no siempre conserva el orden de aparición).
cat_axis <- function(values, ...) list(type = "category", categoryorder = "array", categoryarray = values, ...)

# Mapa de calor con el valor escrito en cada celda (equivale a px.imshow con text_auto).
heatmap <- function(z, x, y, colorscale, zmin = NULL, zmax = NULL, digits = NULL, textsize = 13, showscale = TRUE, colorbar_title = NULL) {
  txt <- if (is.null(digits)) formatC(z, format = "d", big.mark = "") else formatC(z, format = "f", digits = digits)
  dim(txt) <- dim(z)
  plot_ly(type = "heatmap", z = z, x = x, y = y, colorscale = colorscale, zmin = zmin, zmax = zmax, text = txt, texttemplate = "%{text}",
          textfont = list(size = textsize), hovertemplate = "%{y} · %{x}: %{text}<extra></extra>", showscale = showscale,
          colorbar = list(title = list(text = colorbar_title %||% ""), len = 1, y = .5, yanchor = "middle", thickness = 30))
}

# Varias gráficas en una figura con un título encima de cada una. Reproduce make_subplots de Plotly en Python:
# mismos dominios (column_widths, horizontal_spacing = 0.2 / columnas, vertical_spacing = 0.3 / filas) y títulos.
subplots <- function(plots, titles = NULL, nrows = 1, widths = NULL, hspace = NULL, vspace = NULL, share_y = FALSE, title_size = 16) {
  n <- length(plots); ncols <- ceiling(n / nrows)
  hspace <- hspace %||% (0.2 / ncols); vspace <- vspace %||% (0.3 / nrows)
  widths <- widths %||% rep(1, ncols); w <- (1 - hspace * (ncols - 1)) * widths / sum(widths); h <- (1 - vspace * (nrows - 1)) / nrows
  b <- plotly_build(plotly::subplot(plots, nrows = nrows, margin = 0, titleX = TRUE, titleY = TRUE))
  L <- b$x$layout; ann <- list()
  for (i in seq_len(n)) {
    r <- (i - 1) %/% ncols; c <- (i - 1) %% ncols
    xd <- c(sum(w[seq_len(c)]) + c * hspace, 0); xd[2] <- xd[1] + w[c + 1]
    yd <- c(0, 1 - r * (h + vspace)); yd[1] <- yd[2] - h
    xa <- if (i == 1) "xaxis" else paste0("xaxis", i); ya <- if (i == 1) "yaxis" else paste0("yaxis", i)
    L[[xa]]$domain <- xd; L[[ya]]$domain <- yd
    if (share_y && i > 1) { L[[ya]]$matches <- "y"; L[[ya]]$showticklabels <- FALSE }
    if (!is.null(titles))
      ann[[i]] <- list(text = titles[i], x = mean(xd), y = yd[2], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE, font = list(size = title_size))
  }
  L$annotations <- c(L$annotations %||% list(), ann)
  b$x$layout <- L
  b
}

# Encabezado de página con un único enlace al capítulo del Jupyter Book. key es la ruta (título de TITLES)
# o, como en la sección «Modelos finales» del dash, el título ya traducido con su capítulo.
page_head <- function(key, lead, chapter = CHAPTER[[key]]) {
  title <- if (!is.null(TITLES[[key]])) T(TITLES[[key]]) else key
  link <- if (!is.null(chapter)) tags$a(class = "chip-link", href = paste0(BOOK, chapter, ".html"), target = "_blank",
                                        icon("book", "sm"), T("Ver este capítulo en el Jupyter Book"), icon("arrow", "sm"))
  div(class = "page-head", eyebrow("TELCO CUSTOMER CHURN"), tags$h1(title), tags$p(class = "lead", lead), link)
}

# Pestañas: la barra se pinta aquí y el contenido de la pestaña activa lo genera el servidor (uiOutput "<id>-body").
tabs <- function(id, labels, selected = 0) {
  div(class = "tabs-wrap",
      div(class = "tabs", id = id, `data-value` = selected,
          lapply(seq_along(labels) - 1, function(i) tags$button(type = "button", class = paste("tab", if (i == selected) "on"), `data-i` = i, labels[i + 1]))),
      uiOutput(paste0(id, "-body")))
}
tab_body <- function(...) div(class = "tab-body", ...)
