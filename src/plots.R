# Figuras compartidas por las secciones de modelos (equivalen a src/plots.py del dashboard de Dash).

vline <- function(x, color = GREY, dash = "solid", xref = "x", yref = "y domain") list(type = "line", x0 = x, x1 = x, xref = xref, y0 = 0, y1 = 1, yref = yref, line = list(color = color, dash = dash, width = 2))

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

gauge <- function(value, title, color) {
  plot_ly(type = "indicator", mode = "gauge+number", value = value * 100, number = list(suffix = " %", font = list(size = 34)), title = list(text = title, font = list(size = 13)),
          gauge = list(axis = list(range = c(0, 100), tickcolor = "#8792a8"), bar = list(color = color, thickness = .28), bgcolor = "rgba(0,0,0,0)", borderwidth = 0,
                       steps = list(list(range = c(0, 35), color = "rgba(46,134,171,.10)"), list(range = c(35, 60), color = "rgba(154,165,177,.14)"),
                                    list(range = c(60, 100), color = "rgba(228,87,46,.14)"))))
}

roc_fig <- function(y, p, nombre) {
  r <- roc_curve(y, p)
  plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = r$fpr, y = r$tpr, line = list(color = BASE, width = 2.6), name = nombre) %>%
    add_trace(type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), name = T("Azar")) %>%
    layout(xaxis = list(title = T("Tasa de falsos positivos"), range = c(0, 1)), yaxis = list(title = T("Tasa de verdaderos positivos (Recall)"), range = c(0, 1.02)))
}

# Curva precisión-recall como sklearn.metrics.precision_recall_curve.
pr_curve <- function(y, score) {
  o <- order(score, decreasing = TRUE); y <- y[o]; s <- score[o]
  idx <- c(which(diff(s) != 0), length(s))
  tps <- cumsum(y)[idx]; fps <- idx - tps
  prec <- tps / (tps + fps); rec <- tps / sum(y)
  last <- which(tps == max(tps))[1]; k <- rev(seq_len(last))
  list(precision = c(prec[k], 1), recall = c(rec[k], 0))
}

pr_fig <- function(y, p, nombre) {
  r <- pr_curve(y, p)
  plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = r$recall, y = r$precision, line = list(color = BASE, width = 2.6), name = nombre) %>%
    add_trace(type = "scatter", mode = "lines", x = c(0, 1), y = rep(mean(y), 2), line = list(dash = "dash", color = GREY), name = T("Prevalencia")) %>%
    layout(xaxis = list(title = "Recall", range = c(0, 1)), yaxis = list(title = "Precision", range = c(0, 1.02)))
}

# Diagrama de confiabilidad con 10 intervalos uniformes; el tamaño del punto es el número de clientes.
reliability <- function(y, p, bins = 10) {
  b <- pmin(floor(p * bins), bins - 1)
  g <- data.frame(p = tapply(p, b, mean), y = tapply(y, b, mean), n = tapply(y, b, length))
  plot_ly() %>% add_trace(type = "scatter", mode = "lines", x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = GREY), name = T("Calibración perfecta")) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = g$p, y = g$y, name = T("Modelo"), line = list(color = BASE, width = 2.4),
              marker = list(size = 6 + 14 * sqrt(g$n / max(g$n)), color = BASE), customdata = g$n,
              hovertemplate = paste0(T("Probabilidad media"), " %{x:.3f}<br>", T("Frecuencia de abandono"), " %{y:.3f}<br>n = %{customdata}<extra></extra>")) %>%
    layout(xaxis = list(title = T("Probabilidad predicha (media del intervalo)"), range = c(0, 1)), yaxis = list(title = T("Frecuencia observada de abandono"), range = c(0, 1)))
}

# Diagrama de diferencia crítica (Demšar, 2006): rangos medios en un eje y barras que unen los grupos no distinguibles.
cd_diagram <- function(rangos, cd, resaltar, nombres) {
  r <- sort(unlist(rangos)); k <- length(r); nm <- function(m) if (m %in% names(nombres)) nombres[[m]] else m
  col <- function(m) if (m == resaltar) HOT else GREY
  p <- plot_ly(); ann <- list()
  izq <- r[seq_len((k + 1) %/% 2)]; der <- r[-seq_len((k + 1) %/% 2)]
  for (i in seq_along(izq)) {
    m <- names(izq)[i]; v <- izq[[i]]
    p <- add_trace(p, type = "scatter", mode = "lines", x = c(v, v, min(r) - .6), y = c(0, -i, -i), line = list(color = col(m), width = 1.4), hoverinfo = "skip", showlegend = FALSE)
    ann[[length(ann) + 1]] <- list(x = min(r) - .7, y = -i, text = sprintf("%s (%.2f)", nm(m), v), showarrow = FALSE, xanchor = "right", font = list(color = if (m == resaltar) HOT, size = 12))
  }
  dr <- rev(der)
  for (i in seq_along(dr)) {
    m <- names(dr)[i]; v <- dr[[i]]
    p <- add_trace(p, type = "scatter", mode = "lines", x = c(v, v, max(r) + .6), y = c(0, -i, -i), line = list(color = col(m), width = 1.4), hoverinfo = "skip", showlegend = FALSE)
    ann[[length(ann) + 1]] <- list(x = max(r) + .7, y = -i, text = sprintf("(%.2f) %s", v, nm(m)), showarrow = FALSE, xanchor = "left", font = list(color = if (m == resaltar) HOT, size = 12))
  }
  p <- add_trace(p, type = "scatter", mode = "markers", x = unname(r), y = rep(0, k), marker = list(color = ifelse(names(r) == resaltar, HOT, BASE), size = 8),
                 text = vapply(names(r), nm, ""), hovertemplate = paste0("%{text}<br>", T("Rango medio"), " %{x:.2f}<extra></extra>"), showlegend = FALSE)
  grupos <- list(); vals <- unname(r)
  for (i in seq_len(k)) {
    j <- i
    while (j + 1 <= k && vals[j + 1] - vals[i] <= cd) j <- j + 1
    if (j > i && !any(vapply(grupos, function(g) g[1] <= i && j <= g[2], TRUE))) grupos[[length(grupos) + 1]] <- c(i, j)
  }
  for (n in seq_along(grupos)) {
    y <- .35 + .22 * (n - 1); g <- grupos[[n]]
    p <- add_trace(p, type = "scatter", mode = "lines", x = c(vals[g[1]] - .05, vals[g[2]] + .05), y = c(y, y), line = list(color = BASE, width = 4), hoverinfo = "skip", showlegend = FALSE)
  }
  top <- .35 + .22 * length(grupos) + .4
  p <- add_trace(p, type = "scatter", mode = "lines+markers", x = c(1, 1 + cd), y = c(top, top), line = list(color = HOT, width = 3),
                 marker = list(symbol = "line-ns-open", size = 12, color = HOT), hoverinfo = "skip", showlegend = FALSE)
  ann[[length(ann) + 1]] <- list(x = 1 + cd / 2, y = top + .3, text = fmt(T("CD = {cd}"), cd = sprintf("%.2f", cd)), showarrow = FALSE)
  layout(p, annotations = ann, xaxis = list(range = c(min(r) - 6, max(r) + 6), showgrid = FALSE, zeroline = FALSE, tickvals = seq_len(k), side = "top", title = T("Rango medio (menor es mejor)")),
         yaxis = list(visible = FALSE, range = c(-(max(length(izq), length(der)) + 1), top + .8)))
}

# Cascada SHAP: de la probabilidad media del fondo a la del cliente.
shap_waterfall <- function(valores, base, etiquetas, top = 10) {
  v <- valores[order(-abs(valores))]
  cabeza <- utils::head(v, top); extra <- length(v) > top; resto <- sum(v[-seq_len(top)])
  y <- c("E[f(X)]", vapply(names(cabeza), etiquetas, ""), if (extra) fmt(T("{n} variables más"), n = length(v) - top), "f(x)")
  x <- c(base, unname(cabeza), if (extra) resto, 0)
  plot_ly() %>% add_trace(type = "waterfall", orientation = "h", y = y, x = x, measure = c("absolute", rep("relative", length(x) - 2), "total"),
                          increasing = list(marker = list(color = HOT)), decreasing = list(marker = list(color = BASE)), totals = list(marker = list(color = GREY)),
                          connector = list(line = list(color = GREY, width = 1)), hovertemplate = "%{y}: %{x:+.3f}<extra></extra>") %>%
    layout(yaxis = cat_axis(y, autorange = "reversed"), xaxis = list(title = T("Probabilidad de abandono")))
}

# Resumen SHAP global: un punto por cliente y variable; color según el valor relativo de la variable.
shap_beeswarm <- function(s, etiquetas) {
  imp <- tapply(abs(s$shap), s$variable, mean); orden <- names(sort(imp))
  set.seed(0); jitter <- stats::rnorm(nrow(s), 0, .12)
  plot_ly() %>% add_trace(type = "scattergl", mode = "markers", x = s$shap, y = match(s$variable, orden) - 1 + jitter,
                          marker = list(size = 4, color = s$color, colorscale = list(list(0, BASE), list(1, HOT)), opacity = .75,
                                        colorbar = list(title = list(text = T("Valor")), tickvals = c(0, 1), ticktext = T(c("bajo", "alto")), thickness = 10)),
                          hovertemplate = "SHAP %{x:+.3f}<extra></extra>") %>%
    layout(shapes = list(vline(0)), yaxis = list(tickvals = seq_along(orden) - 1, ticktext = vapply(orden, etiquetas, "")),
           xaxis = list(title = T("Valor SHAP (cambio en la probabilidad de abandono)")))
}

# Puntos con intervalo (bigotes asimétricos).
bars_ci <- function(nombres, media, bajo, alto, resaltar, titulo_x) {
  plot_ly() %>% add_trace(type = "scatter", mode = "markers", x = media, y = nombres, marker = list(size = 10, color = ifelse(nombres == resaltar, HOT, BASE)),
                          error_x = list(type = "data", symmetric = FALSE, array = alto - media, arrayminus = media - bajo, color = GREY),
                          hovertemplate = "%{y}: %{x:.4f}<extra></extra>") %>%
    layout(xaxis = list(title = titulo_x), yaxis = cat_axis(nombres, autorange = "reversed"))
}
