# Íconos de línea propios (24×24, trazo 2) que se pintan con el color del texto mediante una máscara CSS.

ICON_PATHS <- jsonlite::fromJSON(file.path(DATA, "icons.json"))

icon_url <- function(name) {
  svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='black' stroke-width='2' ",
                "stroke-linecap='round' stroke-linejoin='round'><path d='", ICON_PATHS[[name]], "'/></svg>")
  sprintf('url("data:image/svg+xml,%s")', utils::URLencode(svg, reserved = TRUE))
}

icon <- function(name, cls = "") {
  u <- icon_url(name)
  tags$span(class = paste("ic", cls), style = sprintf("mask-image:%s;-webkit-mask-image:%s", u, u), `aria-hidden` = "true")
}
