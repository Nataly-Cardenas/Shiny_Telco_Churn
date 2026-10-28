# Traducción español → inglés. Los textos se escriben en español en el código y T() los traduce
# según el idioma activo, igual que en el dashboard de Dash (las traducciones están en data/i18n_en.json).

EN <- jsonlite::fromJSON(file.path(DATA, "i18n_en.json"))
.i18n <- new.env()
.i18n$lang <- "es"

set_lang <- function(lang) .i18n$lang <- if (identical(lang, "en")) "en" else "es"
get_lang <- function() .i18n$lang

# N() marca constantes que se traducen más tarde; T() traduce ahora.
N <- function(s) s
T <- function(s) {
  if (.i18n$lang != "en") return(s)
  vapply(s, function(x) { v <- EN[[x]]; if (is.null(v)) x else v }, character(1), USE.NAMES = FALSE)
}

# Equivalente de str.format de Python para las plantillas con {nombre}.
fmt <- function(.s, ...) {
  args <- list(...)
  for (k in names(args)) .s <- gsub(paste0("{", k, "}"), as.character(args[[k]]), .s, fixed = TRUE)
  .s
}
