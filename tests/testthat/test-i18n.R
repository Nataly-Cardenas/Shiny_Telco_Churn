test_that("every interface text has an English translation", {
  src <- unlist(lapply(list.files(file.path(root, "src"), full.names = TRUE), readLines, encoding = "UTF-8"))
  lit <- regmatches(src, gregexpr('\\b[TN]\\("((?:[^"\\\\]|\\\\.)*)"\\)', src, perl = TRUE))
  strings <- unique(gsub('^[TN]\\("|"\\)$', "", unlist(lit)))
  strings <- gsub('\\\\"', '"', strings)
  vec <- unlist(regmatches(src, gregexpr('\\bT\\(c\\(([^()]*)\\)\\)', src, perl = TRUE)))
  strings <- c(strings, gsub('"', "", unlist(regmatches(vec, gregexpr('"[^"]*"', vec)))))
  # Identificadores y nombres propios que se escriben igual en los dos idiomas.
  same <- c("Recall", "Precision", "ROC-AUC", "Accuracy", "Jupyter Book", "Test")
  missing <- setdiff(strings[!grepl("^\\s*$", strings)], c(names(EN), same))
  expect_equal(missing, character())
})

test_that("the cover, charts and menus switch language", {
  set_lang("en")
  html <- render(PAGES$contexto(0))
  expect_match(html, "Lost", fixed = TRUE)
  expect_false(grepl("Entrar al dashboard", html, fixed = TRUE))
  # Textos largos armados con paste0 (no los detecta la búsqueda de literales).
  for (es in c("Cada cancelación es un ingreso", "Identificar a los clientes de una empresa", "Combinar 11 clasificadores")) expect_false(grepl(es, html, fixed = TRUE))
  lid <- render(modelos_tab(0, list()))
  expect_false(grepl("Cada benchmark se optimizó", lid, fixed = TRUE))
  set_lang("es")
})
