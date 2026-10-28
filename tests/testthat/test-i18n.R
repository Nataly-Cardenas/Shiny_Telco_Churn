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
  html <- render(PAGES$inicio(rows(TRAIN_IDS), 0))
  expect_match(html, "Lost", fixed = TRUE)
  expect_false(grepl("Entrar al dashboard", html, fixed = TRUE))
  # Textos largos armados con paste0 (no los detecta la búsqueda de literales).
  intro <- render(introduccion(rows(TRAIN_IDS))); sim <- render(sim_tab(0, list()))
  for (es in c("El conjunto Telco Customer Churn", "Identificar a los clientes de una empresa")) expect_false(grepl(es, intro, fixed = TRUE))
  expect_false(grepl("Es la tasa de abandono observada", sim, fixed = TRUE))
  set_lang("es")
})
