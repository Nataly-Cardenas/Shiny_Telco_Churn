render <- function(x) htmltools::renderTags(x)$html

test_that("all pages, tabs and interactive outputs render in both languages", {
  for (lang in c("es", "en")) {
    set_lang(lang)
    filters <- list(list("train", "Todos", "Todos", c(0, 72)), list("train", "Two year", "Fiber optic", c(0, 0)), list("test", "Month-to-month", "DSL", c(0, 12)))
    for (f in filters) {
      d <- do.call(cohort, f)
      for (i in 0:8) expect_type(render(eda_tab(i, d, fake_input)), "character")
      expect_type(render(eda_kpis(d)), "character")
      for (v in NUMERIC) { expect_type(render(uni_num(v, d)), "character"); expect_type(render(bi_num(v, d)), "character") }
      for (v in c("__all__", CATEGORICAL)) expect_type(render(uni_cat(v, d)), "character")
      for (v in CATEGORICAL) expect_type(render(bi_cat(v, d)), "character")
      for (x in c(EXPLORABLES, "Contract")) for (cl in c("Churn", "PaymentMethod")) expect_type(render(explorador_xy(x, "MonthlyCharges", cl, d)), "character")
    }
    for (key in names(PAGES)) expect_type(render(PAGES[[key]](0)), "character")
    for (i in 0:7) expect_type(render(modelos_tab(i, fake_input)), "character")
    for (i in 0:8) expect_type(render(model_tab(i, fake_input)), "character")
    expect_type(render(tabla_lideres(TRUE)), "character")
    for (m in MODELOS) {
      expect_type(render(shap_salida(m)), "character")
      for (e in c("validacion_r0", "holdout")) expect_type(render(diagnostico_salida(m, e)), "character")
    }
    for (m in names(MODELS)) {
      expect_type(render(model_output(m, "Todos", "f1_positive")), "character")
      expect_type(render(opt_output(m)), "character"); expect_type(render(res_output(m)), "character")
    }
    for (m in FINALISTS[1:2]) expect_type(render(explain_output(m)), "character")
    for (m in names(BEST_BY_FAMILY)) expect_type(render(cal_map(m)), "character")
    for (m in VARIANT_FAMILIES) expect_type(render(eff_variants(m)), "character")
    for (m in unique(comp("escalamiento")$model)) expect_type(render(eff_scale(m)), "character")
    for (m in FINALISTS) expect_type(render(threshold_output(m, .3)), "character")
    expect_type(render(app_ui(NULL)), "character")
  }
  set_lang("es")
})

test_that("live prediction renders for every model, and invalid input shows a warning", {
  val <- cliente_valores(EJEMPLO())
  for (m in MODELOS) {
    h <- render(prediccion_salida(m, val))
    expect_match(h, "SHAP")
    expect_false(grepl("alert-dash", h))
  }
  bad <- val; bad$tenure <- 500L
  expect_match(render(prediccion_salida("logistic", bad)), "alert-dash")
  bad <- val; bad$PhoneService <- "No"; bad$MultipleLines <- "Yes"
  expect_match(render(prediccion_salida("logistic", bad)), "MultipleLines")
})
