# Los 12 modelos evaluados en R reproducen las probabilidades del experimento (Python) y la validación del formulario.

test_that("R inference matches the stored predictions of the 12 models", {
  P <- utils::read.csv(file.path(ART, "prueba_ida_y_vuelta.csv"), stringsAsFactors = FALSE)
  for (m in MODELOS) expect_lt(max(abs(predecir(m, P[ENTRADA]) - P[[paste0("p_", m)]])), 1e-6)
  h <- predicciones_finales(); h <- h[h$evaluacion == "holdout", ]
  X <- rows(h$row_id)[ENTRADA]
  for (m in c("logistic", "random_forest", "xgboost", "lightgbm", "catboost", "hist_gb")) expect_lt(max(abs(predecir(m, X) - h[[m]])), 1e-5)
})

test_that("form validation rejects inconsistent customers", {
  ok <- cliente_valores(EJEMPLO())
  expect_s3_class(validar(ok), "data.frame")
  bad <- ok; bad$InternetService <- "No"
  expect_error(validar(bad), class = "entrada_invalida")
  bad <- ok; bad$Contract <- "Ten years"
  expect_error(validar(bad), "Contract")
})

test_that("SHAP values add up to the prediction minus the reference mean", {
  X <- validar(cliente_valores(EJEMPLO()))
  s <- shap_local("logistic", X)
  expect_equal(sum(s$valores) + s$base, predecir("logistic", X), tolerance = 1e-8)
})
