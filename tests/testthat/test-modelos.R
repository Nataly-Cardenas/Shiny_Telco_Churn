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

test_that("the numpy random generator is reproduced exactly (np.random.seed(0))", {
  np <- numpy_rng(0)
  expect_equal(np$choice(c(.4, .2, .2, .1, .1), 12) - 1, c(1, 2, 2, 1, 1, 2, 1, 3, 4, 0, 2, 1))
  expect_equal(np$permutation(19) - 1, c(14, 7, 16, 6, 1, 15, 10, 2, 11, 18, 12, 0, 3, 4, 9, 8, 13, 5, 17))
})

test_that("local SHAP of RegionalMAPEM matches the deployed dash for the default customer", {
  # Valores del dash publicado (shap 0.48, KernelExplainer, 100 coaliciones, 10 variables) para 0365-GXEZS.
  dash <- c(InternetService = 0.12773195641910598, TechSupport = -0.048716489572561555, Contract = 0.04265751183370816,
            StreamingMovies = -0.0341540730024734, PaperlessBilling = -0.032858086666555576, PaymentMethod = 0.026824437818272973,
            MultipleLines = -0.02076634197520438, OnlineSecurity = 0.013707767605826295, PhoneService = -0.0012137586211441106,
            DeviceProtection = -0.0007719226605763459)
  s <- shap_local("RegionalMAPEM", validar(cliente_valores(EJEMPLO())))
  expect_equal(s$base, 0.2771058985673643, tolerance = 1e-7)
  expect_equal(unname(s$valores[names(dash)]), unname(dash), tolerance = 1e-6)
  expect_equal(sum(s$valores != 0), 10)
})
