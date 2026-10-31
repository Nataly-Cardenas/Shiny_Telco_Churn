test_that("original split and counts are preserved", {
  expect_equal(nrow(RAW), 7043)
  expect_equal(c(length(TRAIN_IDS), length(TEST_IDS)), c(5634, 1409))
  expect_length(intersect(TRAIN_IDS, TEST_IDS), 0)
  expect_setequal(c(TRAIN_IDS, TEST_IDS), RAW$row_id)
  expect_equal(sum(RAW$target), 1869)
  expect_equal(sum(is.na(RAW$TotalCharges)), 11)
  expect_true(all(RAW$tenure[is.na(RAW$TotalCharges)] == 0))
})

test_that("published results and test predictions are intact", {
  expect_equal(nrow(SUMMARY), 112); expect_equal(nrow(UNITS), 1680)
  expect_true(all(table(paste(UNITS$experiment_id, UNITS$replicate)) == 5))
  expect_equal(SUMMARY$f1_positive_mean[SUMMARY$experiment_id == FINALISTS[1]], .6384205660936855)
  for (e in FINALISTS) {
    p <- predictions(e)
    expect_equal(nrow(p), 1409); expect_false(any(duplicated(p$row_id)))
    expect_setequal(p$row_id, TEST_IDS)
    expect_equal(p$y_true, rows(p$row_id)$target)
    expect_true(all(p$probability >= 0 & p$probability <= 1))
  }
})

test_that("threshold recomputes errors and alerts", {
  p <- predictions(FINALISTS[1]); t <- threshold_metrics(p, .5)
  expect_equal(sum(t$cm), 1409); expect_equal(sum(t$cm[2, ]), 374)
  expect_equal(t$m$Alertas, sum(t$cm[, 2]))
  expect_equal(round(t$m$F1, 4), .6271)
  expect_gte(threshold_metrics(p, .2)$m$Alertas, threshold_metrics(p, .8)$m$Alertas)
  expect_gte(threshold_metrics(p, .2)$m$Recall, threshold_metrics(p, .8)$m$Recall)
  expect_equal(round(auc(p$y_true, p$score), 3), .844)
})

test_that("filters, rates and empty cohorts", {
  d <- cohort("train", "Month-to-month", "Fiber optic", c(0, 12))
  expect_gt(nrow(d), 0); expect_true(all(d$tenure <= 12))
  expect_true(all(d$Contract == "Month-to-month" & d$InternetService == "Fiber optic"))
  g <- category_rates(d, "PaymentMethod")
  expect_equal(sum(g$n), nrow(d)); expect_equal(sum(g$abandonos), sum(d$target))
  expect_equal(nrow(cohort("train", "Two year", "Fiber optic", c(0, 0))), 0)
})

test_that("EDA statistics match the book", {
  d <- rows(TRAIN_IDS); h <- holm_family(d)
  expect_equal(nrow(h$fam), 19)
  expect_equal(round(h$num$`r punto-biserial`[h$num$Variable == "tenure"], 4), -0.3456)
  v <- setNames(h$cat$`V de Cramér`, h$cat$Variable)
  expect_equal(names(which.max(v)), "Contract"); expect_equal(round(max(v), 4), 0.4111)
  expect_false(any(h$fam$`Rechaza H0`[h$fam$Variable %in% c("gender", "PhoneService")]))
  expect_true(all(normality(d)$`¿Normal?` == "No"))
  expect_equal(round(stats::cor(d$tenure, d$TotalCharges, method = "spearman", use = "complete.obs"), 2), 0.89)
})

test_that("modeling artifacts match the book", {
  o <- oof(); rf <- o[o$experiment == BEST_BY_FAMILY[["random_forest"]], ]
  expect_equal(confusion(rf$y_true, rf$y_pred), matrix(c(3289, 396, 850, 1099), 2))
  b <- baseline(); expect_equal(confusion(b$y_true, as.integer(b$probability >= .5)), matrix(c(749, 81, 286, 293), 2))
  expect_equal(round(auc(b$y_true, b$probability), 4), .8419)
})

test_that("charts use one color plus the highlight", {
  allowed <- toupper(c(BASE, HOT, GREY, "#0b1426", "#eaf2ff", "#8792a8", "#ffffff"))
  set_lang("es"); d <- rows(TRAIN_IDS)
  html <- paste(render(eda_tab(3, d, list())), render(prep_tab(3)), render(model_output("random_forest", "Todos", "f1_positive")), render(threshold_output(FINALISTS[1], .5)))
  used <- unique(toupper(regmatches(html, gregexpr("#[0-9A-Fa-f]{6}\\b", html))[[1]]))
  expect_equal(setdiff(used, allowed), character())
})

test_that("assistant answers from its own knowledge base and computes profiles", {
  expect_false(exists("chunks")); expect_equal(length(FAQ), 47)
  a <- answer("¿Qué es el diagrama de Nemenyi?", list(), "es"); expect_match(a, "Nemenyi")
  a <- answer("tasa de abandono con fibra y mes a mes", list(), "es"); expect_match(a, "Contrato: Mes a mes", fixed = TRUE)
  expect_match(answer("precio del dolar hoy", list(), "es"), "No encontré")
  expect_match(answer("what is smote", list(), "en"), "SMOTE")
})


test_that("new Jupyter Book results (entregable 3) match the book", {
  u <- UNITS[UNITS$replicate == 0, ]
  base <- stats::aggregate(u[c("brier", "ece", "cal_brier", "cal_ece")], list(e = u$experiment_id), mean)
  iu <- comp("isotonica_unidades"); iso <- stats::aggregate(iu[c("iso_brier", "iso_ece")], list(e = iu$experiment_id), mean)
  m <- merge(base, iso, by = "e")
  expect_equal(sum(m$iso_brier < m$cal_brier), 67); expect_equal(sum(m$iso_ece < m$cal_ece), 74)
  n <- m[!is.na(m$brier), ]
  expect_equal(sum(n$iso_brier < n$brier & n$iso_ece < n$ece), 84); expect_equal(sum(n$cal_brier < n$brier & n$cal_ece < n$ece), 80)
  r <- comp("multifidelidad_resumen"); sp <- function(x) r$segundos[r$model == x & r$method == "random_completo"] / r$segundos[r$model == x & r$method == "successive_halving"]
  expect_equal(round(c(sp("random_forest"), sp("svm"), sp("xgboost")), 1), c(3.7, 7.1, 1.6))
  v <- comp("estandar_vs_optimizado"); expect_equal(round(v$aceleración_entrenamiento[v$variant == "LinearSVC"]), 36)
  expect_equal(round(comp("escalamiento_exponentes")$exponente_n[comp("escalamiento_exponentes")$model == "svm rbf"], 1), 2)
  expect_false(any(is.na(oof()$probability_isotonic)))
})

test_that("Antena introduces herself and the voice greeting is wired", {
  expect_match(answer("¿cómo te llamas?", list(), "es"), "Antena")
  expect_match(answer("qué es successive halving", list(), "es"), "Successive Halving")
  js <- paste(readLines(file.path(root, "www", "antena.js"), encoding = "UTF-8"), collapse = "\n")
  expect_match(js, "Hola, soy Antena, estoy aquí para resolver tus dudas", fixed = TRUE)
  html <- render(chat_ui()); expect_match(html, "antena-face", fixed = TRUE); expect_match(html, "chat-voice", fixed = TRUE)
})
