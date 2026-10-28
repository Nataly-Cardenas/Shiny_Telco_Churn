# Datos originales y cálculos descriptivos. No entrena ni deserializa modelos.
# Los identificadores de fila (row_id) son los del proyecto original: empiezan en 0.

read <- function(...) utils::read.csv(file.path(DATA, ...), check.names = FALSE, stringsAsFactors = FALSE)

RAW <- read("WA_Fn-UseC_-Telco-Customer-Churn.csv")
RAW$TotalCharges <- suppressWarnings(as.numeric(RAW$TotalCharges))
RAW$Estado <- ifelse(RAW$Churn == "Yes", "Abandona", "Permanece")
RAW$target <- as.integer(RAW$Churn == "Yes")
RAW$row_id <- seq_len(nrow(RAW)) - 1L
RAW_COLS <- names(RAW)[1:21]

rows <- function(ids) RAW[ids + 1L, , drop = FALSE]

TRAIN_IDS <- read("train_index.csv")[[1]]
TEST_IDS <- read("test_index.csv")[[1]]
SUMMARY <- read("resumen_112.csv")
UNITS <- read("unidades.csv")
PROVENANCE <- jsonlite::fromJSON(file.path(DATA, "provenance.json"))

MODELS <- c(knn = "KNN", naive_bayes = "Naive Bayes", logistic = "Regresión logística", decision_tree = "Árbol de decisión",
            random_forest = "Random Forest", xgboost = "XGBoost", svm = "SVM RBF")
BALANCES <- c(none = "Sin balanceo", balanced = "Pesos de clase", smote = "SMOTE", adasyn = "ADASYN")
OPTIMIZERS <- c(grid = "Grid", random = "Random", bayesian_tpe = "TPE", genetic = "Genético")
NUMERIC <- c("tenure", "MonthlyCharges", "TotalCharges")
CATEGORICAL <- setdiff(names(RAW), c(NUMERIC, "customerID", "Churn", "Estado", "target", "row_id"))
LABELS <- c(tenure = "Antigüedad (meses)", MonthlyCharges = "Cargo mensual", TotalCharges = "Cargo acumulado", Contract = "Contrato",
            InternetService = "Internet", PaymentMethod = "Método de pago", TechSupport = "Soporte técnico", OnlineSecurity = "Seguridad en línea",
            gender = "Género", SeniorCitizen = "Adulto mayor", Partner = "Pareja", Dependents = "Dependientes", PhoneService = "Servicio telefónico",
            MultipleLines = "Líneas múltiples", OnlineBackup = "Respaldo en línea", DeviceProtection = "Protección de dispositivos",
            StreamingTV = "Streaming TV", StreamingMovies = "Streaming películas", PaperlessBilling = "Factura electrónica",
            AvgChargePerMonth = "Cargo promedio mensual")
FINALISTS <- c("random_forest__smote__bayesian_tpe", "xgboost__smote__random", "logistic__smote__grid")
FINALIST_NAMES <- c(random_forest__smote__bayesian_tpe = "Random Forest", xgboost__smote__random = "XGBoost", logistic__smote__grid = "Regresión logística")
ES <- c(`Month-to-month` = "Mes a mes", `One year` = "Un año", `Two year` = "Dos años", `Fiber optic` = "Fibra óptica", DSL = "DSL", No = "No",
        Yes = "Sí", `Electronic check` = "Cheque electrónico", `Mailed check` = "Cheque por correo",
        `Bank transfer (automatic)` = "Transferencia automática", `Credit card (automatic)` = "Tarjeta de crédito automática",
        `No internet service` = "Sin internet", `No phone service` = "Sin teléfono", Male = "Hombre", Female = "Mujer")

cohort <- function(split = "train", contract = "Todos", internet = "Todos", tenure = c(0, 72)) {
  d <- if (split == "train") rows(TRAIN_IDS) else if (split == "test") rows(TEST_IDS) else RAW
  if (contract != "Todos") d <- d[d$Contract == contract, , drop = FALSE]
  if (internet != "Todos") d <- d[d$InternetService == internet, , drop = FALSE]
  d[d$tenure >= tenure[1] & d$tenure <= tenure[2], , drop = FALSE]
}

category_rates <- function(d, variable) {
  g <- split(d$target, d[[variable]])
  data.frame(value = names(g), n = vapply(g, length, 1L), abandonos = vapply(g, sum, 1L), tasa = vapply(g, mean, 1), row.names = NULL)
}

.pred_cache <- new.env()
predictions <- function(experiment) {
  if (!experiment %in% FINALISTS) stop("Configuración no autorizada")
  if (is.null(.pred_cache[[experiment]])) .pred_cache[[experiment]] <- read("predictions", experiment, "test_predictions.csv")
  .pred_cache[[experiment]]
}

# Matriz de confusión 2×2 con filas = realidad (0, 1) y columnas = predicción (0, 1).
confusion <- function(y, pred) matrix(c(sum(y == 0 & pred == 0), sum(y == 1 & pred == 0), sum(y == 0 & pred == 1), sum(y == 1 & pred == 1)), 2)

threshold_metrics <- function(p, threshold) {
  y <- p$y_true; pred <- as.integer(p$probability >= threshold)
  cm <- confusion(y, pred); tp <- cm[2, 2]; fp <- cm[1, 2]; fn <- cm[2, 1]
  prec <- if (tp + fp > 0) tp / (tp + fp) else 0
  rec <- if (tp + fn > 0) tp / (tp + fn) else 0
  f1 <- if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0
  list(cm = cm, m = list(Precision = prec, Recall = rec, F1 = f1, Alertas = as.integer(sum(pred))))
}

# Curva ROC como sklearn.metrics.roc_curve (umbrales distintos, sin puntos intermedios colineales).
roc_curve <- function(y, score) {
  o <- order(score, decreasing = TRUE); y <- y[o]; s <- score[o]
  idx <- c(which(diff(s) != 0), length(s))
  tps <- cumsum(y)[idx]; fps <- idx - tps
  if (length(tps) > 2) {
    keep <- c(TRUE, (diff(fps, differences = 2) != 0) | (diff(tps, differences = 2) != 0), TRUE)
    tps <- tps[keep]; fps <- fps[keep]
  }
  tps <- c(0, tps); fps <- c(0, fps)
  list(fpr = fps / max(fps), tpr = tps / max(tps))
}

auc <- function(y, score) {
  r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

test_scores <- function() {
  out <- data.frame(row_id = TEST_IDS)
  for (e in FINALISTS) { p <- predictions(e); out[[e]] <- p$probability[match(TEST_IDS, p$row_id)] }
  out
}

# Clientes de entrenamiento con un perfil parecido; amplía la ventana si hay pocos.
similar_customers <- function(contract, internet, payment, tenure, monthly) {
  d <- rows(TRAIN_IDS)
  base <- d[d$Contract == contract & d$InternetService == internet, , drop = FALSE]
  wins <- list(c(6, 15, 1), c(12, 25, 1), c(12, 25, 0), c(24, 40, 0), c(72, 200, 0))
  for (w in wins) {
    s <- base[abs(base$tenure - tenure) <= w[1] & abs(base$MonthlyCharges - monthly) <= w[2], , drop = FALSE]
    if (w[3] == 1) s <- s[s$PaymentMethod == payment, , drop = FALSE]
    if (nrow(s) >= 30 || w[1] == 72) return(list(s = s, w = list(tenure = w[1], monthly = w[2], payment = w[3] == 1)))
  }
}
