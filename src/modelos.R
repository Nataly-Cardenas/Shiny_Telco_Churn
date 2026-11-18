# Carga de artefactos, validación de entradas, predicción y explicación SHAP local, en R puro.
#
# Los 12 modelos finales (11 benchmarks calibrados con Platt y RegionalMAPEM) se guardan en artifacts/<modelo>/modelo.rds
# con sus parámetros ajustados: coeficientes, árboles, vectores de soporte, calibradores... (ver scripts/convertir_artefactos.R).
# Aquí se reimplementa la inferencia de cada uno tal como la hacen scikit-learn, XGBoost, LightGBM y CatBoost, de modo que
# las probabilidades coinciden con las del experimento (tests/testthat/test-modelos.R). El dashboard nunca entrena.

ART <- Sys.getenv("ARTIFACTS_DIR", "artifacts")
ORIGINAL <- "RegionalMAPEM"
BENCHMARKS <- c("knn", "naive_bayes", "logistic", "decision_tree", "random_forest", "xgboost", "svm", "logistic_splines", "hist_gb", "lightgbm", "catboost")
MODELOS <- c(ORIGINAL, BENCHMARKS)
ENTRADA <- c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity",
             "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
             "MonthlyCharges", "TotalCharges")
SERVICE <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")
NOMBRES <- c(RegionalMAPEM = "RegionalMAPEM (modelo propio)", knn = "KNN", naive_bayes = "Naive Bayes", logistic = "Regresión logística",
             decision_tree = "Árbol de decisión", random_forest = "Random Forest", xgboost = "XGBoost", svm = "SVM RBF",
             logistic_splines = "Logística con splines", hist_gb = "HistGradientBoosting", lightgbm = "LightGBM", catboost = "CatBoost")
SHAP_FONDO <- 12; SHAP_COALICIONES <- 100   # explicación en vivo: 12 clientes de referencia y 100 coaliciones
EPS <- 1e-6

.modelos <- new.env()
metadata <- function(m) {
  key <- paste0("meta_", m)
  if (is.null(.modelos[[key]])) .modelos[[key]] <- jsonlite::fromJSON(file.path(ART, m, "metadata.json"), simplifyVector = FALSE)
  .modelos[[key]]
}
disponibles <- function() MODELOS[file.exists(file.path(ART, MODELOS, "modelo.rds"))]
artefacto <- function(m) {
  if (is.null(.modelos[[m]])) .modelos[[m]] <- readRDS(file.path(ART, m, "modelo.rds"))
  .modelos[[m]]
}

# ---------------------------------------------------------------- utilidades numéricas
sigmoid <- function(x) 1 / (1 + exp(-x))
# scikit-learn y XGBoost comparan en precisión simple (float32): se redondea igual para reproducir cada división.
f32 <- function(x) readBin(writeBin(as.numeric(x), raw(), size = 4), "double", size = 4, n = length(x))

# Recorre un árbol plano (left/right base 1, left = 0 en las hojas) para todas las filas de X a la vez.
tree_apply <- function(t, X, go_left) {
  node <- rep.int(1L, nrow(X)); rows <- seq_len(nrow(X))
  repeat {
    inner <- t$left[node] > 0L
    if (!any(inner)) break
    i <- which(inner); n <- node[i]
    left <- go_left(X[cbind(rows[i], t$feature[n])], t$threshold[n])
    node[i] <- ifelse(left, t$left[n], t$right[n])
  }
  t$value[node]
}
le <- function(x, thr) x <= thr
lt <- function(x, thr) x < thr

# ---------------------------------------------------------------- preprocesamiento (HistoricalFeatures + ColumnTransformer)
historical <- function(X) {
  total <- suppressWarnings(as.numeric(X$TotalCharges))
  if (any(is.na(total) & X$tenure != 0)) stop("TotalCharges ausente con tenure != 0")
  X$TotalCharges <- ifelse(is.na(total), 0, total)
  for (c in SERVICE) X[[c]][X[[c]] == "No internet service"] <- "No"
  X$MultipleLines[X$MultipleLines == "No phone service"] <- "No"
  X$AvgChargePerMonth <- ifelse(X$tenure > 0, X$TotalCharges / pmax(X$tenure, 1), X$MonthlyCharges)
  X
}

transformar <- function(p, X) {
  num <- sweep(sweep(as.matrix(X[p$num]), 2, p$mean), 2, p$scale, "/")
  cats <- lapply(seq_along(p$cat), function(j) {
    lev <- p$categories[[j]][-p$drop_idx[j]]
    vapply(lev, function(l) as.numeric(X[[p$cat[j]]] == l), numeric(nrow(X)))
  })
  out <- do.call(cbind, c(list(num), lapply(cats, function(m) matrix(m, nrow(X)))))
  unname(out)
}

# ---------------------------------------------------------------- respuesta de cada modelo (antes de la calibración de Platt)
bspline_basis <- function(x, t, k) {
  n <- length(t) - k - 1
  B <- vapply(seq_len(length(t) - 1), function(i) as.numeric(x >= t[i] & x < t[i + 1]), numeric(length(x)))
  B <- matrix(B, length(x))
  last <- max(which(t[-1] > t[-length(t)]))                  # el extremo derecho pertenece al último intervalo
  B[x == t[last + 1], last] <- 1
  if (k > 0) for (d in seq_len(k)) {
    Bn <- matrix(0, length(x), length(t) - d - 1)
    for (i in seq_len(ncol(Bn))) {
      a <- if (t[i + d] > t[i]) (x - t[i]) / (t[i + d] - t[i]) * B[, i] else 0
      b <- if (t[i + d + 1] > t[i + 1]) (t[i + d + 1] - x) / (t[i + d + 1] - t[i + 1]) * B[, i + 1] else 0
      Bn[, i] <- a + b
    }
    B <- Bn
  }
  B[, seq_len(n), drop = FALSE]
}

splines <- function(s, x) {   # SplineTransformer con extrapolación constante
  x <- pmin(pmax(x, s$xmin), s$xmax)
  bspline_basis(x, s$t, s$k)
}

respuesta <- function(nombre, m, Z) {
  switch(nombre,
    logistic = drop(Z %*% m$coef) + m$intercept,
    knn = {   # distancia de Manhattan (p = 1) a los clientes del ajuste y voto uniforme de los k más cercanos
      b <- which(apply(m$X, 2, function(v) all(v %in% c(0, 1))))   # indicadores one-hot: |z − x| = z + x − 2zx, con un producto de matrices
      D <- outer(rowSums(Z[, b, drop = FALSE]), rowSums(m$X[, b, drop = FALSE]), "+") - 2 * Z[, b, drop = FALSE] %*% t(m$X[, b, drop = FALSE])
      for (j in setdiff(seq_len(ncol(Z)), b)) D <- D + abs(outer(Z[, j], m$X[, j], "-"))
      D <- t(D); k <- m$k
      vapply(seq_len(nrow(Z)), function(i) {   # empates en el k-ésimo vecino: se toman en el orden del ajuste, como un ordenamiento estable
        d <- D[, i]; kth <- sort.int(d, partial = k)[k]; sel <- d < kth
        (sum(m$y[sel]) + sum(utils::head(m$y[d == kth], k - sum(sel)))) / k
      }, 0)
    },
    naive_bayes = {
      jll <- sapply(1:2, function(c) log(m$prior[c]) - 0.5 * sum(log(2 * pi * m$var[c, ])) - 0.5 * colSums((t(Z) - m$theta[c, ])^2 / m$var[c, ]))
      jll <- matrix(jll, nrow(Z))
      1 / (1 + exp(jll[, 1] - jll[, 2]))
    },
    decision_tree = tree_apply(m, f32(Z) |> matrix(nrow(Z)), le),
    random_forest = { Z32 <- matrix(f32(Z), nrow(Z)); Reduce(`+`, lapply(m$trees, tree_apply, Z32, le)) / length(m$trees) },
    svm = {
      K <- exp(-m$gamma * pmax(outer(rowSums(Z^2), rowSums(m$sv^2), "+") - 2 * Z %*% t(m$sv), 0))
      -(drop(K %*% m$dual) + m$intercept)
    },
    logistic_splines = {
      B <- do.call(cbind, c(lapply(seq_along(m$order), function(i) splines(m$splines[[i]], Z[, m$order[i]])), list(Z[, m$pasan, drop = FALSE])))
      drop(B %*% m$coef) + m$intercept
    },
    hist_gb = m$baseline + Reduce(`+`, lapply(m$trees, tree_apply, Z, le)),
    xgboost = {
      Z32 <- matrix(f32(Z), nrow(Z))
      sigmoid(log(m$base_score / (1 - m$base_score)) + Reduce(`+`, lapply(m$trees, function(t) tree_apply(modifyList(t, list(threshold = f32(t$threshold))), Z32, lt))))
    },
    lightgbm = Reduce(`+`, lapply(m$trees, tree_apply, Z, le)),   # margen (sin sigmoide): así lo recibe el calibrador de Platt
    catboost = {
      Z32 <- matrix(f32(Z), nrow(Z))
      raw <- Reduce(`+`, lapply(m$trees, function(t) {
        idx <- rep(0, nrow(Z))
        for (j in seq_along(t$feature)) idx <- idx + (Z32[, t$feature[j]] > f32(t$border[j])) * 2^(j - 1)
        t$leaf[idx + 1]
      }))
      sigmoid(m$scale * raw + m$bias)
    },
    stop(nombre))
}

# CalibratedClassifierCV(method = "sigmoid", cv = 3): media de los tres calibradores.
predecir_benchmark <- function(nombre, X) {
  a <- artefacto(nombre); H <- historical(X)
  Reduce(`+`, lapply(a$calibrados, function(c) {
    f <- respuesta(nombre, c$modelo, transformar(c$prep, H))
    1 / (1 + exp(c$a * f + c$b))
  })) / length(a$calibrados)
}

# ---------------------------------------------------------------- RegionalMAPEM
regiones <- function(X) {
  e <- artefacto(ORIGINAL)
  Z <- transformar(e$prep, historical(X))[, e$region_columns, drop = FALSE]
  U <- vapply(seq_along(e$scores), function(j) {
    s <- e$scores[[j]]; lo <- findInterval(Z[, j], s, left.open = TRUE); hi <- findInterval(Z[, j], s)
    stats::qnorm(pmin(pmax(((lo + hi) / 2 + 0.5) / (length(s) + 1), 1e-6), 1 - 1e-6))
  }, numeric(nrow(Z)))
  U <- matrix(U, nrow(Z))
  P <- sweep(U, 2, e$pca_mean) %*% t(e$pca_components)
  P <- sweep(sweep(P, 2, e$z_mean), 2, e$z_std, "/")
  logp <- vapply(seq_len(e$K), function(k) {
    L <- t(chol(e$gmm_covs[[k]])); sol <- forwardsolve(L, t(sweep(P, 2, e$gmm_means[k, ])))
    log(e$gmm_w[k]) - 0.5 * (colSums(matrix(sol^2, ncol(P))) + ncol(P) * log(2 * pi)) - sum(log(diag(L)))
  }, numeric(nrow(P)))
  logp <- matrix(logp, nrow(P)); mx <- apply(logp, 1, max)
  R <- exp(logp - mx); R <- R / rowSums(R)
  if (e$hard) R <- diag(e$K)[max.col(R), , drop = FALSE]
  R
}

probabilidades_expertos <- function(X) {
  e <- artefacto(ORIGINAL)
  matrix(vapply(e$expertos, predecir_benchmark, numeric(nrow(X)), X), nrow(X), dimnames = list(NULL, e$expertos))
}

predecir_original <- function(X, P = NULL) {
  e <- artefacto(ORIGINAL)
  P <- pmin(pmax(P %||% probabilidades_expertos(X), EPS), 1 - EPS)
  rowSums((regiones(X) %*% e$weights) * P)
}

predecir <- function(modelo, X) if (modelo == ORIGINAL) predecir_original(X) else predecir_benchmark(modelo, X)

# ---------------------------------------------------------------- validación del formulario
esquema <- function() { e <- metadata(ORIGINAL)$esquema_entrada; setNames(e, vapply(e, `[[`, "", "nombre")) }

validar <- function(valores) {
  esq <- esquema(); fila <- list(); errores <- character()
  for (c in ENTRADA) {
    e <- esq[[c]]; v <- valores[[c]]
    if (is.null(v) || length(v) == 0 || (is.character(v) && !nzchar(trimws(v))) || (length(v) == 1 && is.na(v))) { errores <- c(errores, paste0("Falta ", c, ".")); next }
    if (e$tipo == "numérica") {
      v <- suppressWarnings(as.numeric(v))
      if (is.na(v)) { errores <- c(errores, paste0(c, " debe ser un número.")); next }
      if (!is.finite(v) || v < e$min || v > e$max) { errores <- c(errores, sprintf("%s debe estar entre %s y %s.", c, format(e$min), format(e$max))); next }
      fila[[c]] <- if (c %in% c("tenure", "SeniorCitizen")) as.integer(v) else v
    } else {
      if (!(v %in% unlist(e$categorias))) { errores <- c(errores, sprintf("%s no admite el valor '%s'.", c, v)); next }
      fila[[c]] <- v
    }
  }
  if (!length(errores)) {
    if (fila$tenure == 0 && fila$TotalCharges > fila$MonthlyCharges) errores <- c(errores, "Con tenure = 0 el cargo acumulado no puede superar un mes de cargo.")
    if (fila$tenure > 0 && fila$TotalCharges < 1) errores <- c(errores, "Con tenure > 0 el cargo acumulado debe ser positivo.")
    if (fila$PhoneService == "No" && fila$MultipleLines != "No phone service") errores <- c(errores, "Sin servicio telefónico, MultipleLines debe ser 'No phone service'.")
    if (fila$PhoneService == "Yes" && fila$MultipleLines == "No phone service") errores <- c(errores, "Con servicio telefónico, MultipleLines no puede ser 'No phone service'.")
    sin_internet <- fila$InternetService == "No"
    for (c in SERVICE) if (sin_internet != (fila[[c]] == "No internet service")) {
      errores <- c(errores, paste0(c, " debe ser 'No internet service' si y solo si InternetService = 'No'.")); break
    }
  }
  if (length(errores)) stop(structure(class = c("entrada_invalida", "error", "condition"), list(message = paste(errores, collapse = " "), call = NULL)))
  as.data.frame(fila[ENTRADA], stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- SHAP local (Kernel SHAP con enlace identidad)
# Clientes de referencia: los mismos 12 del desarrollo que usa el dash (numpy default_rng(20261110), data/shap_fondo.csv).
fondo_shap <- function() {
  if (is.null(.modelos$fondo)) {
    ids <- utils::read.csv(file.path(DATA, "shap_fondo.csv"))$row_id
    X <- RAW[ids + 1L, ENTRADA]; X$TotalCharges[is.na(X$TotalCharges)] <- 0
    rownames(X) <- NULL
    .modelos$fondo <- X
  }
  .modelos$fondo
}

# Igual que shap.KernelExplainer: enumera completos los tamaños de coalición que caben en el presupuesto (con su
# complemento) y muestrea el resto con el peso del kernel de Shapley; luego resuelve la regresión ponderada con la
# restricción de eficiencia (los valores suman f(x) − E[f(fondo)]).
shap_local <- function(modelo, X, fondo = fondo_shap(), coaliciones = SHAP_COALICIONES, semilla = 0) {
  X <- X[ENTRADA]; X$TotalCharges <- suppressWarnings(as.numeric(X$TotalCharges)); X$TotalCharges[is.na(X$TotalCharges)] <- 0
  M <- length(ENTRADA); set.seed(semilla)
  fx <- predecir(modelo, X); base <- mean(predecir(modelo, fondo))
  varying <- which(vapply(ENTRADA, function(c) any(fondo[[c]] != X[[c]]), TRUE))
  phi <- setNames(numeric(M), ENTRADA)
  if (!length(varying)) return(list(valores = phi, base = base))
  m <- length(varying)
  if (m == 1) { phi[varying] <- fx - base; return(list(valores = phi, base = base)) }
  masks <- list(); weights <- numeric()
  add <- function(mask, w) { masks[[length(masks) + 1]] <<- mask; weights[length(weights) + 1] <<- w }
  nsub <- ceiling((m - 1) / 2); npaired <- floor((m - 1) / 2)
  wvec <- vapply(seq_len(nsub), function(s) (m - 1) / (s * (m - s)), 0); wvec[seq_len(npaired)] <- 2 * wvec[seq_len(npaired)]; wvec <- wvec / sum(wvec)
  left <- coaliciones; full <- 0; rem <- wvec
  for (s in seq_len(nsub)) {
    nmask <- choose(m, s) * (if (s <= npaired) 2 else 1)
    if (left * rem[s] / nmask < 1 - 1e-8) break
    full <- full + 1; left <- left - nmask
    if (rem[s] < 1) rem <- rem / (1 - rem[s])
    w <- wvec[s] / choose(m, s) / (if (s <= npaired) 2 else 1)
    for (cmb in utils::combn(m, s, simplify = FALSE)) {
      mk <- rep(FALSE, m); mk[cmb] <- TRUE; add(mk, w)
      if (s <= npaired) add(!mk, w)
    }
  }
  nfixed <- length(weights); left <- coaliciones - nfixed
  if (full < nsub && left > 0) {   # muestreo del resto de tamaños con el peso del kernel
    rw <- wvec; rw[seq_len(npaired)] <- rw[seq_len(npaired)] / 2; rw <- rw[(full + 1):nsub]; rw <- rw / sum(rw)
    sizes <- sample.int(length(rw), 4 * left, replace = TRUE, prob = rw) + full
    used <- new.env(); pos <- 0
    while (left > 0 && pos < length(sizes)) {
      pos <- pos + 1; s <- sizes[pos]
      mk <- rep(FALSE, m); mk[sample.int(m)[seq_len(s)]] <- TRUE
      key <- paste(which(mk), collapse = ","); new <- is.null(used[[key]])
      if (new) { used[[key]] <- length(weights) + 1; left <- left - 1; add(mk, 1) } else weights[used[[key]]] <- weights[used[[key]]] + 1
      if (left > 0 && s <= npaired) {
        if (new) { left <- left - 1; add(!mk, 1) } else weights[used[[key]] + 1] <- weights[used[[key]] + 1] + 1
      }
    }
    idx <- seq_along(weights) > nfixed
    weights[idx] <- weights[idx] * sum(wvec[(full + 1):nsub]) / sum(weights[idx])
  }
  # evalúa cada coalición: las variables de la coalición toman el valor del cliente; el resto, el de cada referencia
  n <- nrow(fondo); S <- do.call(rbind, masks)
  big <- fondo[rep(seq_len(n), nrow(S)), , drop = FALSE]
  for (j in seq_len(m)) { on <- rep(S[, j], each = n); big[[ENTRADA[varying[j]]]][on] <- X[[ENTRADA[varying[j]]]] }
  ey <- colMeans(matrix(predecir(modelo, big), n))
  # regresión ponderada con restricción: se elimina la última variable
  y <- ey - base; Sm <- S * 1; last <- Sm[, m]
  A <- Sm[, -m, drop = FALSE] - last; b <- y - last * (fx - base)
  W <- weights
  coef <- tryCatch(solve(crossprod(A, A * W), crossprod(A, b * W)), error = function(e) MASS_ginv(crossprod(A, A * W)) %*% crossprod(A, b * W))
  v <- c(drop(coef), (fx - base) - sum(coef))
  phi[varying] <- v
  list(valores = phi, base = base)
}
MASS_ginv <- function(A) { s <- svd(A); d <- ifelse(s$d > 1e-10 * max(s$d), 1 / s$d, 0); s$v %*% (d * t(s$u)) }
