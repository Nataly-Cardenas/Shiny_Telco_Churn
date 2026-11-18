# Convierte los parámetros exportados de los 12 artefactos del dashboard de Dash (JSON) en artefactos de R (.rds).
#
# Se corre una sola vez, fuera de la app:   Rscript scripts/convertir_artefactos.R <carpeta_json> artifacts
# La carpeta JSON contiene un archivo por modelo con los parámetros ajustados (coeficientes, árboles, vectores de
# soporte, calibradores de Platt...) de los artefactos de Dash_Telco_Churn_ML. La app solo lee los .rds: nunca entrena.
# tests/testthat/test-modelos.R verifica que las predicciones en R coinciden con las del experimento.

args <- commandArgs(TRUE)
src <- args[1]; dst <- if (length(args) > 1) args[2] else "artifacts"
num <- function(x) as.numeric(unlist(x))
int <- function(x) as.integer(unlist(x))
mat <- function(x) do.call(rbind, lapply(x, num))

# Árbol binario plano (índices base 1, hoja = left 0). Sirve para sklearn, HistGB, XGBoost y LightGBM.
flat <- function(left, right, feature, threshold, value) list(left = int(left), right = int(right), feature = int(feature), threshold = num(threshold), value = num(value))

sk_tree <- function(t) {   # sklearn: hoja con left = -1; índices base 0
  l <- int(t$left); leaf <- l < 0
  flat(ifelse(leaf, 0L, l + 1L), ifelse(leaf, 0L, int(t$right) + 1L), ifelse(leaf, 0L, int(t$feature) + 1L), t$threshold, t$value)
}

hgb_tree <- function(t) {
  leaf <- int(t$leaf) == 1L
  flat(ifelse(leaf, 0L, int(t$left) + 1L), ifelse(leaf, 0L, int(t$right) + 1L), ifelse(leaf, 0L, int(t$feature) + 1L), t$threshold, t$value)
}

# Recorre un árbol anidado (XGBoost o LightGBM) y lo deja plano.
nested <- function(root, is_leaf, kids, feat, thr, val, missing_left) {
  L <- R <- F <- integer(); TH <- V <- numeric(); ML <- logical()
  walk <- function(n) {
    i <- length(L) + 1L; L[i] <<- 0L; R[i] <<- 0L; F[i] <<- 0L; TH[i] <<- NA; V[i] <<- 0; ML[i] <<- TRUE
    if (is_leaf(n)) { V[i] <<- val(n); return(i) }
    F[i] <<- feat(n); TH[i] <<- thr(n); ML[i] <<- missing_left(n)
    k <- kids(n); a <- walk(k[[1]]); b <- walk(k[[2]]); L[i] <<- a; R[i] <<- b
    i
  }
  walk(root)
  c(flat(L, R, F, TH, V), list(missing_left = ML))
}

xgb_tree <- function(t) nested(t, function(n) !is.null(n$leaf),
  function(n) { ch <- n$children; ids <- vapply(ch, function(x) x$nodeid, 0); ch[c(which(ids == n$yes), which(ids == n$no))] },
  function(n) as.integer(sub("^f", "", n$split)) + 1L, function(n) n$split_condition, function(n) n$leaf, function(n) n$missing == n$yes)

lgb_tree <- function(t) nested(t, function(n) !is.null(n$leaf_value), function(n) list(n$left_child, n$right_child),
  function(n) n$split_feature + 1L, function(n) n$threshold, function(n) n$leaf_value, function(n) isTRUE(n$default_left))

prep <- function(p) list(num = unlist(p$num), mean = num(p$mean), scale = num(p$scale), cat = unlist(p$cat),
                         categories = lapply(p$categories, unlist), drop_idx = int(p$drop_idx) + 1L)

model <- function(name, m) switch(name,
  logistic = list(coef = num(m$coef), intercept = m$intercept),
  knn = list(k = m$k, p = m$p, weights = m$weights, X = mat(m$X), y = int(m$y)),
  naive_bayes = list(theta = mat(m$theta), var = mat(m$var), prior = num(m$prior)),
  decision_tree = sk_tree(m),
  random_forest = list(trees = lapply(m$trees, sk_tree)),
  svm = list(sv = mat(m$sv), dual = num(m$dual), intercept = m$intercept, gamma = m$gamma),
  logistic_splines = list(splines = lapply(m$splines, function(s) list(t = num(s$t), k = s$k, n_out = s$n_out, xmin = s$xmin, xmax = s$xmax)),
                          order = int(m$order) + 1L, pasan = int(m$pasan) + 1L, coef = num(m$coef), intercept = m$intercept),
  hist_gb = list(baseline = m$baseline, trees = lapply(m$trees, hgb_tree)),
  xgboost = list(base_score = m$base_score, trees = lapply(m$trees, xgb_tree)),
  lightgbm = list(trees = lapply(m$trees, lgb_tree)),
  catboost = list(borders = lapply(m$float_features, function(f) num(f$borders)), scale = num(m$scale_and_bias[[1]]), bias = num(m$scale_and_bias[[2]]),
                  trees = lapply(m$oblivious_trees, function(t) list(feature = vapply(t$splits, function(s) s$float_feature_index, 0) + 1,
                                                                    border = vapply(t$splits, function(s) s$border, 0), leaf = num(t$leaf_values)))))

BENCH <- c("knn", "naive_bayes", "logistic", "decision_tree", "random_forest", "xgboost", "svm", "logistic_splines", "hist_gb", "lightgbm", "catboost")
for (name in BENCH) {
  j <- jsonlite::fromJSON(file.path(src, paste0(name, ".json")), simplifyVector = FALSE)
  out <- list(modelo = name, respuesta = j$respuesta,
              calibrados = lapply(j$calibrados, function(c) list(prep = prep(c$prep), modelo = model(name, c$modelo), a = c$a, b = c$b)))
  dir.create(file.path(dst, name), FALSE, TRUE)
  saveRDS(out, file.path(dst, name, "modelo.rds"), compress = "xz")
  cat(name, "\n")
}
j <- jsonlite::fromJSON(file.path(src, "RegionalMAPEM.json"), simplifyVector = FALSE)
out <- list(modelo = "RegionalMAPEM", prep = prep(j$prep), expertos = unlist(j$expertos), K = j$K, region_columns = int(j$region_columns) + 1L,
            scores = lapply(j$scores, num), pca_mean = num(j$pca_mean), pca_components = mat(j$pca_components), z_mean = num(j$z_mean), z_std = num(j$z_std),
            gmm_w = num(j$gmm_w), gmm_means = mat(j$gmm_means), gmm_covs = lapply(j$gmm_covs, mat), weights = mat(j$weights), hard = isTRUE(j$hard))
dir.create(file.path(dst, "RegionalMAPEM"), FALSE, TRUE)
saveRDS(out, file.path(dst, "RegionalMAPEM", "modelo.rds"), compress = "xz")
cat("RegionalMAPEM\n")
