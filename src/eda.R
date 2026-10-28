# Cálculos del análisis exploratorio, con las mismas pruebas que el notebook EDA_ del Jupyter Book
# (las fórmulas siguen a scipy y pandas para que las cifras coincidan con el libro).

ALPHA <- 0.05

df <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)

holm <- function(p) stats::p.adjust(p, method = "holm")

# Asimetría y curtosis (exceso) ajustadas, como pandas .skew() y .kurt().
skew <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3)
  if (n < 3 || m2 == 0) return(NA_real_)
  sqrt(n * (n - 1)) / (n - 2) * m3 / m2^1.5
}
kurt <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); m <- mean(x); m2 <- mean((x - m)^2); m4 <- mean((x - m)^4)
  if (n < 4 || m2 == 0) return(NA_real_)
  g2 <- m4 / m2^2 - 3
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}

# Tabla de contingencia con niveles ordenados (como pandas.crosstab).
crosstab <- function(a, b, cols = NULL) {
  t <- table(a, b)
  m <- matrix(as.numeric(t), nrow(t), dimnames = list(rownames(t), colnames(t)))
  if (!is.null(cols)) { out <- matrix(0, nrow(m), length(cols), dimnames = list(rownames(m), cols)); keep <- intersect(cols, colnames(m)); out[, keep] <- m[, keep]; m <- out }
  m
}

# Chi-cuadrado sin corrección, V de Cramér y Fisher exacta cuando las esperadas son insuficientes en 2×2.
contingency_test <- function(ct) {
  ct <- ct[rowSums(ct) > 0, colSums(ct) > 0, drop = FALSE]
  if (min(dim(ct)) < 2) return(list(`χ²` = NA_real_, gl = 0L, p = NA_real_, `V de Cramér` = NA_real_, Prueba = "—"))
  r <- suppressWarnings(stats::chisq.test(ct, correct = FALSE))
  chi <- unname(r$statistic); p <- r$p.value
  v <- sqrt(chi / (sum(ct) * min(nrow(ct) - 1, ncol(ct) - 1)))
  method <- "Chi-cuadrado"
  if (!all(r$expected >= 5)) {
    if (all(dim(ct) == 2)) { p <- stats::fisher.test(ct)$p.value; method <- "Fisher exacta" } else { p <- NA_real_; method <- "Esperadas < 5" }
  }
  list(`χ²` = chi, gl = as.integer(unname(r$parameter)), p = p, `V de Cramér` = v, Prueba = method)
}

target_summary <- function(d) {
  n <- c(No = sum(d$Churn == "No"), Yes = sum(d$Churn == "Yes"))
  df(Churn = names(n), Clientes = unname(n), `Porcentaje (%)` = 100 * unname(n) / max(nrow(d), 1))
}

numeric_summary <- function(d) {
  do.call(rbind, lapply(NUMERIC, function(c) {
    x <- d[[c]]; s <- x[!is.na(x)]; q <- if (length(s)) stats::quantile(s, c(.25, .5, .75), names = FALSE) else rep(NA, 3)
    df(Variable = c, N = length(s), Media = mean(s), DS = stats::sd(s), Mínimo = if (length(s)) min(s) else NA, Q1 = q[1], Mediana = q[2],
       Q3 = q[3], Máximo = if (length(s)) max(s) else NA, IQR = q[3] - q[1], Nulos = sum(is.na(x)), Asimetría = skew(s))
  }))
}

outliers <- function(d) {
  out <- lapply(NUMERIC, function(c) {
    s <- d[[c]][!is.na(d[[c]])]
    if (!length(s)) return(NULL)
    q <- stats::quantile(s, c(.25, .75), names = FALSE); iqr <- q[2] - q[1]; lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
    df(Variable = c, Q1 = q[1], Q3 = q[2], IQR = iqr, `Límite inferior` = lo, `Límite superior` = hi, `Posibles outliers` = sum(s < lo | s > hi), Asimetría = skew(s))
  })
  do.call(rbind, out)
}

numeric_tests <- function(d) {
  out <- lapply(NUMERIC, function(c) {
    no <- d[[c]][d$Churn == "No" & !is.na(d[[c]])]; yes <- d[[c]][d$Churn == "Yes" & !is.na(d[[c]])]
    if (min(length(no), length(yes)) < 2) return(NULL)
    if (c == "MonthlyCharges") { t <- stats::t.test(yes, no); method <- "t de Welch" }
    else { t <- stats::wilcox.test(yes, no, exact = FALSE, correct = TRUE); method <- "Mann–Whitney U" }
    ok <- !is.na(d[[c]])
    r <- stats::cor(as.integer(d$Churn[ok] == "Yes"), d[[c]][ok])
    df(Variable = c, Prueba = method, Estadístico = unname(t$statistic), p = t$p.value, `Diferencia media Yes−No` = mean(yes) - mean(no), `r punto-biserial` = r)
  })
  do.call(rbind, out)
}

categorical_tests <- function(d) {
  do.call(rbind, lapply(CATEGORICAL, function(c) do.call(df, c(list(Variable = c), contingency_test(crosstab(d[[c]], d$Churn, c("No", "Yes")))))))
}

# Familia de 19 contrastes (3 numéricos + 16 categóricos) corregida con Holm, como en el libro.
holm_family <- function(d) {
  num <- numeric_tests(d); cat <- categorical_tests(d)
  fam <- rbind(df(Variable = num$Variable, p = num$p, `V de Cramér` = NA_real_), df(Variable = cat$Variable, p = cat$p, `V de Cramér` = cat$`V de Cramér`))
  fam$`p Holm` <- holm(fam$p)
  fam$`Rechaza H0` <- fam$`p Holm` < ALPHA
  num$`p Holm` <- fam$`p Holm`[match(num$Variable, fam$Variable)]
  cat$`p Holm` <- fam$`p Holm`[match(cat$Variable, fam$Variable)]
  list(num = num, cat = cat, fam = fam)
}

churn_rates <- function(d) {
  do.call(rbind, lapply(CATEGORICAL, function(c) {
    ct <- crosstab(d[[c]], d$Churn, c("No", "Yes")); tot <- rowSums(ct); rate <- 100 * ct[, "Yes"] / tot
    df(Variable = c, Categoría = rownames(ct), Yes = as.integer(ct[, "Yes"]), Total = as.integer(tot), `Tasa (%)` = unname(rate), `Mayor de su variable` = unname(rate == max(rate)))
  }))
}

# Prueba K² de D'Agostino-Pearson (scipy.stats.normaltest).
normaltest <- function(x) {
  n <- length(x); m <- mean(x); m2 <- mean((x - m)^2)
  b2 <- mean((x - m)^3) / m2^1.5
  y <- b2 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  beta2 <- 3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3) / ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (beta2 - 1)); delta <- 1 / sqrt(0.5 * log(W2)); alpha <- sqrt(2 / (W2 - 1))
  if (y == 0) y <- 1
  zs <- delta * log(y / alpha + sqrt((y / alpha)^2 + 1))
  k <- mean((x - m)^4) / m2^2
  E <- 3 * (n - 1) / (n + 1); varb2 <- 24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5))
  xk <- (k - E) / sqrt(varb2)
  sb1 <- 6 * (n^2 - 5 * n + 2) / ((n + 7) * (n + 9)) * sqrt(6 * (n + 3) * (n + 5) / (n * (n - 2) * (n - 3)))
  A <- 6 + 8 / sb1 * (2 / sb1 + sqrt(1 + 4 / sb1^2))
  denom <- 1 + xk * sqrt(2 / (A - 4))
  term2 <- if (denom == 0) NA_real_ else sign(denom) * ((1 - 2 / A) / abs(denom))^(1 / 3)
  zk <- ((1 - 2 / (9 * A)) - term2) / sqrt(2 / (9 * A))
  k2 <- zs^2 + zk^2
  list(statistic = k2, p = stats::pchisq(k2, 2, lower.tail = FALSE))
}

normality <- function(d) {
  out <- lapply(NUMERIC, function(c) {
    s <- d[[c]][!is.na(d[[c]])]
    if (length(s) < 8) return(NULL)
    t <- normaltest(s)
    df(Variable = c, N = length(s), Asimetría = skew(s), Curtosis = kurt(s), `K² D'Agostino` = t$statistic, p = t$p)
  })
  r <- do.call(rbind, out)
  if (!is.null(r) && nrow(r)) { r$`p Holm` <- holm(r$p); r$`¿Normal?` <- ifelse(r$`p Holm` >= ALPHA, "Sí", "No") }
  r
}

# Gráfico Q-Q como scipy.stats.probplot: medianas de los estadísticos de orden y recta de mínimos cuadrados.
qq <- function(x) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  v <- numeric(n); v[n] <- 0.5^(1 / n); v[1] <- 1 - v[n]
  if (n > 2) v[2:(n - 1)] <- (2:(n - 1) - 0.3175) / (n + 0.365)
  theo <- stats::qnorm(v); fit <- stats::lm.fit(cbind(1, theo), x)$coefficients
  list(theo = theo, obs = x, slope = unname(fit[2]), inter = unname(fit[1]))
}

CAT_PAIRS <- list(c("InternetService", "OnlineSecurity"), c("InternetService", "TechSupport"), c("PhoneService", "MultipleLines"),
                  c("Contract", "PaperlessBilling"), c("Partner", "Dependents"))

categorical_pairs <- function(d) {
  r <- do.call(rbind, lapply(CAT_PAIRS, function(p) do.call(df, c(list(`Variable A` = p[1], `Variable B` = p[2]), contingency_test(crosstab(d[[p[1]]], d[[p[2]]]))))))
  r$`p Holm` <- holm(r$p)
  r
}

redundancy <- function(d) {
  svc <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")
  r <- df(Regla = paste0("InternetService=No ↔ ", svc, "=No internet service"),
          Discrepancias = vapply(svc, function(c) sum((d$InternetService == "No") != (d[[c]] == "No internet service")), 1L, USE.NAMES = FALSE))
  rbind(r, df(Regla = "PhoneService=No ↔ MultipleLines=No phone service", Discrepancias = sum((d$PhoneService == "No") != (d$MultipleLines == "No phone service"))))
}

# Densidad gaussiana con el ancho de banda de Scott (scipy.stats.gaussian_kde).
kde <- function(d, col, n = 200) {
  grid <- seq(min(d[[col]], na.rm = TRUE), max(d[[col]], na.rm = TRUE), length.out = n); out <- list()
  for (k in c("No", "Yes")) {
    v <- d[[col]][d$Churn == k & !is.na(d[[col]])]
    if (length(v) > 2 && length(unique(v)) > 1) {
      bw <- stats::sd(v) * length(v)^(-1 / 5)
      out[[k]] <- vapply(grid, function(g) mean(stats::dnorm((g - v) / bw)) / bw, 1)
    }
  }
  list(grid = grid, dens = out)
}

missing_table <- function(d) {
  n <- colSums(is.na(d[RAW_COLS]))
  r <- df(Variable = names(n), Nulos = unname(n), `Porcentaje (%)` = 100 * unname(n) / max(nrow(d), 1))
  r[r$Nulos > 0, , drop = FALSE]
}

mcar_tests <- function(d) {
  miss <- is.na(d$TotalCharges)
  comps <- list(Churn = d$Churn, `tenure = 0` = d$tenure == 0)
  r <- do.call(rbind, lapply(names(comps), function(nm) do.call(df, c(list(Comparación = nm), contingency_test(crosstab(miss, comps[[nm]]))))))
  r$`p Holm` <- holm(r$p)
  r
}
