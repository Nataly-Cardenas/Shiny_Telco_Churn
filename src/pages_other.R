# 6. Conclusiones y Autoras.

PAPER <- "https://doi.org/10.5281/zenodo.24268653"
REPO_ML <- "https://github.com/Nataly-Cardenas/Telco_Churn_Entregable"

# ================================================================ 6. CONCLUSIONES
REPRO <- list(c("file", N("Versiones exactas"), N("requirements.txt fija cada librería usada en el entrenamiento.")),
              c("shield", N("Verificación automática"), N("verificar_reproduccion.py compara versiones, huellas SHA-256 de los datos y los artefactos de las 1,680 unidades.")),
              c("layers", N("Resultados versionados"), N("Pipelines, predicciones, checkpoints y semillas de cada unidad quedan guardados; el libro se construye sin reentrenar.")),
              c("code", N("Código del experimento"), N("Un apéndice reproduce las 32 celdas exactas que entrenaron las 112 configuraciones.")))

conclusiones <- function(d) {
  v <- resultado("veredicto_R3.json")
  finds <- list(c("layers", N("El perfil de cuenta delata el riesgo"), N("Contrato, antigüedad y servicio de internet concentran la señal, en el EDA y en SHAP.")),
                c("link", N("El modelo propio empata con la mejor referencia"), N("RegionalMAPEM (log-loss 0.41127) y la logística con splines (0.41130) no se separan: ninguna prueba rechaza tras Holm y el efecto es despreciable.")),
                c("scale", N("Combinar expertos calibrados sí ayuda"), N("RegionalMAPEM supera con al menos tres pruebas a siete de los once modelos de referencia; un vector de pesos global lo logra igual, con menos costo.")),
                c("gauge", N("Las regiones no aportan en Telco"), N("La fiabilidad relativa de los expertos casi no cambia entre tipos de cliente; el prior de Dirichlet protege al modelo, pero la ponderación local no mejora al ensamble global.")),
                c("hourglass", N("Buscar puede costar menos"), N("Successive Halving igualó el F1 de una búsqueda completa en Random Forest y SVM con 3.7× y 7.1× menos tiempo; en XGBoost no conviene.")),
                c("flame", N("El costo está en el núcleo del algoritmo"), N("La logística con splines busca en 55 s, predice en 0.11 s por 1,000 clientes y pesa 0.04 MB.")))
  limits <- c(N("Un solo conjunto principal: las conclusiones confirmatorias son sobre Telco."), N("KDDCup09 se evaluó con 2 repeticiones por costo de cómputo."),
              N("Los datos son una foto sintética de un operador: no garantizan anticipar el abandono futuro."), N("Sin costos de intervención, el log-loss no se traduce en dinero."))
  steps <- list(c(N("Validación temporal"), N("Evaluar en clientes futuros.")), c(N("Costos explícitos"), N("Elegir el umbral con el costo de cada error.")),
                c(N("Datos con más heterogeneidad"), N("Probar el mecanismo en conjuntos con segmentos de negocio claramente distintos o más variables de uso.")))
  verdict <- list(c("R1", N("Novedad"), N("Se cumple, con alcance acotado"), N("La combinación por zonas ya existía en churn; lo nuevo es la regla: pertenencias suaves, expertos fijos y MAP-EM con prior de Dirichlet."), "cyan"),
                  c("R2", N("Implementación propia"), N("Se cumple"), N("PCA, GMM y MAP-EM programados en numpy y scipy, con interfaz de scikit-learn y 12 pruebas unitarias."), "cyan"),
                  c("R3", N("Superioridad"), N("No se cumple"), N("Solo se cumple 1 de las 4 condiciones: la diferencia es de {d} en log-loss y ninguna prueba rechaza."), "coral"))
  tagList(page_head("conclusiones", T("Qué aprendimos, qué sigue siendo incierto y qué falta para llevarlo a una campaña de retención.")),
          subhead(T("Veredicto sobre R1, R2 y R3"), T("Resultado negativo documentado: el protocolo se congeló antes de ver los resultados.")),
          div(class = "findings", lapply(verdict, function(r) div(class = "finding", tags$span(class = "obj-n", r[1]), tags$h4(T(r[2])), tags$span(class = paste("pill", r[5]), T(r[3])),
                                                                   tags$p(fmt(T(r[4]), d = sprintf("%.5f", abs(v$diferencia))))))),
          subhead(T("Hallazgos principales")),
          div(class = "findings", lapply(finds, function(f) div(class = "finding", tags$span(class = "drv-icon", icon(f[1])), tags$h3(T(f[2])), tags$p(T(f[3]))))),
          grid(card(eyebrow(T("LIMITACIONES")), tags$ul(lapply(limits, function(x) tags$li(T(x))))),
               card(eyebrow(T("PRÓXIMOS PASOS")), div(lapply(seq_along(steps), function(k) div(class = "next", tags$span(class = "obj-n", sprintf("0%d", k)), div(tags$h4(T(steps[[k]][1])), tags$p(T(steps[[k]][2])))))))),
          card(eyebrow(T("REPRODUCIBILIDAD")), tags$h3(T("Todo lo que se reporta se puede verificar")),
               div(class = "repro", lapply(REPRO, function(r) div(class = "station", tags$span(class = "st-icon", icon(r[1])), tags$h4(T(r[2])), tags$p(T(r[3]))))),
               tags$a(class = "chip-link", href = paste0(BOOK, "CODIGO_EXPERIMENTO.html"), target = "_blank", icon("code", "sm"), T("Ver el código del experimento en el Jupyter Book"), icon("arrow", "sm"))),
          card(cls = "glow", div(class = "paper-card",
               div(eyebrow(T("PAPER DEL PROYECTO")), tags$h2(T("Ponderación por regiones con MAP-EM de clasificadores calibrados para predecir la fuga de clientes")),
                   tags$p(T("Modelo propuesto, protocolo pre-registrado, 223 comparaciones con Holm y veredicto sobre R1, R2 y R3."))),
               div(class = "cover-cta", tags$a(class = "btn primary", href = PAPER, target = "_blank", icon("file"), T("Leer el paper")),
                   tags$a(class = "btn ghost", href = REPO_ML, target = "_blank", icon("code"), T("Repositorio"))))))
}

# ================================================================ AUTORAS
autoras <- function(d) {
  people <- list(c("laura.jpg", "Laura S. Rivera Figueredo", "sriveral@uninorte.edu.co", "https://github.com/lauraformore", "https://www.linkedin.com/in/laura-s-rivera-figueredo-862451315/"),
                 c("nataly.jpg", "Natalý A. Cárdenas Izaquita", "nizaquita@uninorte.edu.co", "https://github.com/Nataly-Cardenas", "https://www.linkedin.com/in/n%C3%A1taly-c%C3%A1rdenas-iz%C3%A1quita"))
  res <- list(c("code", T("Código y notebooks"), T("Repositorio Telco_Churn_JBook"), REPO), c("sliders", T("Este dashboard"), T("Repositorio Shiny_Telco_Churn"), SHINY_REPO),
              c("table", T("Conjunto de datos"), "Kaggle · Telco Customer Churn (IBM)", "https://www.kaggle.com/datasets/blastchar/telco-customer-churn"), c("file", "Paper", T("Versión web y PDF"), paste0(BOOK, "paper.html")))
  tagList(page_head("autoras", T("El equipo detrás del proyecto y los recursos para profundizar.")),
          div(class = "authors", lapply(people, function(p) div(class = "author",
              div(class = "avatar", tags$img(src = paste0("img/", p[1]), alt = fmt(T("Foto de {n}"), n = p[2]))), tags$h2(p[2]),
              tags$p(class = "muted", T("Ciencia de Datos · Universidad del Norte")),
              div(class = "author-links", tags$a(class = "chip-link linkedin", href = p[5], target = "_blank", icon("linkedin", "sm"), "LinkedIn"),
                  tags$a(class = "chip-link", href = p[4], target = "_blank", icon("code", "sm"), "GitHub"),
                  tags$a(class = "chip-link", href = paste0("mailto:", p[3]), icon("mail", "sm"), p[3]))))),
          tags$p(class = "center muted", T("Departamento de Matemáticas, Física y Ciencia de Datos · Universidad del Norte · Barranquilla, Colombia")),
          tags$a(class = "book-cta", href = BOOK, target = "_blank", tags$span(class = "book-icon", icon("book")),
                 div(eyebrow("JUPYTER BOOK"), tags$h2(T("Lee el proyecto completo")), tags$p(T("Introducción, marco teórico, EDA, preprocesamiento, modelado y paper."))),
                 tags$span(class = "book-arrow", icon("arrow"))),
          do.call(grid, c(lapply(res, function(r) tags$a(class = "res", href = r[4], target = "_blank", tags$h4(icon(r[1]), r[2]), tags$p(r[3]))), cols = 4)))
}

PAGES <- list(inicio = function(d, tab) cover(), introduccion = function(d, tab) introduccion(d), eda = eda_page, preprocesamiento = preprocesamiento,
              modelado = modelado, modelos_ml = function(d, tab) modelos_ml(tab), conclusiones = function(d, tab) conclusiones(d), autoras = function(d, tab) autoras(d))
