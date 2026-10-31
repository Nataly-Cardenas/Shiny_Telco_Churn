# Estructura fija de la página: barra superior, cinta, filtros, contenido, pie y asistente.

TICKER <- c(N("Contrato mes a mes: 42.7 % de abandono"), N("Fibra óptica: 42.1 %"), N("Cheque electrónico: 45.7 %"), N("Primer año: el periodo más frágil"),
            N("Sin seguridad en línea: 41.9 %"), N("Mejor F1: 0.638 · Random Forest + SMOTE"), N("SMOTE mejoró 6 de 7 familias"), N("Contrato, internet y antigüedad lideran SHAP"),
            N("LinearSVC: 36× más rápido que SVC RBF"), N("Successive Halving: 7.1× menos tiempo en SVM"))

ticker <- function() div(class = "ticker-track", lapply(rep(TICKER, 2), function(t) tags$span(tags$i(class = "tick-dot"), T(t))))

split_labels <- function() list(train = T("Entrenamiento · 5,634"), test = T("Test · 1,409"), all = T("Todo · 7,043"))
CONTRACTS <- c("Todos", sort(unique(RAW$Contract))); INTERNETS <- c("Todos", sort(unique(RAW$InternetService)))

# Textos fijos de la interfaz que cambian con el idioma (los aplica www/app.js).
chrome_texts <- function() {
  list(logo = T("perdida"), logo_b = T("Señal"), nav = setNames(lapply(NAV, function(n) T(n[2])), vapply(NAV, `[`, "", 1)),
       labels = list(`f-split` = T("Universo"), `f-contract` = T("Contrato"), `f-internet` = T("Internet"), `f-tenure` = T("Antigüedad (meses)")),
       reset = T("Restablecer"), footer = T("Señal perdida · Telco Customer Churn"), theme = T("Cambiar tema claro / oscuro"),
       titles = lapply(TITLES, T), suffix = if (get_lang() == "en") " · Lost signal" else " · Señal perdida")
}

app_ui <- function(req) {
  tagList(
    tags$head(
      tags$meta(charset = "utf-8"), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "Dashboard interactivo para entender y anticipar el abandono de clientes de telecomunicaciones."),
      tags$title("Señal perdida · Telco Customer Churn"),
      # El tema se aplica antes de pintar para evitar un destello del tema equivocado.
      tags$script(HTML('try{var t=JSON.parse(localStorage.getItem("theme"));if(t)document.documentElement.dataset.theme=t;var l=JSON.parse(localStorage.getItem("lang"));if(l)document.documentElement.lang=l;}catch(e){}')),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@500;600;700&family=Inter:wght@400;500;600&display=swap"),
      tags$link(rel = "stylesheet", href = "style.css"), tags$link(rel = "stylesheet", href = "shiny.css"),
      tags$script(src = "cover.js"), tags$script(src = "app.js"), tags$script(src = "antena.js")),
    div(id = "shell", class = "shell cover-mode",
        tags$header(id = "topbar", class = "topbar",
                    tags$a(class = "logo", href = "#inicio", tags$span(class = "logo-sig", tags$b(), tags$b(), tags$b(), tags$b()),
                           tags$span(id = "logo-text", class = "logo-text", tags$b("Señal"), " perdida")),
                    tags$nav(class = "nav", lapply(NAV, function(n) tags$a(id = paste0("nav-", n[1]), class = "nav-link", href = paste0("#", n[1]), icon(n[3], "nav-i"),
                                                                       tags$span(id = paste0("nav-t-", n[1]), n[2])))),
                    div(class = "top-actions",
                        div(id = "lang-switch", class = "segmented", tags$button(id = "lang-es", class = "seg on", type = "button", "ES"), tags$button(id = "lang-en", class = "seg", type = "button", "EN")),
                        tags$button(id = "theme-btn", class = "icon-btn", type = "button", icon("sun", "sun"), icon("moon", "moon")),
                        tags$a(class = "btn small", href = BOOK, target = "_blank", icon("book", "sm"), tags$span(class = "book-btn-t", "Jupyter Book")))),
        uiOutput("ticker", class = "ticker"),
        div(id = "filters", class = "filters", style = "display:none",
            field("Universo", dropdown("split", c("train", "test", "all"), "train", split_labels()), id = "f-split"),
            field("Contrato", dropdown("contract", CONTRACTS, "Todos", list(Todos = "Todos")), id = "f-contract"),
            field("Internet", dropdown("internet", INTERNETS, "Todos", list(Todos = "Todos")), id = "f-internet"),
            field("Antigüedad (meses)", slider("tenure", 0, 72, 1, c(0, 72), c(`0` = "0", `24` = "24", `48` = "48", `72` = "72")), id = "f-tenure"),
            tags$button(id = "reset", class = "btn ghost small action-button", type = "button", icon("reset", "sm"), tags$span(id = "reset-t", "Restablecer"))),
        uiOutput("content", container = tags$main),
        tags$footer(id = "footer", class = "footer", tags$span(id = "footer-l", "Señal perdida · Telco Customer Churn"), tags$span("Laura Rivera & Natalý Cárdenas · Universidad del Norte")),
        chat_ui()))
}
