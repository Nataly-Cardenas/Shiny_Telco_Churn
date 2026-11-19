# Señal perdida · Dashboard Telco Customer Churn (Shiny)

Dashboard predictivo en **R Shiny y Plotly** para el proyecto de abandono de clientes de **Laura S. Rivera Figueredo y Natalý A. Cárdenas Izaquita** (Universidad del Norte). Es la versión en R del dashboard publicado en https://telco-churn-dashboard-hom3.onrender.com (repositorio [Dash_Telco_Churn_ML](https://github.com/lauraformore/Dash_Telco_Churn_ML), rama `dash-original-con-ML`). Tiene las mismas secciones, gráficas, textos, colores, animaciones, asistente e idiomas, y predice en vivo con los mismos **12 modelos finales**: 11 benchmarks calibrados y el modelo propio, RegionalMAPEM. Todo el código de la aplicación es R.

![Portada del dashboard](preview-desktop.png)

- Jupyter Book: https://nataly-cardenas.github.io/Telco_Churn_JBook/ (capítulo 8: dashboard y despliegue)
- Experimento y resultados: https://github.com/Nataly-Cardenas/Telco_Churn_JBook
- Versión en Python (Dash): https://telco-churn-dashboard-hom3.onrender.com

## Secciones

| Ruta | Página | Contenido |
|---|---|---|
| `#inicio` | Portada | Red de clientes interactiva y un celular que muestra 30 clientes reales del hold-out con la probabilidad de RegionalMAPEM |
| `#introduccion` | Introducción | Problema y relevancia, pregunta de investigación, objetivos, ficha de los datos, pipeline del proyecto, marco teórico, diccionario de variables, limitaciones y ética |
| `#eda` | Análisis exploratorio | Filtros globales (partición, contrato, internet, antigüedad) y 7 pestañas: ETL y partición, univariado, bivariado, multivariado, valores faltantes, resumen, y explorador libre (ejes x, y y color a elección) con tabla y descarga CSV |
| `#preprocesamiento` | Preprocesamiento | Pasos del pipeline, validación cruzada, ablación y evaluación del modelo base |
| `#modelado` | Modelado avanzado | Podio y 7 pestañas: 112 configuraciones, calibración, optimizadores, multifidelidad, eficiencia computacional, comparación estadística e interpretabilidad |
| `#modelos_ml` | Modelos finales | Tabla de líderes, predicción en vivo (umbral de alerta, conjunto conforme al 90 % y SHAP local), diagnóstico, interpretabilidad global, comparación estadística (diagrama de diferencia crítica y 223 contrastes con Holm), modelo original y ficha del modelo |
| `#conclusiones` | Conclusiones | Veredicto sobre R1, R2 y R3, hallazgos, limitaciones, próximos pasos, reproducibilidad y paper |
| `#autoras` | Autoras | Autoras, Jupyter Book y recursos |

Las secciones usan fragmentos de URL, así que admiten enlaces directos y los botones de atrás y adelante del navegador.

## Ejecutar

Requiere R 4.1 o superior.

```r
source("install.R")   # una sola vez
shiny::runApp()
```

En RStudio basta con abrir la carpeta del proyecto y ejecutar esas dos líneas en la consola. Desde una terminal: `Rscript -e "shiny::runApp(port = 8050, launch.browser = TRUE)"`. En Windows, si `Rscript` no se reconoce, se usa la ruta completa, por ejemplo `"C:\Program Files\R\R-4.x.x\bin\Rscript.exe"`. Para publicar en shinyapps.io: `rsconnect::deployApp()`.

## Modelos en R

Los 12 artefactos del dash (`modelo.joblib` de scikit-learn, XGBoost, LightGBM y CatBoost) se convirtieron una sola vez a `artifacts/<modelo>/modelo.rds` con sus parámetros ajustados. Cada `.rds` guarda el escalado y el one-hot, los coeficientes, los árboles, los vectores de soporte, los B-splines y los calibradores de Platt. El de RegionalMAPEM guarda además los cuantiles, la PCA, el GMM de regiones y los pesos por región. `src/modelos.R` reimplementa en R la inferencia de cada modelo tal como la hace su librería original, incluida la comparación en precisión simple de los árboles. Por eso **las probabilidades coinciden con las del experimento**: la diferencia máxima es menor que 1e-7 en los 40 clientes de `artifacts/prueba_ida_y_vuelta.csv` y en el hold-out. Lo comprueba `tests/testthat/test-modelos.R`.

- `scripts/convertir_artefactos.R` arma los `.rds` a partir de los parámetros exportados en JSON.
- La explicación local reproduce el **Kernel SHAP de shap 0.48** que usa el dash. Toma los mismos 12 clientes de referencia, 100 coaliciones y la selección de 10 variables con LARS. El sorteo de coaliciones usa el mismo generador aleatorio de numpy (`np.random.seed(0)`, Mersenne Twister). Así, **los valores SHAP son los mismos que muestra el dash**; las pruebas lo verifican con el cliente de ejemplo.
- El dashboard **nunca entrena**: solo carga los artefactos una vez por proceso.

## Asistente: Antena

El botón flotante abre a **Antena**, que saluda en voz alta (Web Speech API, `www/antena.js`). Responde sin límite de preguntas sobre el proyecto, sus datos y cada sección, con la base de conocimiento del dash (`data/faq.json`). Para perfiles concretos («tasa de abandono con fibra y mes a mes») calcula la cifra sobre el conjunto de entrenamiento. Si existe la variable de entorno `ANTHROPIC_API_KEY`, redacta las respuestas con Claude, y si la API falla vuelve a la respuesta local.

## Idioma, tema y paleta

Arriba a la derecha están los botones `ES | EN` y el cambio entre modo claro y oscuro; el navegador recuerda la elección. Las gráficas usan `#2E86AB` para todas las series, `#E4572E` para resaltar y `#9AA5B1` para referencias. Cada gráfica toma como alto máximo `min(alto, 100vh − 230 px)` para que quepa al proyectar en 1280 × 720.

## Archivos

- `app.R`: punto de entrada.
- `src/app_ui.R`, `src/app_server.R`: barra, cinta, filtros, pie y lógica del servidor.
- `src/pages_eda.R`: portada, Introducción y Análisis exploratorio.
- `src/pages_model.R`, `src/pages_model_extra.R`: Preprocesamiento y Modelado avanzado.
- `src/pages_modelos_ml.R`: Modelos finales.
- `src/pages_other.R`: Conclusiones y Autoras.
- `src/modelos.R`: carga de artefactos, validación del formulario, predicción y SHAP local.
- `src/plots.R`, `src/components.R`: gráficas (gauge, ROC, calibración, diferencia crítica, cascada SHAP…), tarjetas, tablas y controles.
- `src/analytics.R`, `src/eda.R`: datos, cohortes, resultados del experimento y pruebas estadísticas.
- `src/chatbot.R`, `src/i18n.R`, `src/icons.R`: asistente, traducción e íconos.
- `www/`: estilos y animaciones. `style.css`, `cover.js` y `antena.js` se copiaron sin cambios del dash y `.gitattributes` los marca como externos. `app.js` y `shiny.css` hacen que los controles de Shiny se vean y funcionen como los de Dash.
- `artifacts/`: los 12 modelos (`modelo.rds`, `metadata.json`).
- `data/`: datos originales, resultados del experimento (`resultados/`), `MODEL_CARD.md` y `DATA_STATEMENT.md`.

## Verificar

```r
testthat::test_dir("tests/testthat")
```

Las pruebas comprueban:
- la equivalencia de los 12 modelos con el experimento;
- que el SHAP local coincida con el del dash;
- la validación de entradas;
- que todas las páginas y pestañas se generen en español e inglés;
- las cifras del EDA, las traducciones y la paleta.

En Windows, si aparecen errores de codificación, hay que ejecutarlas con la configuración regional UTF-8.
