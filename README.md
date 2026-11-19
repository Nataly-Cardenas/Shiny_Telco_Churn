# Señal perdida · Telco Customer Churn

Dashboard interactivo en **R Shiny** para entender y anticipar el abandono de clientes de una empresa de telecomunicaciones. Recorre el proyecto completo: los datos, el análisis exploratorio, la preparación, la comparación de modelos y el modelo propio **RegionalMAPEM**. Además permite predecir en vivo la probabilidad de abandono de un cliente con los 12 modelos finales.

Proyecto de **Laura S. Rivera Figueredo y Natalý A. Cárdenas Izaquita**, Ciencia de Datos, Universidad del Norte (Barranquilla, Colombia).

![Portada del dashboard](preview-desktop.png)

- **Dashboard en línea:** https://z2nlvx-n0taly-c0rdenas.shinyapps.io/telco-churn/
- **Jupyter Book del proyecto:** https://nataly-cardenas.github.io/Telco_Churn_JBook/
- **Código del experimento:** https://github.com/Nataly-Cardenas/Telco_Churn_JBook

## El problema

El conjunto *Telco Customer Churn* de IBM reúne, para 7,043 clientes, los servicios contratados, los datos de la cuenta y el perfil demográfico, junto con si el cliente canceló el servicio en el último mes. El 26.5 % de los clientes abandonó.

El proyecto busca identificar a los clientes con más riesgo de irse y los factores asociados a ese riesgo, para dirigir las campañas de retención a tiempo. También evalúa, con un protocolo reproducible, si un modelo propio de combinación por regiones mejora a los modelos de referencia.

**Resultado principal:** RegionalMAPEM (log-loss 0.41127) empata con el mejor modelo de referencia, la regresión logística con splines (0.41130). Ninguna prueba estadística encuentra diferencia tras la corrección de Holm.

## Secciones del dashboard

| Sección | Contenido |
|---|---|
| **Inicio** | Portada interactiva: una red de clientes y un celular que muestra clientes reales con su probabilidad de abandono |
| **Introducción** | Problema y relevancia, pregunta de investigación, objetivos, ficha de los datos, pipeline del proyecto, marco teórico, diccionario de variables, limitaciones y ética |
| **Análisis exploratorio** | Filtros por partición, contrato, internet y antigüedad. Siete pestañas: ETL y partición, análisis univariado, bivariado y multivariado, valores faltantes, resumen, y un explorador libre con descarga en CSV |
| **Preprocesamiento** | Pasos del pipeline, validación cruzada, ablación y modelo base |
| **Modelado avanzado** | 112 configuraciones (7 modelos × 4 balanceos × 4 optimizadores), calibración, optimizadores, multifidelidad, eficiencia computacional, comparación estadística e interpretabilidad (SHAP y LIME) |
| **Modelos finales** | Tabla de líderes, predicción en vivo (probabilidad calibrada, umbral de alerta, conjunto conforme al 90 % y explicación SHAP), diagnóstico, interpretabilidad global, comparación estadística (Friedman, Nemenyi y 223 contrastes con Holm), modelo original y ficha del modelo |
| **Conclusiones** | Veredicto sobre los requisitos R1, R2 y R3, hallazgos, limitaciones, próximos pasos, reproducibilidad y paper |
| **Autoras** | Equipo y recursos del proyecto |

El dashboard está en español e inglés, tiene modo claro y oscuro, e incluye a **Antena**, un asistente que responde preguntas sobre el proyecto y cada sección.

## Los modelos

Los 12 modelos finales se entrenaron con los 5,634 clientes de desarrollo: once modelos de referencia calibrados con Platt y RegionalMAPEM. Los modelos de referencia son regresión logística, regresión logística con splines, KNN, Naive Bayes, árbol de decisión, Random Forest, XGBoost, LightGBM, CatBoost, HistGradientBoosting y SVM.

**RegionalMAPEM** combina las probabilidades de los once modelos con pesos que cambian según la región del cliente. Las regiones salen de una mezcla gaussiana sobre componentes principales, y los pesos se estiman por máxima posterior con EM, con un prior de Dirichlet que los acerca a los pesos globales.

Los modelos entrenados están en `artifacts/`, y `src/modelos.R` los evalúa directamente en R para la predicción en vivo. El dashboard no entrena modelos: solo los carga.

## Ejecutar

Requiere R 4.1 o superior.

```r
source("install.R")   # instala los paquetes una sola vez
shiny::runApp()
```

Para publicarlo en shinyapps.io:

```r
rsconnect::deployApp(appName = "telco-churn")
```

## Estructura del repositorio

```
app.R                    punto de entrada
src/
  app_ui.R, app_server.R   barra, filtros, pie y lógica del servidor
  pages_eda.R              portada, introducción y análisis exploratorio
  pages_model*.R           preprocesamiento y modelado avanzado
  pages_modelos_ml.R       modelos finales y predicción en vivo
  pages_other.R            conclusiones y autoras
  modelos.R                carga de los modelos, predicción y SHAP
  plots.R, components.R    gráficas, tablas y controles
  analytics.R, eda.R       datos, cohortes y pruebas estadísticas
  chatbot.R, i18n.R        asistente y traducción
artifacts/               los 12 modelos finales y sus metadatos
data/                    datos originales, resultados del experimento, ficha del modelo y de los datos
www/                     estilos, animaciones e imágenes
tests/                   pruebas automáticas
```

## Pruebas

```r
testthat::test_dir("tests/testthat")
```

Las pruebas comprueban:
- que las predicciones de los modelos coincidan con las del experimento;
- la validación de los datos de entrada;
- que todas las páginas se generen en ambos idiomas;
- las cifras del análisis exploratorio, las traducciones y la paleta de colores.

## Datos

[Telco Customer Churn](https://www.kaggle.com/datasets/blastchar/telco-customer-churn): conjunto de muestra de IBM, publicado en Kaggle, para uso educativo y de demostración. No contiene información personal real.
