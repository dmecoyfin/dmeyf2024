# [Competencia_1](https://github.com/fedepicado/dmeyf2024/tree/main/src/Competencia_1)

El modelo final es la combinación de dos modelos distintos: el modelo SHAP y el modelo Gustavo. Cada uno fue optimizado para encontrar los mejores hiperparametros, con los cuales se entrenaron 200 modelos distintos, donde lo unico que cambio fue la semilla. Se realizaron las 200 predicciones y para cada cliente se tomo la mayor probabilidad asignada. Luego, se concatenaron los dataset finales, seleccionando a los clientes con mayor probabilidad de baja nuevamente. Se realiza la ultima serie de envios y se selecciona un punto de corte de 13000.

## Data Quality 
Inspección de columnas y eliminación
## Backtesting 
Entreno con Febrero predigo en abril.
## SHAP
Selección de las 100 variables más importantes de una muestra sobre representada de bajas
## Optimización_modelo_SHAP
entrenamiento y predicción
## Optimización del modelo Gustavo
entrenamiento y predicción
## Subida a Kaggle
Concatenación de los resultados finales y seleccion de punto de corte
## Video Miranda
Segmentación de clientes

