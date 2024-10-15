# Competencia 01 - Replicabilidad

Este repositorio contiene los scripts necesarios para replicar exactamente el submit elegido en Kaggle para la Competencia 01.

## Contenido del repositorio

- `1_preprocesamiento.r`: Script para el preprocesamiento de datos
- `2_lightgbm_binaria_BO.r`: Script para la optimización Bayesiana de hiperparámetros de LightGBM
- `3_lightgbm_final.r`: Script para el entrenamiento del modelo final y generación de archivos para Kaggle
- `mis_semillas.txt`: Archivo con las semillas utilizadas en los experimentos

## Instrucciones de uso

1. Asegúrese de tener instalado R y los siguientes paquetes:
   - data.table
   - lightgbm
   - ulimit
   - rlist
   - DiceKriging
   - mlrMBO

2. Clone este repositorio en su máquina local.

3. Coloque el archivo de datos `competencia_01.csv` en la carpeta `datasets/` en el directorio raíz del proyecto.

4. Ejecute los scripts en el siguiente orden:

   a. `1_preprocesamiento.r`
   b. `2_lightgbm_binaria_BO.r`
   c. `3_lightgbm_final.r`

5. Los archivos de submit para Kaggle se generarán en la carpeta del experimento con el formato `KA7250_XXXXX.csv`, donde XXXXX es el número de envíos.

## Notas importantes

- El archivo `mis_semillas.txt` contiene las semillas utilizadas en los experimentos.
