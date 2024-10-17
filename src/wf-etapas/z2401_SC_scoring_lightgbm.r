#!/usr/bin/env Rscript
cat( "ETAPA  z2401_SC_scoring.r  INIT\n")

# Workflow  Scoring

# inputs   
#   * modelos de lightgbm en formato texto  .model,  
#      correspondientes a varios ranks, y para cada rank varias semillas
#   * dataset  future
# outputs
#   para cada modelo .model  se aplica lightgbm a future 
#     y se genera  un archivo < primarykey, probabilidad >


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)

require("lightgbm", quietly=TRUE)

#cargo la libreria
# args <- c( "~/labo2024ba", "FM-0001", "TS-0001" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2401_SC_scoring.r  START\n")
action_inicializar() 


GrabarOutput()

# leo la salida de la optimizacion bayesiana
# En PARAM$input[1]  tango el nombre del experimento de Hyperparameter Tuning
arch_tb_modelos <- paste0( "./", envg$PARAM$input[1], "/tb_modelos.txt")
action_verificar_archivo( arch_tb_modelos )
tb_modelos <- fread(arch_tb_modelos)


# leo el dataset donde voy a entrenar el modelo final
# En PARAM$input[2]  tango el nombre del experimento de TS Training Strategy
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[2], "/dataset_metadata.yml" ) )

# leo el dataset donde voy a aplicar el modelo final
arch_future <- paste0("./", envg$PARAM$input[2], "/dataset_future.csv.gz")
cat( "lectura dataset_future.csv.gz\n")
dfuture <- fread(arch_future)

campos_buenos <- setdiff(colnames(dfuture), c(envg$PARAM$dataset_metadata$clase, "clase01"))

if( file.exists( "tb_future_prediccion.txt" ) ){
  tb_future_prediccion <- fread( "tb_future_prediccion.txt" )
} else {
  tb_future_prediccion <- dfuture[ ,
      c( envg$PARAM$dataset_metadata$primarykey, envg$PARAM$dataset_metadata$clase),
      with=FALSE ]
}

qpred <- nrow( tb_modelos )

dfuture_matriz <- data.matrix(dfuture[, campos_buenos, with = FALSE])

for ( ipred in seq(qpred) ) {

  mod <- tb_modelos[ ipred ]
  cat("\nmodelo_rank: ", mod$rank, ", isem: ", mod$isem, "\n")
  envg$OUTPUT$status$modelo_rank <- mod$rank
  envg$OUTPUT$status$modelo_isem <- mod$isem

  modelo_final <- lgb.load(filename =  paste0( "./", envg$PARAM$input[1], "/", mod$archivo))

  # genero la prediccion, Scoring
  cat( "creo predict\n")
  prediccion <- predict(
    modelo_final,
    dfuture_matriz
  )

  campo_pred <- paste0("m_",mod$rank, "_", mod$isem)
  tb_future_prediccion[, paste0(campo_pred) := prediccion ]
  tb_modelos[ ipred, campo := campo_pred ]
  tb_modelos[ ipred, archivo_pred := "tb_future_prediccion.txt" ]

  rm( prediccion )
  rm( modelo_final )
  gc(verbose= FALSE)
}


fwrite( tb_modelos[ , list(rank,iteracion_bayesiana, isem, semilla, campo, archivo_pred) ],
        file= "tb_predicciones.txt",
        sep= "\t" )


fwrite( tb_future_prediccion,
        file = "tb_future_prediccion.txt",
        sep = "\t" )

#------------------------------------------------------------------------------
# copia la metadata sin modificar
cat( "grabar metadata\n")

write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c()) 
cat( "ETAPA  z2401_SC_scoring.r  END\n")
