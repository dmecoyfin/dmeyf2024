#!/usr/bin/env Rscript
cat( "ETAPA  z2402_SC_scoring_lightgbm_semillerio.r  INIT\n")

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
gc(full=TRUE, verbose=FALSE) # garbage collection

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
cat( "ETAPA  z2402_SC_scoring_lightgbm_semillerio.r  START\n")
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

lista_semi <- list()

for ( ipred in seq(qpred) ) {

  mod <- tb_modelos[ ipred ]

  # semillerio
  semi_str <- paste0( "sem_", mod$rank, "_", mod$repeticion )
  if( !( semi_str %in%  names( lista_semi ) ) ) {
    lista_semi[[semi_str]]$mod <- mod
    lista_semi[[semi_str]]$repeticion <- mod$repeticion
    lista_semi[[semi_str]]$qty  <- 0
    lista_semi[[semi_str]]$pred <- rep( 0, nrow(dfuture) )
  }

  cat("\nmodelo_rank: ", mod$rank, ", repeticion: ", mod$repeticion, ", isem: ", mod$isem, "\n")
  envg$OUTPUT$status$modelo_rank <- mod$rank
  envg$OUTPUT$status$repeticion <- mod$repeticion
  envg$OUTPUT$status$modelo_isem <- mod$isem

  modelo_final <- lgb.load(filename =  paste0( "./", envg$PARAM$input[1], "/", mod$archivo))

  # genero la prediccion, Scoring
  cat( "creo predict\n")
  prediccion <- predict(
    modelo_final,
    dfuture_matriz
  )

  # semillerio
  lista_semi[[semi_str]]$qty <- lista_semi[[semi_str]]$qty + 1
  lista_semi[[semi_str]]$pred <- lista_semi[[semi_str]]$pred + prediccion

  campo_pred <- paste0("m_",mod$rank, "_", mod$repeticion, "_", mod$isem)
  tb_future_prediccion[, paste0(campo_pred) := prediccion ]
  tb_modelos[ ipred, semillerio := 0L ]
  tb_modelos[ ipred, qsemillas := 1L ]
  tb_modelos[ ipred, campo := campo_pred ]
  tb_modelos[ ipred, archivo_pred := "tb_future_prediccion.txt" ]

  rm( prediccion )
  rm( modelo_final )
  gc(verbose= FALSE)
}


# grabo campos de semillerios
semillerios <- names( lista_semi )
for( semi_str in  semillerios )
{
  campo_pred <- semi_str

  lista_semi[[campo_pred]]$mod$isem <- -1L
  lista_semi[[campo_pred]]$mod$semilla <- -1L
  lista_semi[[campo_pred]]$mod$semillerio <- 1L
  lista_semi[[campo_pred]]$mod$qsemillas <- lista_semi[[semi_str]]$qty
  lista_semi[[campo_pred]]$mod$campo <- campo_pred
  lista_semi[[campo_pred]]$mod$archivo_pred <- "tb_future_prediccion.txt"

  tb_future_prediccion[, paste0(campo_pred) := lista_semi[[semi_str]]$pred / lista_semi[[semi_str]]$qty ]
  tb_modelos <- rbindlist( list( tb_modelos, lista_semi[[campo_pred]]$mod ) )
}


fwrite( tb_modelos[ , list(rank,iteracion_bayesiana, repeticion, semillerio, qsemillas, isem, semilla, campo, archivo_pred) ],
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
cat( "ETAPA  z2402_SC_scoring_semillerio.r  END\n")
