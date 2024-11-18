#!/usr/bin/env Rscript
cat( "ETAPA  z2302_FM_final_models_lightgbm_semillerio.r  INIT\n")

# Workflow  final_models

# inputs
#  * dataset  final_training
#  * archivo BO_log.txt de una Bayesian Optimization
#  * ranks para los que se tiene interes en generar modelos
#  * cantidad de semillas
# output  
#   para cada rank, semilla  un   modelos  .model


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose=FALSE) # garbage collection

require("data.table")
require("yaml")
require("primes")

require("lightgbm")

#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# grabo la importancia de variables

grabar_importancia <- function(modelo_final, modelo_rank, iteracion_bayesiana) {
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(tb_importancia,
    file = paste0(
      "impo_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".txt"
    ),
    sep = "\t"
  )

  rm(tb_importancia)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2302_FM_final_models_lightgbm_semillerio.r  START\n")
action_inicializar() 

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla, kind = "L'Ecuyer-CMRG")
total_semillas <- envg$PARAM$semillerio * envg$PARAM$repeticiones_exp
# me quedo con PARAM$semillerio  primos al azar
envg$PARAM$semillas <- sample(primos)[1:total_semillas]


GrabarOutput()

# leo la salida de la optimizacion bayesiana
# En PARAM$input[1]  tango el nombre del experimento de Hyperparameter Tuning
arch_log <- paste0( "./", envg$PARAM$input[1], "/BO_log.txt")
action_verificar_archivo( arch_log )
tb_log <- fread(arch_log)
setorderv(tb_log, "metrica", envg$PARAM$metrica_order)


# leo el dataset donde voy a entrenar el modelo final
# En PARAM$input[2]  tango el nombre del experimento de TS Training Strategy
arch_dataset <- paste0("./", envg$PARAM$input[2], "/dataset_train_final.csv.gz")
cat( "lectura dataset_train_final.csv.gz\n")
action_verificar_archivo( arch_dataset )
dataset <- fread(arch_dataset)
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[2], "/dataset_metadata.yml" ) )


campos_buenos <- setdiff(colnames(dataset), c(envg$PARAM$dataset_metadata$clase, "clase01"))

dataset[ , clase01 := 
  ifelse( get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 1, 0 ) ]

# genero un modelo para cada uno de las modelos_qty MEJORES
# iteraciones de la Bayesian Optimization
vganancias_suavizadas <- c()


if( file.exists( "tb_modelos.txt" ) ){
  tb_modelos <- fread( "tb_modelos.txt" )
} else {
  tb_modelos <- data.table( 
    rank= integer(),
    iteracion_bayesiana= integer(),
    repeticion= integer(),
    semilla= integer(),
    isem= integer(),
    archivo= character() )
}

# puedo resumir
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rcanresume.txt",
    append = TRUE
   )

imodelo <- 0L
for (modelo_rank in envg$PARAM$modelos_rank) {
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  envg$OUTPUT$status$modelo_rank <- modelo_rank

  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana


  # creo CADA VEZ el dataset de lightgbm
  cat( "creo lgb.Dataset\n")
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01],
    free_raw_data = FALSE
  )

  ganancia <- parametros$ganancia

  # elimino los parametros que no son de lightgbm
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$metrica <- NULL
  parametros$iteracion_bayesiana <- NULL

  # recalculo min_data_in_leaf  y  num_leaves  para ajustarlo al nuevo dataset  dtrain
  if( "learning_rate_log" %in% names(parametros) )
   parametros$learning_rate <- 2.0 ^ parametros$learning_rate_log

  if( "feature_fraction_log" %in% names(parametros) )
   parametros$feature_fraction <- 2.0 ^ parametros$feature_fraction_log

  if( "num_iterations_log" %in% names(parametros) )
   parametros$num_iterations <- pmax( 1L, as.integer( round( 2.0 ^ parametros$num_iterations_log ) ))

  # para que extra_trees pueda ser  c( 0, 1, integer )
  if( "extra_trees"  %in%  names(parametros) )
   parametros$extra_trees <- as.logical( parametros$extra_trees )

  # para que is_unbalance pueda ser  c( 0, 1, integer )
  if( "is_unbalance"  %in%  names(parametros) )
   parametros$is_unbalance <- as.logical( parametros$is_unbalance )

  if( "min_data_in_leaf_log" %in% names(parametros) )
   parametros$min_data_in_leaf <- pmax( 1L, as.integer( round(2.0 ^ parametros$min_data_in_leaf_log)))

  # hago la transformacion de leaf_size_log y  coverage
  if( "leaf_size_log"  %in% names(parametros) &
      "coverage_log"  %in% names(parametros)
  )
  {
    # primero defino el tamaño de las hojas
    parametros$min_data_in_leaf <- pmax( 1,  round( nrow(dtrain) * ( 2.0 ^ parametros$leaf_size_log ))  )
    # luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
    parametros$num_leaves <- pmin( 131072, 
     pmax( 8,  round( ((2.0^parametros$coverage_log)) * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
  }

  if( "leaf_size_log"  %in% names(parametros) & 
      !( "coverage_log"  %in% names(parametros) )
  )
  {
    # primero defino el tamaño de las hojas
    parametros$min_data_in_leaf <- pmax( 1,  round( nrow(dtrain) * ( 2.0 ^ parametros$leaf_size_log ))  )
  }


  #  parametros$num_iterations  <- 10  # esta linea es solo para pruebas

  for( irepeticion in 1:envg$PARAM$repeticiones_exp )
  {
    desde <- envg$PARAM$semillerio * (irepeticion -1 ) + 1
    hasta <- desde + envg$PARAM$semillerio -1

    sem <- 0L

    for (vsemilla in envg$PARAM$semillas[desde:hasta] )
    {
      sem <- sem + 1L
      cat(sem, " ")
      envg$OUTPUT$status$sem <- sem
      GrabarOutput()

      # Utilizo la semilla definida en este script
      parametros$seed <- vsemilla

      nombre_raiz <- paste0(
        sprintf("%02d", modelo_rank),
        "_",
        sprintf("%03d", irepeticion),
        "_",
        sprintf("%03d", iteracion_bayesiana),
        "_s",
        parametros$seed
      )

      arch_modelo <- paste0(
        "modelo_",
        nombre_raiz,
        ".model"
      )

      # genero el modelo entrenando en los datos finales
      #  en caso que ya no exista
      if( !file.exists( arch_modelo ) )
      {
        cat( "\nentrenando modelo = ", sem, "  ." )
        set.seed(parametros$seed, kind = "L'Ecuyer-CMRG")
        modelo_final <- lightgbm(
          data = dtrain,
          param = parametros,
          verbose = -100
        )
        cat( " ...Fin." )

        # grabo el modelo, achivo .model
        lgb.save(modelo_final,
          file = arch_modelo
        )

        # creo y grabo la importancia de variables, solo para la primer semilla
        if (sem == 1) {
          cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
              file = "z-Rcanresume.txt",
              append = TRUE
          )

          grabar_importancia(modelo_final, modelo_rank, iteracion_bayesiana)
        }

        # Agrego a tb_semillas
        tb_modelos <- rbind( tb_modelos,
           list(modelo_rank,
                iteracion_bayesiana,
                irepeticion,
                vsemilla,
                sem,
               arch_modelo
               ))

        fwrite( tb_modelos,
                file = "tb_modelos.txt",
                sep ="\t"
              )

        rm( "modelo_final" )
        gc(verbose= FALSE)
      }
    }
  }

  rm( "dtrain" )
  gc(verbose= FALSE)
}

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
cat( "ETAPA  z2302_FM_final_models_lightgbm_semillerio.r  END\n")
