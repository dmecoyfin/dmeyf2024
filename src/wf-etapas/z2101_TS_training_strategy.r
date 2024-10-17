#!/usr/bin/env Rscript
cat( "ETAPA  z2101_TS_training_strategy.r  INIT\n" )

# Workflow  Training Strategy

# inputs
#  * gran dataset
#  * especificacion de pariodos que van a cada  dataset, particion
#  * especificaciones de undersampling
# output  
#   Hasta tres datasets, cada uno con las mismas columnas que el original PERO con menos registros
#   1. future
#   2. final_train
#   3. con particiones de  train, validate y test


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)
require("primes", quietly=TRUE)

#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2101_TS_training_strategy.r  START\n" )
action_inicializar() 

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla)
# me quedo con PARAM$semillerio  primos al azar
envg$PARAM$semillas <- sample(primos)[1:2]


envg$PARAM$train$semilla <- envg$PARAM$semillas[1]
envg$PARAM$final_train$semilla <- envg$PARAM$semillas[2]

  
# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

cat( "ordeno_dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

archivos_salida <- c()

if( "future" %in%  names( envg$PARAM ) )
{
  cat( "inicio grabar future\n")
  # grabo los datos del futuro
  cat( "Iniciando grabado de dataset_future.csv.gz\n" )
  fwrite(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ],
    file = "dataset_future.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_future.csv.gz\n" )

  archivos_salida <- c( archivos_salida, "dataset_future.csv.gz")
}


if( "final_train" %in%  names( envg$PARAM ) )
{
  # grabo los datos donde voy a entrenar los Final Models
  cat( "Iniciando grabado de dataset_train_final.csv.gz\n" )

  set.seed(envg$PARAM$final_train$semilla, kind = "L'Ecuyer-CMRG")
  dataset[
    get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train$training,
    azar := runif(nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train$training ]))
  ]

  campos_buenos <- setdiff( colnames(dataset), c("azar") )

  fwrite(
    dataset[ get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train$training &
      (azar <= envg$PARAM$final_train$undersampling |
      get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$final_train$clase_minoritaria ), 
      campos_buenos,
      with= FALSE],
    file = "dataset_train_final.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_train_final.csv.gz\n" )

  archivos_salida <- c( archivos_salida, "dataset_train_final.csv.gz" )
}


if( "train" %in%  names( envg$PARAM ) )
{
  cat( "inicio grabar train\n")
  # grabo los datos donde voy a hacer la optimizacion de hiperparametros
  set.seed(envg$PARAM$train$semilla, kind = "L'Ecuyer-CMRG")
  dataset[
    get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training,
    azar := runif(nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training]))
  ]

  dataset[, fold_train := 0L]
  dataset[
    get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training &
      (azar <= envg$PARAM$train$undersampling |
        get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase_minoritaria ),
    fold_train := 1L
  ]

  dataset[, fold_validate := 0L]
  dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$validation, fold_validate := 1L]

  dataset[, fold_test := 0L]
  dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$testing, fold_test := 1L]

  campos_buenos <- setdiff( colnames(dataset), c("azar") )

  cat( "Iniciando grabado de dataset_training.csv.gz\n" )
  fwrite(
    dataset[fold_train + fold_validate + fold_test >= 1, 
      campos_buenos,
      with= FALSE ],
    file = "dataset_training.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_training.csv.gz\n" )

  archivos_salida <- c( archivos_salida, "dataset_training.csv.gz" )
}


#------------------------------------------------------------------------------
# copia la metadata sin modificar
cat( "grabar metadata\n")

write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )


#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(is.na(x))
    }
  ),
  "ceros" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(x == 0, na.rm = TRUE)
    }
  )
))

fwrite(tb_campos,
  file = "dataset_training.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset_train$ncol <- ncol(dataset[fold_train > 0, ])
envg$OUTPUT$dataset_train$nrow <- nrow(dataset[fold_train > 0, ])
envg$OUTPUT$dataset_train$periodos <- dataset[fold_train > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_validate$ncol <- ncol(dataset[fold_validate > 0, ])
envg$OUTPUT$dataset_validate$nrow <- nrow(dataset[fold_validate > 0, ])
envg$OUTPUT$dataset_validate$periodos <- dataset[fold_validate > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_test$ncol <- ncol(dataset[fold_test > 0, ])
envg$OUTPUT$dataset_test$nrow <- nrow(dataset[fold_test > 0, ])
envg$OUTPUT$dataset_test$periodos <- dataset[fold_test > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_future$ncol <- ncol(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ])
envg$OUTPUT$dataset_future$nrow <- nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ])
envg$OUTPUT$dataset_future$periodos <- dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_finaltrain$ncol <- ncol(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, ])
envg$OUTPUT$dataset_finaltrain$nrow <- nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, ])
envg$OUTPUT$dataset_finaltrain$periodos <- dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = archivos_salida) 
cat( "ETAPA  z2101_TS_training_strategy.r  END\n" )
