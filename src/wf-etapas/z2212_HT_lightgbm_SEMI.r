#!/usr/bin/env Rscript
cat( "ETAPA  z2212_HT_lightgbm_SEMI.r  INIT\n")

# Hyperparameter Tuning  lightgbm

# inputs
#  * dataset  con particion  train, valitate, test
#  * hiperparametros fijos y variables (que van a la Bayesian Optimization )
# output  
#   archivo  BO_log.txt  resultado de la Bayesian Optimization

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose=FALSE) # garbage collection

require("data.table", quietly=TRUE) 
require("rlist", quietly=TRUE) 
require("yaml", quietly=TRUE) 
require("primes", quietly=TRUE) 

require("lightgbm", quietly=TRUE) 

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging", quietly=TRUE) 
require("mlrMBO", quietly=TRUE) 


#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/mlog.r" ) )
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------

GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")


  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,  envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]

  tbl[, gan_suavizada :=
    frollmean(
      x = gan_acum, n = envg$PARAM$train$meseta, align = "center",
      na.rm = TRUE, hasNA = TRUE
    )]

  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]


  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }


  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm <- function(x) {

  cat( "Inicio EstimarGanancia_lightgbm()\n")
  gc(verbose= FALSE)
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  envg$OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  # para que una siguiente corrida pueda retomar
  if( file.exists("bayesiana.RDATA") ) {

    cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rcanresume.txt",
      append = TRUE
     )
  }


  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(envg$PARAM$lgb_basicos, x)

  if( "learning_rate_log" %in% names(param_completo) )
   param_completo$learning_rate <- 2.0 ^ param_completo$learning_rate_log

  if( "feature_fraction_log" %in% names(param_completo) )
   param_completo$feature_fraction <- 2.0 ^ param_completo$feature_fraction_log

  if( "num_iterations_log" %in% names(param_completo) )
   param_completo$num_iterations <- pmax( 1L, as.integer( round( 2.0 ^ param_completo$num_iterations_log ) ))

  # para que extra_trees pueda ser  c( 0, 1, integer )
  if( "extra_trees"  %in%  names(param_completo) )
   param_completo$extra_trees <- as.logical( param_completo$extra_trees )

  # para que is_unbalance pueda ser  c( 0, 1, integer )
  if( "is_unbalance"  %in%  names(param_completo) )
   param_completo$is_unbalance <- as.logical( param_completo$is_unbalance )

  if( "min_data_in_leaf_log" %in% names(param_completo) )
   param_completo$min_data_in_leaf <- pmax( 1L, as.integer( round(2.0 ^ param_completo$min_data_in_leaf_log)))

  # hago la transformacion de leaf_size_log y  coverage
  if( "leaf_size_log"  %in% names(param_completo) &
      "coverage_log"  %in% names(param_completo)
  )
  {
    # primero defino el tamaño de las hojas
    param_completo$min_data_in_leaf <- pmax( 1,  round( nrow(dtrain) * ( 2.0 ^ param_completo$leaf_size_log ))  )
    # luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
    param_completo$num_leaves <- pmin( 131072, 
     pmax( 8,  round( ((2.0^param_completo$coverage_log)) * nrow( dtrain ) / param_completo$min_data_in_leaf ) ) )
  }

  if( "leaf_size_log"  %in% names(param_completo) & 
      !( "coverage_log"  %in% names(param_completo) )
  )
  {
    # primero defino el tamaño de las hojas
    param_completo$min_data_in_leaf <- pmax( 1,  round( nrow(dtrain) * ( 2.0 ^ param_completo$leaf_size_log ))  )
  }




  param_completo$early_stopping_rounds <-
    as.integer( param_completo$early_stopping_base + 4 / param_completo$learning_rate)
  param_completo$early_stopping_base <- NULL

  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )

  cat("\n")

  cant_corte <- vcant_optima[modelo_train$best_iter]

  tiempo_normals <- c()
  gan_normals <- c()
  cant_normals <- c()
  semillas_normals <- c()
  for( iexp in seq(envg$PARAM$train$repeticiones_exp) )
  {
    # donde acumulo el semillerio
    tb_semillerio <- as.data.table( list( prob_acum = rep(0, nrow(dataset_test)) ))

    semi_ganancia_test <- 0
    semi_cantidad_test_normalizada <- 0

    t0_loop <- proc.time()
    tiempo_loop <- 0.0

    isem <- 0
    while( isem < envg$PARAM$train$semillerio & tiempo_loop < envg$PARAM$train$timeout )
    {
      t0_local <- proc.time()
      isem <- isem + 1
      param_ganador <- copy( param_completo )
      if( "leaf_size_log" %in% param_ganador )  param_ganador$leaf_size_log <- NULL
      if( "coverage_log" %in% param_ganador )  param_ganador$coverage_log <- NULL

      param_ganador$num_iterations <- modelo_train$best_iter
      param_ganador$early_stopping <- 0
      sem <- envg$PARAM$semillas[  (iexp-1)*envg$PARAM$train$repeticiones_exp + isem ]
      param_ganador$seed <- sem

      set.seed(envg$PARAM$seed, kind = "L'Ecuyer-CMRG")
      modelo_local <- lgb.train(
        data = dtrain,
        param = param_ganador,
        verbose = -100
      )

      t1_local <- proc.time()
      tiempo_local <- as.numeric( t1_local[1] - t0_local[1] + t1_local[2] - t0_local[2] )
      tiempo_loop <- as.numeric( t1_local[1] - t0_loop[1] + t1_local[2] - t0_loop[2] )

      # aplico el modelo a testing y calculo la ganancia
      prediccion <- predict(
        modelo_local,
        data.matrix(dataset_test[, campos_buenos, with = FALSE])
      )

      # acumulo en el semillerio
      tb_semillerio[ , prob_acum := prob_acum + prediccion ]

      tbl1 <- copy(dataset_test[, list("gan" = 
        ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$positivos, 
           envg$PARAM$train$gan1, 
           envg$PARAM$train$gan0))])

      tbl1[, prob := prediccion]
      setorder(tbl1, -prob)
      tbl1[, gan_acum := cumsum(gan)]
      tbl1[, gan_suavizada := frollmean(
        x = gan_acum, n = envg$PARAM$train$meseta,
        align = "center", na.rm = TRUE, hasNA = TRUE
      )]


      ganancia_test_normalizada <- tbl1[, max(gan_suavizada, na.rm = TRUE)]
      cantidad_test_normalizada <- which.max(tbl1[, gan_suavizada])
      cant_normals <- c(cant_normals, cantidad_test_normalizada )

      # ahora para el semillerio
      tbl2 <- copy(dataset_test[, list("gan" = 
        ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$positivos, 
           envg$PARAM$train$gan1, 
           envg$PARAM$train$gan0))])

      tbl2[, prob := tb_semillerio$prob_acum]
      setorder(tbl2, -prob)
      tbl2[, gan_acum := cumsum(gan)]
      tbl2[, gan_suavizada := frollmean(
        x = gan_acum, n = envg$PARAM$train$meseta,
        align = "center", na.rm = TRUE, hasNA = TRUE
      )] 


      semi_ganancia_test_normalizada <- tbl2[, max(gan_suavizada, na.rm = TRUE)]
      semi_cantidad_test_normalizada <- which.max(tbl2[, gan_suavizada])


      rm(tbl)
      gc(verbose= FALSE)

      # logueo final
      ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
      xx <- c(ds, copy(param_completo))

      xx$early_stopping_rounds <- NULL
      xx$num_iterations <- NULL
      xx$num_iterations <- modelo_train$best_iter
      xx$estimulos <- cantidad_test_normalizada
      xx$qsemillas <- 1
      xx$semilla <- sem
      xx$iexp <- iexp
      xx$tiempo_corrida <- tiempo_local
      xx$ganancia <- ganancia_test_normalizada
      xx$metrica <- ganancia_test_normalizada
      xx$iteracion_bayesiana <- GLOBAL_iteracion
      mlog_log(xx, arch = "BO_log_detalle.txt")

      xx$estimulos <- semi_cantidad_test_normalizada
      xx$qsemillas <- isem
      xx$semilla <- -1
      xx$iexp <- iexp
      xx$tiempo_corrida <- tiempo_loop
      xx$ganancia <- semi_ganancia_test_normalizada
      xx$metrica <- semi_ganancia_test_normalizada
      mlog_log(xx, arch = "BO_log_detalle_semi.txt")
    }

    # acumulo en vector resultados de semillerios completos
    tiempo_normals <- c( tiempo_normals, tiempo_loop )
    gan_normals <- c( gan_normals, semi_ganancia_test_normalizada)
    cant_normals <- c(cant_normals, semi_cantidad_test_normalizada )
    semillas_normals <- c(semillas_normals, isem)
  }
  
  # promedio las ganancias y los envios
  tiempo_test_normalizado <- mean(tiempo_normals)
  ganancia_test_normalizada <- mean(gan_normals)
  cantidad_test_normalizada <- round(mean(cant_normals))

  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))

  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$qexp <- envg$PARAM$train$repeticiones_exp
  xx$semillerio <- round( mean(semillas_normals) )
  xx$tiempo_corrida <- tiempo_test_normalizado
  xx$ganancia <- ganancia_test_normalizada
  xx$metrica <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  superacion <- FALSE
  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))

    fwrite(tb_importancia,
      file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
      sep = "\t"
    )

    rm(tb_importancia)
    envg$OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    envg$OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$metrica <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$arboles <<- modelo_train$best_iter
    GrabarOutput()
    mlog_log(xx, arch = "BO_log_mejor.txt")

    t <- format(Sys.time(), "%Y%m%d %H%M%S")
    cat( t, "\n",
      file = "z-Rcanbypass.txt",
      append = TRUE
    )

    superacion <- TRUE
  }

  mlog_log(xx, arch = "BO_log.txt", parentreplicate= superacion)

  # retorno el vector con las dos variables que optimizo al mismo tiempo
  set.seed(envg$PARAM$seed, kind = "L'Ecuyer-CMRG")
  return( c( xx$metrica, 
             xx$tiempo_corrida
            )
        )  # tiempo al azar

}
#------------------------------------------------------------------------------
# esta es la funcion mas mistica de toda la asignatura
# sera explicada en  Laboratorio de Implementacion III

vcant_optima <- c()

fganancia_lgbm_mesetaCV <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  GLOBAL_arbol <<- GLOBAL_arbol + 1L

  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,  envg$PARAM$train$gan1, envg$PARAM$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = as.integer(envg$PARAM$train$meseta/envg$PARAM$lgb_crossvalidation_folds),
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]

  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]

  pos <- which.max(tbl[, gan_suavizada])

  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% (10 * envg$PARAM$lgb_crossvalidation_folds) == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

    cat("\r")
    cat(
      "Cross Validate ", GLOBAL_iteracion, " ", " ",
      as.integer(GLOBAL_arbol / envg$PARAM$lgb_crossvalidation_folds), "  ",
      gan * envg$PARAM$lgb_crossvalidation_folds, "   ",
      GLOBAL_gan_max * envg$PARAM$lgb_crossvalidation_folds, "   "
    )
  }

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbmCV <- function(x) {

  cat( "Inicio EstimarGanancia_lightgbmCV()\n")
  gc(verbose= FALSE)
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  envg$OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  # para que una siguiente corrida pueda retomar
  if( file.exists("bayesiana.RDATA") ) {

    cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rcanresume.txt",
      append = TRUE
     )
  }

  param_completo <- c(envg$PARAM$lgb_basicos, x)

  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)

  vcant_optima <<- c()
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_lgbm_mesetaCV,
    param = param_completo,
    stratified = TRUE, # sobre el cross validation
    nfold = envg$PARAM$lgb_crossvalidation_folds,
    verbose = -100
  )

  cat("\n")

  desde <- (modelocv$best_iter - 1) * envg$PARAM$lgb_crossvalidation_folds + 1
  hasta <- desde + envg$PARAM$lgb_crossvalidation_folds - 1

  cant_corte <- as.integer(mean(vcant_optima[desde:hasta]) *
    envg$PARAM$lgb_crossvalidation_folds)

  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]
  ganancia_normalizada <- ganancia * envg$PARAM$lgb_crossvalidation_folds


  if (ktest == TRUE) {
    # debo recrear el modelo
    param_completo$early_stopping_rounds <- NULL
    param_completo$num_iterations <- modelocv$best_iter

    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )

    # aplico el modelo a testing y calculo la ganancia
    prediccion <- predict(
      modelo,
      data.matrix(dataset_test[, campos_buenos, with = FALSE])
    )

    tbl <- copy(dataset_test[
      ,
      list("gan" = ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 
         envg$PARAM$train$gan1, envg$PARAM$train$gan0))
    ])

    tbl[, prob := prediccion]
    setorder(tbl, -prob)

    tbl[, gan_acum := cumsum(gan)]
    tbl[, gan_suavizada := frollmean(
      x = gan_acum, n = envg$PARAM$train$meseta,
      align = "center", na.rm = TRUE, hasNA = TRUE
    )]


    # Dato que hay testing, estos valores son ahora los oficiales
    ganancia_normalizada <- tbl[, max(gan_suavizada, na.rm = TRUE)]
    cant_corte <- which.max(tbl[, gan_suavizada])

    rm(tbl)
    gc(verbose= FALSE)
  }



  # voy grabando las mejores column importance
  if (ganancia_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_normalizada

    param_impo <- copy(param_completo)
    param_impo$early_stopping_rounds <- 0
    param_impo$num_iterations <- modelocv$best_iter

    modelo <- lgb.train(
      data = dtrain,
      param = param_impo,
      verbose = -100
    )

    tb_importancia <- as.data.table(lgb.importance(modelo))

    fwrite(tb_importancia,
      file = paste0("impo_", GLOBAL_iteracion, ".txt"),
      sep = "\t"
    )

    rm(tb_importancia)

    envg$OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    envg$OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$metrica <<- GLOBAL_ganancia
    envg$OUTPUT$BO$mejor$arboles <<- modelocv$best_iter
    GrabarOutput()
  }


  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))

  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelocv$best_iter
  xx$estimulos <- cant_corte
  xx$qsemillas <- 1L
  xx$ganancia <- ganancia_normalizada
  xx$metrica <- ganancia_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  mlog_log(xx, arch = "BO_log.txt")
  set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  
  cat( "Fin EstimarGanancia_lightgbmCV()\n")
  return(ganancia_normalizada)
}
#------------------------------------------------------------------------------

parametrizar  <- function( lparam )
{
  param_fijos  <- copy( lparam )
  hs  <- list()

  for( param  in  names( lparam ) )
  {
    if( length( lparam[[ param ]] ) > 1 )
    {
      desde  <- as.numeric( lparam[[ param ]][[1]]  )
      hasta  <- as.numeric( lparam[[ param ]][[2]]  )

      if( length( lparam[[ param ]] ) == 2 )
      {
         hs  <- append( hs,  
                        list( makeNumericParam( param, lower= desde, upper= hasta)  ) )
      } else {
         hs  <- append( hs, 
                        list( makeIntegerParam( param, lower= desde, upper= hasta) ) )
      }

      param_fijos[[ param ]] <- NULL  #lo quito 
    }
  }

  return( list( "param_fijos" =  param_fijos,
                "paramSet"    =  hs ) )
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2212_HT_lightgbm_SEMI.r  START\n")
action_inicializar() 

# cargo las semillas
envg$PARAM$lgb_semilla <- envg$PARAM$semilla

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla, kind = "L'Ecuyer-CMRG")
# me quedo con PARAM$semillerio  primos al azar
total_semillas <- envg$PARAM$train$repeticiones_exp * envg$PARAM$train$semillerio
envg$PARAM$semillas <- sample(primos)[1:total_semillas]



# apertura de los parametros de LightGBM
#  en los que van fijos directo al LightGBM
#  y los que pasan a formar parte de la Bayesian Optimization
apertura  <- parametrizar( envg$PARAM$lgb_param )
envg$PARAM$lgb_basicos <- apertura$param_fijos
envg$PARAM$bo_lgb <- makeParamSet( params= apertura$paramSet )

envg$PARAM$lgb_basicos$seed <- envg$PARAM$semilla

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset_training.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

dataset[, azar := NULL]

# Verificaciones
if (!("fold_train" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_train \n")
}

if (!("fold_validate" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_validate \n")
}

if (!("fold_test" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_test  \n")
}

if (dataset[fold_train == 1, .N] == 0) {
  stop("Error, en el dataset no hay registros con fold_train==1 \n")
}


GrabarOutput()

cat(envg$PARAM$exp_input,
  file = "TrainingStrategy.txt",
  append = FALSE
)

# defino la clase binaria clase01
cat( "creacion clase01\n")
dataset[, clase01 :=  0L ]
dataset[ get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, clase01 := 1L ]


# los campos que se pueden utilizar para la prediccion
cat( "creacion campos_buenos\n")
campos_buenos <- setdiff(
  copy(colnames(dataset)),
  c("clase01", envg$PARAM$dataset_metadata$clase, "fold_train", "fold_validate", "fold_test")
)

# la particion de train siempre va
cat( "creacion dtrain\n")
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[fold_train == 1, campos_buenos, with = FALSE]),
  label = dataset[fold_train == 1, clase01],
  weight = dataset[
    fold_train == 1,
    ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$positivos, 1.0000001, 1.0 )
  ],
  free_raw_data = FALSE
)

envg$OUTPUT$train$ncol <- ncol(dtrain)
envg$OUTPUT$train$nrow <- nrow(dtrain)
envg$OUTPUT$train$periodos <- dataset[fold_train == 1, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

kvalidate <- FALSE
ktest <- FALSE
kcrossvalidation <- TRUE

# Si hay que hacer validacion
cat( "creacion dvalidate\n")
if (dataset[fold_train == 0 & fold_test == 0 & fold_validate == 1, .N] > 0) {
  kcrossvalidation <- FALSE
  kvalidate <- TRUE
  dvalidate <- lgb.Dataset(
    data = data.matrix(dataset[fold_validate == 1, campos_buenos, with = FALSE]),
    label = dataset[fold_validate == 1, clase01],
    weight = dataset[
      fold_validate == 1,
      ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$positivos, 1.0000001, 1.0 )
    ],
    free_raw_data = FALSE
  )

  envg$OUTPUT$validate$ncol <- ncol(dvalidate)
  envg$OUTPUT$validate$nrow <- nrow(dvalidate)

  envg$OUTPUT$validate$periodos <- dataset[
    fold_validate == 1,
    length(unique(get(envg$PARAM$dataset_metadata$periodo)))
  ]
}


# Si hay que hacer testing
if (dataset[fold_train == 0 & fold_validate == 0 & fold_test == 1, .N] > 0) {
  cat( "creacion testing\n")
  ktest <- TRUE
  campos_buenos_test <- setdiff(
    copy(colnames(dataset)),
    c("fold_train", "fold_validate", "fold_test")
  )

  dataset_test <- dataset[fold_test == 1, campos_buenos_test, with = FALSE]

  envg$OUTPUT$test$ncol <- ncol(dataset_test)
  envg$OUTPUT$test$nrow <- nrow(dataset_test)
  envg$OUTPUT$test$periodos <- dataset_test[, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

}

# Inicializo MLFlow
mlog_init()

if (kcrossvalidation) {
  datahash <- mlog_table_hash( dataset[, envg$PARAM$dataset_metadata$primarykey, with=FALSE ] )
  mlflow_exp_det <- paste0( "/xval-", datahash )
} else {

  datahash <- mlog_table_hash( dataset_test[, envg$PARAM$dataset_metadata$primarykey, with=FALSE ] )
  mlflow_exp_det <- paste0( "/test-", datahash )
}


mlog_addfile("BO_log.txt",
             mlflow_exp= mlflow_exp_det,
             mlflow_run= envg$PARAM$experimento,
             cols_fijas= list(expw=envg$PARAM$experimento_largo) )


rm(dataset)
gc(verbose= FALSE)


# si ya existe el archivo log, traigo hasta donde procese
if (file.exists("BO_log.txt")) {
  tabla_log <- fread("BO_log.txt")
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_ganancia <- tabla_log[, max(ganancia)]
  rm(tabla_log)
} else {
  GLOBAL_iteracion <- 0L
  GLOBAL_ganancia <- -Inf
}


# Aqui comienza la configuracion de mlrMBO


envg$OUTPUT$crossvalidation <- kcrossvalidation
GrabarOutput()

# deobo hacer cross validation o  Train/Validate/Test
if (kcrossvalidation) {
  funcion_optimizar <- EstimarGanancia_lightgbmCV
} else {
  funcion_optimizar <- EstimarGanancia_lightgbm
}


configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeMultiObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = c( FALSE, TRUE ), # estoy Maximizando la ganancia y Minimizando el tiempo
  noisy = FALSE,
  par.set = envg$PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE, # paso los parametros en una lista
  n.objectives = 2L
)

# archivo donde se graba y cada cuantos segundos
ctrl <- makeMBOControl(
  save.on.disk.at.time = 60,
  save.file.path = "bayesiana.RDATA",
  n.objectives = 2L,
  y.name = c("ganancia", "tiempo")
)

ctrl <- setMBOControlTermination(
  ctrl,
  iters = envg$PARAM$bo_iteraciones
) # cantidad de iteraciones

ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
ctrl <- setMBOControlInfill(ctrl, filter.proposed.points = TRUE)

ctrl <- setMBOControlMultiObj(
    ctrl,
    method = "parego",
    parego.s = 100000L,
    parego.rho = 0.05,
    parego.sample.more.weights = 1,
    parego.normalize = front
)

# establezco la funcion que busca el maximo
surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  optim.method = "BFGS",
  nugget.estim = TRUE,
  jitter = TRUE,
  control = list(trace = TRUE)
)


# Aqui inicio la optimizacion bayesiana
set.seed(envg$PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
if (!file.exists("bayesiana.RDATA")) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  # si ya existe el archivo RDATA,
  # debo continuar desde el punto hasta donde llegue
  #  usado para cuando se corta la virtual machine
  run <- mboContinue("bayesiana.RDATA") # retomo en caso que ya exista
}

#------------------------------------------------------------------------------
BO_log <- fread("BO_log.txt")
envg$OUTPUT$ganancia_max <- BO_log[, max(ganancia, na.rm = TRUE)]

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()
#------------------------------------------------------------------------------

# ya no tiene sentido retomar, se termino el trabajo
file.remove("z-Rcanresume.txt")
#------------------------------------------------------------------------------

# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("BO_log.txt")) 
cat( "ETAPA  z2212_HT_lightgbm_SEMI.r  END\n")