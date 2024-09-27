# Este script esta pensado para correr en Google Cloud
#   8 vCPU
# 128 GB memoria RAM

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("ulimit")  # para controlar la memoria

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento_data <- "PP7230"
PARAM$experimento <- "HT7240"

PARAM$semilla_azar <- 102191 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000


# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$semilla_azar
)


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 4096L),
  makeIntegerParam("min_data_in_leaf", lower = 10L, upper = 50000L)
)

# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
#  si se llama J.T. dejelo en 50 para no sufrir
PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana


#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {

  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) # Escribo los titulos
    {
      linea <- paste0(
        "fecha\t",
        paste(list.names(reg), collapse = "\t"), "\n"
      )

      cat(linea, file = archivo)
    }

  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )

  cat(linea, file = archivo, append = TRUE) # grabo al archivo

  if (verbose) cat(linea) # imprimo por pantalla
}
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
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
      PARAM$hyperparametertuning$POS_ganancia,
      PARAM$hyperparametertuning$NEG_ganancia  )
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]

  tbl[, gan_suavizada :=
    frollmean(
      x = gan_acum, n = 2001, align = "center",
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
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L

  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)

  param_completo$early_stopping_rounds <-
    as.integer(200 + 4 / param_completo$learning_rate)

  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )

  cat("\n")

  cant_corte <- vcant_optima[modelo_train$best_iter]

  # aplico el modelo a testing y calculo la ganancia
  prediccion <- predict(
    modelo_train,
    data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )

  tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
    PARAM$hyperparametertuning$POS_ganancia, 
    PARAM$hyperparametertuning$NEG_ganancia))])

  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]


  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]

  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])

  rm(tbl)
  gc()

  ganancia_test_normalizada <- ganancia_test


  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))

    fwrite(tb_importancia,
      file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
      sep = "\t"
    )

    rm(tb_importancia)
  }


  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))

  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  loguear(xx, arch = "BO_log.txt")

  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )

setwd("~/buckets/b1/exp/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))


# creo la carpeta donde va el experimento
dir.create(paste0(PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- "BO_log.txt"


# ahora SI comienza la optimizacion Bayesiana

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01",
    "part_training", "part_validation", "part_testing",
    "part_final_train", "part_future")
)


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[part_training == 1L, campos_buenos, with = FALSE]),
  label = dataset[part_training == 1L, clase01],
  weight = dataset[part_training == 1L, 
    ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
      ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)



dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[part_validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[part_validation == 1L, clase01],
  weight = dataset[part_validation == 1L, 
    ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
      ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)

dataset_test <- dataset[part_testing == 1]

# libero espacio
rm(dataset)
gc()

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = FALSE,  # para que no se impaciente Joaquin Tschopp
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$bo_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())


# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")
