# Este script esta pensado para correr en Google Cloud

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,
# con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation el cual es muuuy lento
# la probabilidad de corte es un hiperparametro

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("primes")

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

PARAM$semilla_primigenia <- 102191
PARAM$experimento <- "HT4220"

PARAM$input$dataset <- "./datasets/competencia_01.csv"
PARAM$input$training <- c(202104) # los meses en los que vamos a entrenar

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
# undersampling de 1.0  implica tomar TODOS los datos
PARAM$trainingstrategy$undersampling <- 1.0

PARAM$hyperparametertuning$iteraciones <- 150
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

#------------------------------------------------------------------------------

# Aqui se cargan los bordes de los hiperparametros
hs <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 8000L),
  makeIntegerParam("envios", lower = 5000L, upper = 15000L)
)

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
# esta funcion calcula internamente la ganancia de la prediccion probs
# es llamada por lightgbm luego de construir cada  arbolito

fganancia_logistic_lightgbm <- function(probs, datos) {
  vpesos <- get_field(datos, "weight")

  # vector de ganancias
  vgan <- ifelse(vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia,
    ifelse(vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia,
      PARAM$hyperparametertuning$NEG_ganancia /
        PARAM$trainingstrategy$undersampling
    )
  )

  tbl <- as.data.table(list("vprobs" = probs, "vgan" = vgan))
  setorder(tbl, -vprobs)
  ganancia <- tbl[1:GLOBAL_envios, sum(vgan)]

  return(list(
    "name" = "ganancia",
    "value" = ganancia,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales,
# la semilla del mal ...


EstimarGanancia_lightgbm <- function(x) {
  gc() # libero memoria

  # llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # para usar en fganancia_logistic_lightgbm
  # asigno la variable global
  GLOBAL_envios <<- as.integer(x$envios / PARAM$hyperparametertuning$xval_folds)

  # cantidad de folds para cross validation
  kfolds <- PARAM$hyperparametertuning$xval_folds

  param_basicos <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # valor grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para evitar warning
    seed = ksemilla_azar1
  )

  # el parametro discolo, que depende de otro
  param_variable <- list(
    early_stopping_rounds =
      as.integer(50 + 5 / x$learning_rate)
  )

  param_completo <- c(param_basicos, param_variable, x)

  set.seed(ksemilla_azar1)
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_logistic_lightgbm,
    stratified = TRUE, # sobre el cross validation
    nfold = kfolds, # folds del cross validation
    param = param_completo,
    verbose = -100
  )

  # obtengo la ganancia
  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]

  ganancia_normalizada <- ganancia * kfolds # normailizo la ganancia

  # asigno el mejor num_iterations
  param_completo$num_iterations <- modelocv$best_iter
  # elimino de la lista el componente
  param_completo["early_stopping_rounds"] <- NULL


  # el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  # esta es la forma de devolver un parametro extra
  attr(ganancia_normalizada, "extras") <-
    list("num_iterations" = modelocv$best_iter)

  # logueo
  xx <- param_completo
  xx$ganancia <- ganancia_normalizada # le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)

  # Voy registrando la importancia de variables
  if (ganancia_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_normalizada
    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )

    tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- paste0("impo_", GLOBAL_iteracion, ".txt")
    fwrite(tb_importancia,
      file = archivo_importancia,
      sep = "\t" )

    loguear(xx, arch = klog_mejor)
  }

  return(ganancia_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory


# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, 2 )
ksemilla_azar1 <- PARAM$semillas[1]
ksemilla_azar2 <- PARAM$semillas[2]

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")
klog_mejor <- paste0(PARAM$experimento, "_mejor.txt")

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[
  foto_mes %in% PARAM$input$training,
  clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)
]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
# notar que para esto utilizo la SEGUNDA semila
set.seed(ksemilla_azar2)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, ifelse(clase_ternaria == "BAJA+2", 1.0000002, ifelse(clase_ternaria == "BAJA+1", 1.0000001, 1.0))],
  free_raw_data = FALSE
)



# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs, # definido al comienzo del programa
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
  iters = PARAM$hyperparametertuning$iteraciones
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
