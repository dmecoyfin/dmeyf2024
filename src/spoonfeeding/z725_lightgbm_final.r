# para correr el Google Cloud
#   8 vCPU
#  32 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("ulimit")  # para controlar la memoria


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
PARAM$experimento_bayesiana <- "HT7240"

PARAM$experimento <- "KA7250"


#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {

  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )

# Aqui empieza el programa
setwd("~/buckets/b1/exp/")

# cargo el resultado de la Bayesian Optimization
tb_BO_log <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))


# creo la carpeta donde va el experimento
dir.create(paste0(PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./", PARAM$experimento, "/"))



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
dfinaltrain <- lgb.Dataset(
  data = data.matrix(dataset[part_final_train == 1L, campos_buenos, with = FALSE]),
  label = dataset[part_final_train == 1L, clase01],
  free_raw_data = FALSE
)

#--------------------------------------

# me quedo con los mejores hiperparametros de la Bayesian Optimization
setorder( tb_BO_log, -ganancia )
param_completo <- copy(as.list(tb_BO_log[1]))

set.seed(param_completo$seed, kind = "L'Ecuyer-CMRG")

# entreno el modelo
modelo <- lightgbm(
  data = dfinaltrain,
  params = param_completo,
  verbose = -100
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos future
dfuture <- dataset[part_future==1L]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dfuture[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 13000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
