# script compañero del  z744_lightgbm_binaria_BO.r

# para correr el Google Cloud
#   8 vCPU
#  32 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("primes")
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
PARAM$experimento_data <- "PP7430"
PARAM$experimento_bayesiana <- "HT7440"

PARAM$experimento <- "KA7450"

PARAM$semilla_azar <- 102191 # Aqui poner su  primer  semilla
PARAM$semillas_cantidad <- 3

# c(1,2) son el mejor y el segundo mejor de la bayesian optimization
PARAM$bo_ranks <- c(1, 2 )

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
# Final Model -----------------------------------------------------------------
# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )

# Aqui empieza el programa
setwd("~/buckets/b1/exp/")

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_azar)
# me quedo con PARAM$semillerio  primos al azar
ksemillas <- sample(primos)[seq(PARAM$semillas_cantidad)]

# cargo el resultado de la Bayesian Optimization
tb_BO_log <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))

# En un mundo prolijo, estas variables se eliminan
#  durante la creacion del dataset
# https://www.youtube.com/watch?v=eitDnP0_83k
dataset[, cprestamos_personales := NULL ]
dataset[, cprestamos_personales_lag1 := NULL ]
dataset[, cprestamos_personales_delta1 := NULL ]

dataset[, mprestamos_personales := NULL ]
dataset[, mprestamos_personales_lag1 := NULL ]
dataset[, mprestamos_personales_delta1 := NULL ]

dataset[, cplazo_fijo := NULL ]
dataset[, cplazo_fijo_lag1 := NULL ]
dataset[, cplazo_fijo_delta1 := NULL ]

dataset[, ctarjeta_debito := NULL ]
dataset[, ctarjeta_debito_lag1 := NULL ]
dataset[, ctarjeta_debito_delta1 := NULL ]


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

# dataset donde voy a aplicar el modelo
dfuture <- dataset[part_future==1L, ]

#--------------------------------------

# me quedo con los mejores hiperparametros de la Bayesian Optimization
setorder( tb_BO_log, -ganancia )

for( vrank in PARAM$bo_ranks ){

  cat( "rank=", vrank, "  ")
  param_completo <- copy(as.list(tb_BO_log[ vrank ]))

  # hago la transformacion de leaf_size_log y  coverage
  if( "leaf_size_log"  %in% names(param_completo) )
  {
    # primero defino el tamaño de las hojas
    param_completo$min_data_in_leaf <- pmax( 1,  round( nrow(dfinaltrain) * ( 2.0 ^ param_completo$leaf_size_log ))  )
    # luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
    param_completo$num_leaves <- pmin( 131072, 
      pmax( 2,  round( ((2.0^param_completo$coverage_log)) * nrow( dfinaltrain ) / param_completo$min_data_in_leaf ) ) )
  }

  if( "num_iterations_log"  %in% names(param_completo) )
  {
    param_completo$num_iterations <- pmax( 2L, as.integer( round( 2.0 ^ param_completo$num_iterations_log ) ) )
  }



  for( vsemilla in ksemillas ) {

    cat( "  ", vsemilla )
    # establezo  la semilla
    param_completo$seed <- vsemilla
    set.seed(param_completo$seed, kind = "L'Ecuyer-CMRG")

    # entreno el modelo
    modelo_final <- lightgbm(
     data = dfinaltrain,
     params = param_completo,
     verbose = -100
    )

    #--------------------------------------
    # ahora imprimo la importancia de variables
    #  solo para la primer semilla
    if( vsemilla == ksemillas[1] ){
      tb_importancia <- as.data.table(lgb.importance(modelo_final))
      archivo_importancia <- paste0( "impo_", vrank ,".txt")

      fwrite(tb_importancia,
        file = archivo_importancia,
       sep = "\t"
      )

      # grabo el modelo
      lgb.save(modelo_final, paste0("modelo_",  vrank,".model"))
    }


    # Scoring ---------------------------------------------------------------------
    # aplico el modelo a los datos future

    prediccion <- predict(
      modelo_final,
      data.matrix(dfuture[, campos_buenos, with = FALSE])
    )

    # genero la tabla de entrega
    tb_entrega <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_entrega[, prob := prediccion]

    # grabo las probabilidad del modelo
    fwrite(tb_entrega,
     file = paste0("prediccion_", vrank, "_", vsemilla, ".txt"),
     sep = "\t"
    )

    # Kaggle ----------------------------------------------------------------------
    # ordeno por probabilidad descendente
    setorder(tb_entrega, -prob)


    # genero archivos para Kaggle
    cortes <- seq(8000, 13000, by = 500)
    for (envios in cortes) {
      tb_entrega[, Predicted := 0L]
      tb_entrega[1:envios, Predicted := 1L]

      nom_arch <- paste0(PARAM$experimento, "_",
        vrank, "_",
        vsemilla, "_",
        envios, ".csv"
      )

      fwrite( tb_entrega[, list(numero_de_cliente, Predicted)],
         file = nom_arch,
         sep = ","
      )
    }

  }

  cat("\n" )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
