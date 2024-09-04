# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol utilizando un subset distinto de atributos del dataset

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("yaml")

# parametros experimento
PARAM <- list()
PARAM$experimento <- "KA4610"

# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 102191

# parametros rpart

#  cargue aqui los hiperparametros elegidos
PARAM$rpart <- data.table( 
  "cp" = -1,
  "minsplit" = 50,
  "minbucket" = 20,
  "maxdepth" = 6
)

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
#  por ahora, es fijo
PARAM$feature_fraction <- 0.5


# voy a generar 128 arboles,
#  a mas arboles mas tiempo de proceso y MEJOR MODELO,
#  pero ganancias marginales
PARAM$num_trees_max <- 128
PARAM$grabar <- c(1, 2, 4, 8, 16, 32, 64, 128)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory


# cargo los datos
dataset <- fread("./datasets/competencia_01.csv")

# defino los dataset de entrenamiento y aplicacion
dtrain <- dataset[foto_mes == 202104]
dapply <- dataset[foto_mes == 202106]

# arreglo clase_ternaria por algun distraido ""
dapply[, clase_ternaria := NA ]

# elimino lo que ya no utilizo
rm(dataset)
gc()


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/", PARAM$experimento, "/")
dir.create(paste0("./exp/", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)



# Establezco cuales son los campos que puedo usar para la prediccion
# el copy() es por la Lazy Evaluation
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



# Genero las salidas
for( icorrida in seq(nrow(PARAM$rpart)) ){

  cat( "Corrida ", icorrida, " ; " )

  # aqui se va acumulando la probabilidad del ensemble
  dapply[, prob_acumulada := 0]

  # los parametros que voy a utilizar para rpart
  param_rpart <- PARAM$rpart[ icorrida ]

  set.seed(PARAM$semilla_primigenia) # Establezco la semilla aleatoria

  for (arbolito in seq(PARAM$num_trees_max) ) {
    qty_campos_a_utilizar <- as.integer(length(campos_buenos)
       * PARAM$feature_fraction)

    campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

    # paso de un vector a un string con los elementos
    # separados por un signo de "+"
    # este hace falta para la formula
    campos_random <- paste(campos_random, collapse = " + ")

    # armo la formula para rpart
    formulita <- paste0("clase_ternaria ~ ", campos_random)

    # genero el arbol de decision
    modelo <- rpart(formulita,
      data = dtrain,
      xval = 0,
      control = param_rpart
    )

    # aplico el modelo a los datos que no tienen clase
    prediccion <- predict(modelo, dapply, type = "prob")

    dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

    if (arbolito %in% PARAM$grabar) {

      # Genero la entrega para Kaggle
      umbral_corte <- (1 / 40) * arbolito
      entrega <- as.data.table(list(
        "numero_de_cliente" = dapply[, numero_de_cliente],
        "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
      )) # genero la salida

      nom_arch_kaggle <- paste0(
        PARAM$experimento, "_",
        icorrida, "_",
        sprintf("%.3d", arbolito), # para que tenga ceros adelante
        ".csv"
      )

      # grabo el archivo 
      fwrite(entrega,
        file = nom_arch_kaggle,
        sep = ","
      )

    }

    cat(arbolito, " ")
  }
}

