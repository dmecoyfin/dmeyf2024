# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")

require("ranger")
require("randomForest") # solo se usa para imputar nulos


PARAM <- list()
PARAM$experimento <- "KA4720"

# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 102191


# hiperparámetros de Random Forest
PARAM$ranger <- list(
  "num.trees" = 300, # cantidad de arboles
  "mtry" = 13, # cantidad de atributos que participan en cada split
  "min.node.size" = 50, # tamaño minimo de las hojas
  "max.depth" = 10 # 0 significa profundidad infinita
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory


# cargo los datos
dataset <- fread("./datasets/competencia_01.csv")


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/", PARAM$experimento, "/")
dir.create(paste0("./exp/", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


# asigno un valor muy negativo
#  estas dos lineas estan relacionadas con el Data Drifting
if( "Master_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Master_Finiciomora) , Master_Finiciomora := -999 ]

if( "Visa_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Visa_Finiciomora) , Visa_Finiciomora :=  -999 ]

# defino donde entreno y donde aplico el modelo
dtrain <- dataset[foto_mes == 202104]
dapply <- dataset[foto_mes == 202106]



set.seed( PARAM$semilla_primigenia ) # Establezco la semilla aleatoria


# ranger necesita la clase de tipo factor
factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dtrain <- na.roughfix(dtrain)

setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA

# genero el modelo de Random Forest llamando a ranger()
modelo <- ranger(
  formula = "clase_ternaria ~ .",
  data = dtrain,
  probability = TRUE, # para que devuelva las probabilidades
  num.trees = PARAM$ranger$num.trees,
  mtry = PARAM$ranger$mtry,
  min.node.size = PARAM$ranger$min.node.size,
  max.depth = PARAM$ranger$max.depth
)


# Carpinteria necesaria sobre  dapply
# como quiere la Estadistica Clasica, imputar nulos por separado
# ( aunque en este caso ya tengo los datos del futuro de anteman
#  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)


# aplico el modelo recien creado a los datos del futuro
prediccion <- predict(modelo, dapply)

# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
)) # genero la salida



nom_arch_kaggle <- "KA4720_001.csv"

# genero el archivo para Kaggle
fwrite(entrega,
  file = nom_arch_kaggle,
  sep = ","
)

