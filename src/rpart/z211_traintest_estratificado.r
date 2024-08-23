rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

PARAM <- list()
PARAM$semilla <- 102191
PARAM$training_pct <- 70L  # entre  1L y 99L 

PARAM$rpart <- list (
  "cp" = -1, # complejidad minima
  "minsplit" = 700, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 350, # minima cantidad de regs en una hoja
  "maxdepth" = 8 # profundidad máxima del arbol
)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa

# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# Establezco el Working Directory, elija una carpeta de su 
setwd("~/buckets/b1/")

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

# trabajo, por ahora, solo con 202104
dataset <- dataset[foto_mes==202104]

# particiono estratificadamente el dataset 70%, 30%
particionar(dataset,
  division = c(PARAM$training_pct, 100L -PARAM$training_pct), 
  agrupa = "clase_ternaria",
  seed = PARAM$semilla # aqui se usa SU semilla
)


# genero el modelo
# quiero predecir clase_ternaria a partir del resto
# fold==1  es training,  el 70% de los datos
modelo <- rpart("clase_ternaria ~ .",
  data = dataset[fold == 1],
  xval = 0,
  control = PARAM$rpart # aqui van los parametros
)


# aplico el modelo a los datos de testing
prediccion <- predict(modelo, # el modelo que genere recien
  dataset[fold == 2], # fold==2  es testing, el 30% de los datos
  type = "prob"
) # type= "prob"  es que devuelva la probabilidad

# prediccion es una matriz con TRES columnas,
#  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego una columna que es la de las ganancias
dataset[, ganancia := ifelse(clase_ternaria == "BAJA+2", 273000, -7000)]

# para testing agrego la probabilidad
dataset[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]

# calculo la ganancia en testing  qu es fold==2
ganancia_test <- dataset[fold == 2 & prob_baja2 > 0.025, sum(ganancia)]

# escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )

estimulos <- dataset[fold == 2 & prob_baja2 > 0.025, .N]
aciertos <- dataset[fold == 2 & prob_baja2 > 0.025 & clase_ternaria == "BAJA+2", .N]


cat("Testing total: ", dataset[fold == 2, .N], "\n")
cat("Testing BAJA+2: ", dataset[fold == 2 & clase_ternaria == "BAJA+2", .N], "\n")

cat("Estimulos: ", estimulos, "\n")
cat("Aciertos (BAJA+2): ", aciertos, "\n")

cat("Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n")
