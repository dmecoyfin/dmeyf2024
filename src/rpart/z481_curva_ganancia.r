# Grafico de la ganancia que visualiza el overfitting
# La idea es probar con distintos hiperparametros del arbol de decision
# y ver como se acercan o separan las curvas de ganancia
# MUY importante :  notar que Training = 50%  y  Testing = 50%

# Notar que la curva en training es siempre convexa
# mientras que la de testing puede tener concavidades

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")
require("ggplot2")


# cambiar aqui los parametros
PARAM <- list()

PARAM$dataset <- "./datasets/competencia_01.csv"

PARAM$semilla_primigenia <- 102191

PARAM$minsplit <- 300
PARAM$minbucket <- 20
PARAM$maxdepth <- 11

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
       if (!is.na(seed)) set.seed(seed)

       bloque <- unlist(mapply(
              function(x, y) {
                     rep(y, x)
              },
              division, seq(from = start, length.out = length(division))
       ))

       data[, (campo) := sample(rep(
              bloque,
              ceiling(.N / length(bloque))
       ))[1:.N],
       by = agrupa
       ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory


# cargo los datos
dataset <- fread( PARAM$dataset )


# a partir de ahora solo trabajo con 202104
#  el ultimo mes que tiene clase
dataset <- dataset[foto_mes == 202104] # defino donde voy a entrenar


# La division training/testing es 50%, 50%
#  que sea 50/50 se indica con el c(1,1)
particionar(dataset,
  division = c(1, 1),
  agrupa = "clase_ternaria",
  seed = PARAM$semilla_primigenia
)

# Entreno el modelo
# los datos donde voy a entrenar
# aqui es donde se deben probar distintos hiperparametros
modelo <- rpart(
       formula = "clase_ternaria ~ . -fold",
       data = dataset[fold == 1, ],
       xval = 0,
       cp = -1,
       minsplit = PARAM$minsplit,
       minbucket = PARAM$minbucket,
       maxdepth = PARAM$maxdepth
)


# aplico el modelo a TODOS los datos, inclusive los de training
prediccion <- predict(modelo, dataset, type = "prob")

# Pego la probabilidad de  BAJA+2
dataset[, prob_baja2 := prediccion[, "BAJA+2"]]


# Dibujo la curva de ganancia acumulada
setorder(dataset, fold, -prob_baja2)

# agrego una columna que es la de las ganancias
# la multiplico por 2 para que ya este normalizada
#  es 2 porque cada fold es el 50%
dataset[, gan := 2 *ifelse(clase_ternaria == "BAJA+2", 273000, -7000)]
dataset[, ganancia_acumulada := cumsum(gan), by = fold]
dataset[, pos := sequence(.N), by = fold]


# Esta hermosa curva muestra como en el mentiroso training
#   la ganancia es siempre mejor que en el real testing


# defino hasta donde muestra el grafico
amostrar <- 20000


gra <- ggplot(
           data = dataset[pos <= amostrar],
           aes( x = pos, y = ganancia_acumulada,
                color = ifelse(fold == 1, "train", "test") )
             ) + geom_line()

print( gra )

cat( "Train gan max: ", dataset[fold==1, max(ganancia_acumulada)], "\n" )
cat( "Test  gan max: ", dataset[fold==2, max(ganancia_acumulada)], "\n" )

