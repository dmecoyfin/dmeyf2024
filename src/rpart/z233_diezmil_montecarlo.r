rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")
require("ggplot2")


PARAM <- list()
# reemplazar por las propias semillas
PARAM$semilla_primigenia <- 102191
PARAM$qsemillas <- 10000

# dataset
PARAM$dataset_nom <- "./datasets/competencia_01.csv"

PARAM$training_pct <- 70L  # entre  1L y 99L 


PARAM$rpart <- list (
  "cp" = -1, # complejidad minima
  "minsplit" = 700, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 350, # minima cantidad de regs en una hoja
  "maxdepth" = 8 # profundidad máxima del arbol
)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste
#  en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#  crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset,
    division = c(param_basicos$training_pct, 100L -param_basicos$training_pct), 
    agrupa = "clase_ternaria",
    seed = semilla # aqui se usa SU semilla
  )

  # genero el modelo
  # predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos$rpart
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 273000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )

  return(list(
    "semilla" = semilla,
    "testing" = dataset[fold == 2, .N],
    "testing_pos" = dataset[fold == 2 & clase_ternaria == "BAJA+2", .N],
    "envios" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025)],
    "aciertos" = dataset[
        fold == 2,
        sum(prediccion[, "BAJA+2"] > 0.025 & clase_ternaria == "BAJA+2")
    ],
    "ganancia_test" = ganancia_test_normalizada
  ))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory


# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, PARAM$qsemillas )


# cargo los datos
dataset <- fread(PARAM$dataset_nom)

# trabajo, por ahora, solo con 202104
dataset <- dataset[foto_mes==202104]

dir.create("~/buckets/b1/exp/EX2330u", showWarnings = FALSE)
setwd("~/buckets/b1/exp/EX2330u")


# la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
#  tantas veces como valores tenga el vector  PARAM$semillas
salidas <- mcmapply(ArbolEstimarGanancia,
  PARAM$semillas, # paso el vector de semillas
  MoreArgs = list(PARAM), # aqui paso el segundo parametro
  SIMPLIFY = FALSE,
  mc.cores = detectCores()
)

# muestro la lista de las salidas en testing
#  para la particion realizada con cada semilla
salidas

# paso la lista a vector
tb_salida <- rbindlist(salidas)

fwrite( tb_salida,
        "tb_salida.txt",
        sep ="\t" )



# grafico densidades
media <- tb_salida[ , mean( ganancia_test) ]
cuantiles  <-  quantile(  tb_salida[,  ganancia_test ],
   prob= c(0.05, 0.5, 0.95),
   na.rm=TRUE )

grafico <- ggplot( tb_salida, aes(x=ganancia_test)) + geom_density(alpha=0.25)  +
  geom_vline(xintercept=media, linewidth=1, color="red") +
  geom_vline(xintercept=cuantiles[1], linewidth=0.5, color="blue") +
  geom_vline(xintercept=cuantiles[2], linewidth=0.5, color="blue") +
  geom_vline(xintercept=cuantiles[3], linewidth=0.5, color="blue")

pdf("densidad.pdf")
print(grafico)
dev.off()

# desvios estandar
tb_final <- data.table(
    grupo_size=integer(),
    grupos=integer(),
    gan_media=numeric(),
    gan_sd=numeric()
)

cant <-  nrow( tb_salida )
for( q in  c(1,2,4, 8, 16, 32, 64, 128 ) )
{
  i <- 1
  grupos <- 0
  vgan <- c()
  while(  i+q-1 <= cant )
  {
    gan <- tb_salida[ i:(i+q-1) , mean( ganancia_test) ]
    vgan <-  c( vgan, gan )
    i <- i + q
    grupos <-  grupos + 1
  }
  
  tb_final <- rbindlist( list( tb_final, list( q, grupos, mean(vgan), sd(vgan) ) ) )
}

print( tb_final )
