# Script prueba de submit a las dos competencias Kaggle 

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")

#------------------------------------------------------------------------------

getandincrement <- function( nom_archivo )
{
  contador <- read_yaml(nom_archivo)
  valor <- contador$contador
  contador$contador <- contador$contador + 1L
  write_yaml( contador, file=nom_archivo )
  return( valor )
}
#------------------------------------------------------------------------------

generarmodelo <- function( idcompetencia, kcompetencia, param )
{
  # cargo el dataset pequeno
  dataset <- fread("~/datasets/competencia_01_crudo.csv" )


  # calculo el periodo0 consecutivo
  dsimple <- dataset[, list(
      "pos" = .I,
      numero_de_cliente,
      periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 ) ]

  # ordeno
  setorder( dsimple, numero_de_cliente, periodo0 )

  # calculo topes
  periodo_ultimo <- dsimple[, max(periodo0) ]
  periodo_anteultimo <- periodo_ultimo - 1


  # calculo los leads de orden 1 y 2
  dsimple[, c("periodo1", "periodo2") :=
     shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

  # assign most common class values = "CONTINUA"
  dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

  # calculo BAJA+1
  dsimple[ periodo0 < periodo_ultimo &
    ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
    clase_ternaria := "BAJA+1" ]

  # calculo BAJA+2
  dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
    & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
    clase_ternaria := "BAJA+2" ]


  # pego el resultado en el dataset original y grabo
  setorder( dsimple, pos )
  dataset[, clase_ternaria := dsimple$clase_ternaria ]

  dtrain <- dataset[foto_mes == 202104] # defino donde voy a entrenar
  dapply <- dataset[foto_mes == 202106] # defino donde voy a aplicar el modelo

  # genero el modelo,  aqui se construye el arbol
  # quiero predecir clase_ternaria a partir de el resto de las variables
  modelo <- rpart(
      formula = "clase_ternaria ~ .",
      data = dtrain, # los datos donde voy a entrenar
      xval = 0,
      control = param,
  )

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
      object = modelo,
      newdata = dapply,
      type = "prob"
  )

  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

  # archivo de salida
  contador <- getandincrement("contador.yml")
  archivo_submit <- paste0( "K100_",
     sprintf("%.3d", contador),
     ".csv"
  )

  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = archivo_submit,
         sep = ","
  )

  # preparo todo para el submit
  comentario <- paste0( "'",
      "cp=-1",
      " minsplit=", param$minsplit,
      " minbucket=", param$minbucket,
      " maxdepth=", param$maxdepth,
      "'"
  )

  comando <- paste0( "~/install/proc_kaggle_submit.sh ",
      "TRUE ",
	  idcompetencia, " ",
      kcompetencia, " ",
      archivo_submit, " ",
      comentario
  )

  ganancia <- system( comando, intern=TRUE )
  cat( paste0( ganancia, "\t", archivo_submit, "\n"),
      file="tb_ganancias.txt",
      append=TRUE
  )

  return(  as.numeric(ganancia) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# aqui empieza el programa

# creo la carpeta donde voy a trabajar
dir.create("~/buckets/b1/exp/KA2000", showWarnings = FALSE)
setwd("~/buckets/b1/exp/KA2000")

# creo el contador
if( !file.exists( "contador.yml" ) )
{
  contador <- list( "contador" = 1L )
  write_yaml( contador, file="contador.yml" )
}

# genero al azar maxdepth, minsplit y minbucket
param_vivencial <- list()
set.seed( Sys.time() )

# modelo vivencial
param_vivencial$cp <- -1
param_vivencial$maxdepth <- sample( 4:10, 1 )
param_vivencial$minsplit <- sample( 50:500, 1 )
param_vivencial$minbucket <- sample( 1:(param_vivencial$minsplit/2), 1 )
gan_vivencial <- generarmodelo( 1, "dm-ey-f-2024-primera", param_vivencial )

quit( save="no" )
